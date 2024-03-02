use crate::{ast::*, Compile, CompilerState};
use general_assembly::operation::Operation;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::Ident;

use self::intrinsic::{ConditionalJump, LocalAddress, SetNFlag, SetZFlag, SignExtend, ZeroExtend};

impl Compile for Function {
    type Output = TokenStream;
    fn compile(&self, state: &mut CompilerState<Self::Output>) -> Self::Output {
        match self {
            // This should not be managed by us
            Self::Ident(i, args) => quote! {#i(#(#args),*)},
            Self::Intrinsic(i) => i.compile(state),
        }
    }
}
impl Compile for (Ident, RustSyntax) {
    type Output = TokenStream;
    fn compile(&self, state: &mut CompilerState<Self::Output>) -> Self::Output {
        match self.1.clone() {
            RustSyntax::If(e, happy_case, Some(sad_case)) => {
                let to_declare_global: Vec<Ident> = state.to_declare.drain(..).collect();
                let declaration_strings_global = to_declare_global.iter().map(|el| el.to_string());

                let happy_case = (self.0.clone(), *happy_case).compile(state);
                let to_declare_happy: Vec<Ident> = state.to_declare.drain(..).collect();
                let declaration_strings_happy = to_declare_happy.iter().map(|el| el.to_string());

                let sad_case = (self.0.clone(), *sad_case).compile(state);
                let to_declare_sad: Vec<Ident> = state.to_declare.drain(..).collect();
                let declaration_strings_sad = to_declare_sad.iter().map(|el| el.to_string());
                quote!(
                    #(let #to_declare_global = Operand::Local(#declaration_strings_global.to_owned());)*
                    if #e {
                        #(let #to_declare_happy = Operand::Local(#declaration_strings_happy.to_owned());)*
                        #happy_case;
                    } else {
                        #(let #to_declare_sad = Operand::Local(#declaration_strings_sad.to_owned());)*
                        #sad_case;
                    }
                )
            }
            RustSyntax::If(e, happy_case, None) => {
                let to_declare_global: Vec<Ident> = state.to_declare.drain(..).collect();
                let declaration_strings_global = to_declare_global.iter().map(|el| el.to_string());

                let happy_case = (self.0.clone(), *happy_case).compile(state);
                let to_declare_happy: Vec<Ident> = state.to_declare.drain(..).collect();
                let declaration_strings_happy = to_declare_happy.iter().map(|el| el.to_string());
                quote!(
                    #(let #to_declare_global = Operand::Local(#declaration_strings_global.to_owned());)*
                    if #e {
                        #(let #to_declare_happy = Operand::Local(#declaration_strings_happy.to_owned());)*
                        #happy_case;
                    }
                )
            }
            RustSyntax::For(i, e, block) => {
                let to_declare_global: Vec<Ident> = state.to_declare.drain(..).collect();
                let declaration_strings_global = to_declare_global.iter().map(|el| el.to_string());
                let block = (self.0.clone(), *block).compile(state);
                let to_declare_inner: Vec<Ident> = state.to_declare.drain(..).collect();
                let declaration_strings_inner = to_declare_inner.iter().map(|el| el.to_string());
                quote!(
                    #(let #to_declare_global = Operand::Local(#declaration_strings_global.to_owned());)*
                    for #i in #e {
                        #(let #to_declare_inner = Operand::Local(#declaration_strings_inner.to_owned());)*
                        #block
                    }
                )
            }
            RustSyntax::Exprs(extensions) => {
                let mut ext = Vec::new();
                for el in extensions {
                    ext.push(el.compile(state));
                }
                let ret = self.0.clone();
                let declerations: Vec<Ident> = state.to_declare.drain(..).collect();
                let to_insert_above: Vec<TokenStream> = state.to_insert_above.drain(..).collect();
                let declaration_strings = declerations.iter().map(|el| el.to_string());
                quote!(
                #(let #declerations = Operand::Local(#declaration_strings.to_owned());)*
                #ret.extend([
                    #(#to_insert_above,)*
                    #(#ext,)*
                ]);
                )
            }
            RustSyntax::RustExpr(_) => todo!()
        }
    }
}

impl Compile for Intrinsic {
    type Output = TokenStream;
    fn compile(&self, state: &mut CompilerState<Self::Output>) -> Self::Output {
        match self {
            Self::ZeroExtend(z) => z.compile(state),
            Self::SignExtend(s) => s.compile(state),
            Self::SetNFlag(n) => n.compile(state),
            Self::SetZFlag(z) => z.compile(state),
            Self::ConditionalJump(j) => j.compile(state),
            Self::LocalAddress(a) => a.compile(state),
        }
    }
}
impl Compile for LocalAddress {
    type Output = TokenStream;
    fn compile(&self, state: &mut CompilerState<Self::Output>) -> Self::Output {
        let name = self.name.clone();
        let bits = self.bits.clone();
        quote!(Operand::AddressInLocal(#name.to_owned(),#bits))
    }
}
impl Compile for ConditionalJump {
    type Output = TokenStream;
    fn compile(&self, state: &mut CompilerState<Self::Output>) -> Self::Output {
        let operand = self.operand.compile(state);
        let condition = self.condition.clone();

        quote!(Operation::ConditionalJump { destination: #operand,condition:#condition.clone() })
    }
}
impl Compile for SetNFlag {
    type Output = TokenStream;
    fn compile(&self, state: &mut CompilerState<Self::Output>) -> Self::Output {
        let operand = self.operand.compile(state);
        quote!(Operation::SetNFlag { operand: #operand })
    }
}
impl Compile for SetZFlag {
    type Output = TokenStream;
    fn compile(&self, state: &mut CompilerState<Self::Output>) -> Self::Output {
        let operand = self.operand.compile(state);
        quote!(Operation::SetZFlag { operand: #operand })
    }
}
impl Compile for SignExtend {
    type Output = TokenStream;
    fn compile(&self, state: &mut CompilerState<Self::Output>) -> Self::Output {
        let intermediate = state.intermediate();
        let operand = self.operand.compile(state);
        let bits = self.bits.clone();
        state.to_insert_above.push(quote!(Operation::SignExtend { destination: #intermediate.clone(), operand: #operand, bits: #bits.clone() }));
        quote!(#intermediate)
    }
}
impl Compile for ZeroExtend {
    type Output = TokenStream;
    fn compile(&self, state: &mut CompilerState<Self::Output>) -> Self::Output {
        let intermediate = state.intermediate();
        let operand = self.operand.compile(state);
        let bits = self.bits.clone();
        state.to_insert_above.push(quote!(Operation::ZeroExtend { destination: #intermediate.clone(), operand: #operand, bits: #bits.clone() }));
        quote!(#intermediate)
    }
}

impl Compile for Operand {
    type Output = TokenStream;
    fn compile(&self, state: &mut crate::CompilerState<Self::Output>) -> Self::Output {
        match self {
            Self::Expr(e) => e.compile(state),
            Self::Ident(i) => i.compile(state),
            Self::FunctionCall(f) => (*f).compile(state),
        }
    }
}
impl Compile for FunctionCall {
    type Output = TokenStream;
    fn compile(&self, state: &mut crate::CompilerState<Self::Output>) -> Self::Output {
        let f: TokenStream = self.ident.clone().compile(state);
        let args = self.args.clone();
        quote! {
            #f(#(#args),*)
        }
    }
}
impl Compile for ExprOperand {
    type Output = TokenStream;
    fn compile(&self, state: &mut crate::CompilerState<Self::Output>) -> Self::Output {
        match self {
            Self::Paren(p) => quote!((#p)),
            Self::Chain(i, it) => {
                let ident: TokenStream = (*i).compile(state);
                let ops: Vec<TokenStream> = it
                    .into_iter()
                    .map(|(ident, args)| {
                        let args = args
                            .into_iter()
                            .map(|el| (*el).compile(state))
                            .collect::<Vec<TokenStream>>();
                        quote!(#ident(#(#args),*))
                    })
                    .collect();
                quote!(#ident.#(#ops).*)
            }
            Self::Ident(i) => quote!(#i.clone()),
            Self::Literal(l) => quote!(#l),
            Self::FunctionCall(f) => f.compile(state),
        }
    }
}
impl Compile for IdentOperand {
    type Output = TokenStream;
    fn compile(&self, state: &mut crate::CompilerState<Self::Output>) -> Self::Output {
        match self.define {
            true => state.to_declare.push(self.ident.clone()),
            false => {}
        };
        let ident = self.ident.clone();
        quote!(#ident.clone())
    }
}
impl Compile for Assign {
    type Output = TokenStream;
    fn compile(&self, state: &mut crate::CompilerState<Self::Output>) -> Self::Output {
        let dst: TokenStream = self.dest.compile(state);
        let rhs: TokenStream = self.rhs.compile(state);
        let to_insert = state.to_insert_above.drain(..);
        quote! {
            #(#to_insert,)*
            Operation::Move { destination: #dst, source: #rhs }
        }
        .into()
    }
}
impl Compile for UnOp {
    type Output = TokenStream;
    fn compile(&self, state: &mut crate::CompilerState<Self::Output>) -> Self::Output {
        let dst: TokenStream = self.dest.compile(state);
        let rhs: TokenStream = self.rhs.compile(state);
        let ret = match self.op {
            UnaryOperation::BitwiseNot => quote!(
                Operation::Not { destination: #dst, operand: #rhs }
            ),
        };

        let to_insert = state.to_insert_above.drain(..);
        quote!(
        #(#to_insert,)*
        #ret
        )
    }
}
impl Compile for BinOp {
    type Output = TokenStream;
    fn compile(&self, state: &mut crate::CompilerState<Self::Output>) -> Self::Output {
        let dst: TokenStream = self.dest.compile(state);
        let rhs: TokenStream = self.rhs.compile(state);
        let lhs: TokenStream = self.lhs.compile(state);
        let ret = match self.op {
            BinaryOperation::Sub => quote!(
                        Operation::Sub { destination: #dst, operand1: #lhs, operand2: #rhs }
            ),
            BinaryOperation::Add => quote!(
                        Operation::Add { destination: #dst, operand1: #lhs, operand2: #rhs }
            ),
            BinaryOperation::AddWithCarry => quote!(
                        Operation::Adc { destination: #dst, operand1: #lhs, operand2: #rhs }
            ),
            BinaryOperation::Div => quote!(
                        Operation::Div { destination: #dst, operand1: #lhs, operand2: #rhs }
            ),
            BinaryOperation::Mul => quote!(
                        Operation::Mul { destination: #dst, operand1: #lhs, operand2: #rhs }
            ),
            BinaryOperation::BitwiseOr => quote!(
                        Operation::Or { destination: #dst, operand1: #lhs, operand2: #rhs }
            ),
            BinaryOperation::BitwiseAnd => quote!(
                        Operation::And { destination: #dst, operand1: #lhs, operand2: #rhs }
            ),
            BinaryOperation::BitwiseXor => quote!(
                        Operation::Xor { destination: #dst, operand1: #lhs, operand2: #rhs }
            ),
            BinaryOperation::LogicalLeftShift => quote!(
                        Operation::Sl { destination: #dst, operand: #lhs, shift: #rhs }
            ),
            BinaryOperation::LogicalRightShift => quote!(
                        Operation::Srl { destination: #dst, operand: #lhs, shift: #rhs }
            ),
            BinaryOperation::ArithmeticRightShift => quote!(
                        Operation::Sra { destination: #dst, operand: #lhs, shift: #rhs }
            ),
        };
        let to_insert = state.to_insert_above.drain(..);
        quote!(
        #(#to_insert,)*
        #ret
        )
    }
}
impl Compile for IRExpr {
    type Output = TokenStream;
    fn compile(&self, state: &mut crate::CompilerState<Self::Output>) -> Self::Output {
        match self {
            Self::Assign(assign) => assign.compile(state),
            Self::UnOp(unop) => unop.compile(state),
            Self::BinOp(binop) => binop.compile(state),
            Self::Function(f) => f.compile(state),
        }
    }
}

impl Into<TokenStream> for IR {
    fn into(self) -> TokenStream {
        // let mut declerations: Vec<TokenStream> = vec![];
        // self.extensions
        //     .iter()
        //     .for_each(|el| el.declare(&mut declerations));
        let mut state = CompilerState::new();
        let ret = self.ret.clone().unwrap_or(format_ident!("ret"));
        let ext = self
            .extensions
            .into_iter()
            .map(|el| (ret.clone(), el).compile(&mut state))
            .collect::<Vec<TokenStream>>();
        let declerations = state.to_declare;
        let declaration_strings = declerations.iter().map(|el| el.to_string());
        match self.ret {
            Some(_) => quote!(
                #(let #declerations = Operand::Local(#declaration_strings.to_owned());)*
                #(#ext;)*
            ),
            None => quote!(
                let ret =  Vec::new();
                #(let #declerations = Operand::Local(#declaration_strings.to_owned());)*
                #(#ext;)*
            ),
        }
        .into()
    }
}
