use crate::ast::*;
use proc_macro2::TokenStream;
use quote::quote;

impl Into<TokenStream> for Function {
    fn into(self) -> TokenStream {
        match self {
            Self::Ident(i, args) => quote! {#i(#(#args),*)},
            Self::Intrinsic(i) => i.into(),
        }
    }
}

impl Into<TokenStream> for Intrinsic {
    fn into(self) -> TokenStream {
        todo!("Add instrinsics")
    }
}
impl Into<TokenStream> for Operand {
    fn into(self) -> TokenStream {
        match self {
            Self::Expr(e) => e.into(),
            Self::Ident(i) => i.into(),
            Self::FunctionCall(f) => (*f).into(),
        }
    }
}
impl Into<TokenStream> for FunctionCall {
    fn into(self) -> TokenStream {
        let f: TokenStream = self.ident.into();
        let args = self.args;
        quote! {
            #f(#(#args),*)
        }
    }
}
impl Into<TokenStream> for ExprOperand {
    fn into(self) -> TokenStream {
        match self {
            Self::Paren(p) => quote!((#p)),
            Self::Chain(i, it) => {
                let ident: TokenStream = (*i).into();
                let ops: Vec<TokenStream> = it
                    .into_iter()
                    .map(|(ident, args)| {
                        let args = args
                            .into_iter()
                            .map(|el| (*el).into())
                            .collect::<Vec<TokenStream>>();
                        quote!(#ident(#(#args),*))
                    })
                    .collect();
                quote!(#ident.#(#ops).*)
            }
            Self::Ident(i) => quote!(#i.clone()),
            Self::Literal(l) => quote!(#l),
            Self::FunctionCall(f) => f.into(),
        }
    }
}
impl Into<TokenStream> for IdentOperand {
    fn into(self) -> TokenStream {
        // let l = match self.define {
        //     true => quote!(let),
        //     false => quote!(),
        // };
        let ident = self.ident;
        quote!(#ident)
    }
}

impl Into<TokenStream> for Assign {
    fn into(self) -> TokenStream {
        let dst: TokenStream = self.dest.into();
        let rhs: TokenStream = self.rhs.into();
        quote::quote! {
            Operation::Move { destination: #dst, source: #rhs }
        }
        .into()
    }
}
impl Into<TokenStream> for UnOp {
    fn into(self) -> TokenStream {
        let dst: TokenStream = self.dest.into();
        let rhs: TokenStream = self.rhs.into();
        match self.op {
            UnaryOperation::BitwiseNot => quote!(
                Operation::Not { destination: #dst, operand: #rhs }
            ),
        }
    }
}

impl Into<TokenStream> for BinOp {
    fn into(self) -> TokenStream {
        let dst: TokenStream = self.dest.into();
        let rhs: TokenStream = self.rhs.into();
        let lhs: TokenStream = self.lhs.into();
        match self.op {
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
        }
    }
}
impl Into<TokenStream> for IRExpr {
    fn into(self) -> TokenStream {
        match self {
            Self::Assign(assign) => assign.into(),
            Self::UnOp(unop) => unop.into(),
            Self::BinOp(binop) => binop.into(),
        }
    }
}

impl Declaration for IdentOperand {
    fn declare(&self, declerations: &mut Vec<TokenStream>) {
        if self.define {
            let ident = self.ident.clone();
            let id = self.ident.to_string();
            declerations.push(quote!(
                let #ident = Operand::Local(#id.to_owned())
            ));
        }
    }
}
impl Declaration for Operand {
    fn declare(&self, declerations: &mut Vec<TokenStream>) {
        match self {
            Self::Ident(i) => i.declare(declerations),
            _ => {}
        }
    }
}
impl Declaration for Assign {
    fn declare(&self, declerations: &mut Vec<TokenStream>) {
        self.dest.declare(declerations);
        // self.lhs.declare(declerations);
        self.rhs.declare(declerations);
    }
}
impl Declaration for UnOp {
    fn declare(&self, declerations: &mut Vec<TokenStream>) {
        self.dest.declare(declerations);
        // self.lhs.declare(declerations);
        self.rhs.declare(declerations);
    }
}
impl Declaration for BinOp {
    fn declare(&self, declerations: &mut Vec<TokenStream>) {
        self.dest.declare(declerations);
        self.lhs.declare(declerations);
        self.rhs.declare(declerations);
    }
}
impl Declaration for IRExpr {
    fn declare(&self, declerations: &mut Vec<TokenStream>) {
        match self {
            Self::UnOp(o) => o.declare(declerations),
            Self::BinOp(o) => o.declare(declerations),
            Self::Assign(o) => o.declare(declerations),
        }
    }
}

trait Declaration {
    fn declare(&self, declerations: &mut Vec<TokenStream>);
}
impl Into<TokenStream> for IR {
    fn into(self) -> TokenStream {
        let mut declerations: Vec<TokenStream> = vec![];
        self.extensions
            .iter()
            .for_each(|el| el.declare(&mut declerations));

        let ext = self
            .extensions
            .into_iter()
            .map(|el| el.into())
            .collect::<Vec<TokenStream>>();
        let ret = self.ret;
        quote!(
        #(#declerations);*;
        #ret.extend([
            #(#ext),*
        ]);
        )
        .into()
    }
}
