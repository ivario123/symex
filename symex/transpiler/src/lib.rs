//! Defines a transpiler that allows inline pseudo code
//! to be translated in to [`general_assembly`]
extern crate proc_macro;

use general_assembly::operation::Operation;

use proc_macro::{Delimiter, Literal, TokenStream};
use quote::quote;
use syn::parse::discouraged::Speculative;
use syn::parse::{Parse, ParseStream};
use syn::token::{self, Let, Token};
use syn::{
    parse_macro_input, ConstParam, Expr, Ident, Lifetime, LifetimeParam, Lit, Result, Token,
    TypeParam,
};

#[derive(Debug, Clone)]
enum Function {
    Ident(Ident, Vec<Expr>),
    Intrinsic(Intrinsic),
}
impl Into<proc_macro2::TokenStream> for Function {
    fn into(self) -> proc_macro2::TokenStream {
        match self {
            Self::Ident(i, args) => quote! {#i(#(#args),*)},
            Self::Intrinsic(i) => i.into(),
        }
    }
}
#[derive(Debug, Clone)]
enum Intrinsic {}

impl Into<proc_macro2::TokenStream> for Intrinsic {
    fn into(self) -> proc_macro2::TokenStream {
        todo!("Add instrinsics")
    }
}
#[derive(Debug, Clone)]
enum Operand {
    Expr(ExprOperand),
    Ident(IdentOperand),
    FunctionCall(Box<FunctionCall>),
}
impl Into<proc_macro2::TokenStream> for Operand {
    fn into(self) -> proc_macro2::TokenStream {
        match self {
            Self::Expr(e) => e.into(),
            Self::Ident(i) => i.into(),
            Self::FunctionCall(f) => (*f).into(),
        }
    }
}
#[derive(Debug, Clone)]
struct FunctionCall {
    ident: Function,
    args: Vec<Expr>,
}
impl Into<proc_macro2::TokenStream> for FunctionCall {
    fn into(self) -> proc_macro2::TokenStream {
        let f: proc_macro2::TokenStream = self.ident.into();
        let args = self.args;
        quote! {
            #f(#(#args),*)
        }
    }
}
#[derive(Debug, Clone)]
enum ExprOperand {
    Paren(Expr),
    /// A chain like a.local(<args>).?
    Chain(Box<ExprOperand>, Vec<(Ident, Vec<Box<Operand>>)>),
    Ident(Ident),
    Literal(Lit),
    FunctionCall(Function),
}
impl Into<proc_macro2::TokenStream> for ExprOperand {
    fn into(self) -> proc_macro2::TokenStream {
        match self {
            Self::Paren(p) => quote!((#p)),
            Self::Chain(i, it) => {
                let ident: proc_macro2::TokenStream = (*i).into();
                let ops: Vec<proc_macro2::TokenStream> = it
                    .into_iter()
                    .map(|(ident, args)| {
                        let args = args
                            .into_iter()
                            .map(|el| (*el).into())
                            .collect::<Vec<proc_macro2::TokenStream>>();
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
#[derive(Debug, Clone)]
struct IdentOperand {
    /// Wether or not the struct was created with a let keyword
    define: bool,
    /// The identifier used
    ident: Ident,
}
impl Into<proc_macro2::TokenStream> for IdentOperand {
    fn into(self) -> proc_macro2::TokenStream {
        // let l = match self.define {
        //     true => quote!(let),
        //     false => quote!(),
        // };
        let ident = self.ident;
        quote!(#ident)
    }
}

#[derive(Debug, Clone)]
enum IRExpr {
    UnOp(UnOp),
    BinOp(BinOp),
    Assign(Assign),
}

#[derive(Debug, Clone)]
struct IR {
    /// This must be a [`Vec`]
    ret: Ident,
    extensions: Vec<IRExpr>,
}

#[derive(Debug, Clone)]
enum BinaryOperation {
    Sub,
    Add,
    AddWithCarry,
    Div,
    Mul,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LogicalLeftShift,
    LogicalRightShift,
    ArithmeticRightShift,
}

#[derive(Debug, Clone)]
enum UnaryOperation {
    BitwiseNot,
}

#[derive(Debug, Clone)]
struct Assign {
    dest: Operand,
    rhs: Operand,
}

#[derive(Debug, Clone)]
struct UnOp {
    dest: Operand,
    op: UnaryOperation,
    rhs: Operand,
}

#[derive(Debug, Clone)]
struct BinOp {
    dest: Operand,
    op: BinaryOperation,
    lhs: Operand,
    rhs: Operand,
}

impl Parse for IR {
    fn parse(input: ParseStream) -> Result<Self> {
        // Expected syntax : ret.extend[ .. ]
        let ret: Ident = input.parse()?;
        let _: Token![.] = input.parse()?;
        let token: Ident = input.parse()?;
        if token.to_string() != "extend".to_owned() {
            return Err(input.error("Exptected extend"));
        }
        let mut content;
        syn::bracketed!(content in input);
        let extensions = content.parse_terminated(IRExpr::parse, syn::token::Semi)?;

        let ret = Self {
            ret,
            extensions: extensions.into_iter().collect(),
        };
        Ok(ret)
    }
}

impl Parse for IRExpr {
    fn parse(input: ParseStream) -> Result<Self> {
        println!("Parsing IRExpr");
        let speculative = input.fork();

        if let Ok(unop) = speculative.parse() {
            input.advance_to(&speculative);
            println!("parsed : {unop:?}");
            return Ok(Self::UnOp(unop));
        }
        println!("Not unop");
        let speculative = input.fork();

        if let Ok(binop) = speculative.parse() {
            input.advance_to(&speculative);
            println!("parsed : {binop:?}");
            return Ok(Self::BinOp(binop));
        }
        println!("Not binop");

        let assign: Assign = input.parse()?;
        println!("parsed : {assign:?}");
        Ok(Self::Assign(assign))
    }
}
impl Parse for Assign {
    fn parse(input: ParseStream) -> Result<Self> {
        let dest: Operand = input.parse()?;
        let _: Token![=] = input.parse()?;
        let rhs: Operand = input.parse()?;
        Ok(Self { dest, rhs })
    }
}
impl Parse for UnOp {
    fn parse(input: ParseStream) -> Result<Self> {
        let dest: Operand = input.parse()?;
        let _: Token![=] = input.parse()?;
        let op: UnaryOperation = input.parse()?;
        let rhs: Operand = input.parse()?;
        println!("buffer: {rhs:?}");
        println!("Unop parsed");
        Ok(Self { dest, op, rhs })
    }
}
impl Parse for BinOp {
    fn parse(input: ParseStream) -> Result<Self> {
        let dest: Operand = input.parse()?;
        let _: Token![=] = input.parse()?;

        let lhs: Operand = input.parse()?;

        let op: BinaryOperation = input.parse()?;

        let rhs: Operand = input.parse()?;
        Ok(Self { dest, op, lhs, rhs })
    }
}
impl Parse for Function {
    fn parse(input: ParseStream) -> Result<Self> {
        let speculative = input.fork();
        if let Ok(intrinsic) = speculative.parse() {
            input.advance_to(&speculative);
            return Ok(Self::Intrinsic(intrinsic));
        }
        let ident: Ident = input.parse()?;
        let mut content;
        syn::parenthesized!(content in input);
        let inner = content.parse_terminated(Expr::parse, Token![,])?;
        Ok(Self::Ident(ident, inner.into_iter().collect()))
    }
}
impl Parse for Intrinsic {
    fn parse(input: ParseStream) -> Result<Self> {
        Err(input.error("TODO!( parse intrinsics )"))
    }
}
impl Parse for ExprOperand {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(syn::token::Paren) {
            let mut content;
            syn::parenthesized!(content in input);
            let inner: Expr = content.parse()?;
            // This needs to be cleaned up
            if input.peek(Token![.]) {
                let mut ops = vec![];
                while input.peek(Token![.]) {
                    let _: Token![.] = input.parse()?;
                    let fident: Ident = input.parse()?;
                    if input.peek(syn::token::Paren) {
                        let mut content;
                        syn::parenthesized!(content in input);
                        let operands =
                            content.parse_terminated(Operand::parse, syn::token::Semi)?;
                        ops.push((
                            fident,
                            operands.into_iter().map(|el| Box::new(el)).collect(),
                        ));
                        continue;
                    }
                    return Err(input.error("Expected function arguments"));
                }
                // Chain(Box<ExprOperand>, Vec<(Ident, Vec<Box<Operand>>)>),
                return Ok(Self::Chain(Box::new(Self::Paren(inner)), ops));
            }
            return Ok(Self::Paren(inner));
        }
        let speculative = input.fork();

        if let Ok(literal) = speculative.parse() {
            input.advance_to(&speculative);
            // This needs to be cleaned up
            if input.peek(Token![.]) {
                let mut ops = vec![];
                while input.peek(Token![.]) {
                    let _: Token![.] = input.parse()?;
                    let fident: Ident = input.parse()?;
                    if input.peek(syn::token::Paren) {
                        let mut content;
                        syn::parenthesized!(content in input);
                        let operands =
                            content.parse_terminated(Operand::parse, syn::token::Semi)?;
                        ops.push((
                            fident,
                            operands.into_iter().map(|el| Box::new(el)).collect(),
                        ));
                        continue;
                    }
                    return Err(input.error("Expected function arguments"));
                }
                // Chain(Box<ExprOperand>, Vec<(Ident, Vec<Box<Operand>>)>),
                return Ok(Self::Chain(Box::new(Self::Literal(literal)), ops));
            }
            return Ok(Self::Literal(literal));
        }
        let speculative = input.fork();
        let ident = speculative.parse()?;
        if speculative.peek(Token![.]) {
            input.advance_to(&speculative);
            let mut ops = vec![];
            while input.peek(Token![.]) {
                let _: Token![.] = input.parse()?;
                let fident: Ident = input.parse()?;
                if input.peek(syn::token::Paren) {
                    let mut content;
                    syn::parenthesized!(content in input);
                    let operands = content.parse_terminated(Operand::parse, syn::token::Semi)?;
                    ops.push((
                        fident,
                        operands.into_iter().map(|el| Box::new(el)).collect(),
                    ));
                    continue;
                }
                return Err(input.error("Expected function arguments"));
            }
            // Chain(Box<ExprOperand>, Vec<(Ident, Vec<Box<Operand>>)>),
            return Ok(Self::Chain(Box::new(Self::Ident(ident)), ops));
        }
        let speculative_f = input.fork();
        if let Ok(function_call) = speculative_f.parse() {
            input.advance_to(&speculative_f);
            if input.peek(Token![.]) {
                let mut ops = vec![];
                while input.peek(Token![.]) {
                    let _: Token![.] = input.parse()?;
                    let fident: Ident = input.parse()?;
                    if input.peek(syn::token::Paren) {
                        let mut content;
                        syn::parenthesized!(content in input);
                        let operands =
                            content.parse_terminated(Operand::parse, syn::token::Semi)?;
                        ops.push((
                            fident,
                            operands.into_iter().map(|el| Box::new(el)).collect(),
                        ));
                        continue;
                    }
                    return Err(input.error("Expected function arguments"));
                }
                // Chain(Box<ExprOperand>, Vec<(Ident, Vec<Box<Operand>>)>),
                return Ok(Self::Chain(
                    Box::new(Self::FunctionCall(function_call)),
                    ops,
                ));
            }
            return Ok(Self::FunctionCall(function_call));
        }
        input.advance_to(&speculative);
        Ok(Self::Ident(ident))
    }
}
impl Parse for Operand {
    fn parse(input: ParseStream) -> Result<Self> {
        let speculative = input.fork();
        if let Ok(val) = speculative.parse() {
            input.advance_to(&speculative);
            return Ok(Self::Expr(val));
        }
        let speculative = input.fork();
        if let Ok(val) = speculative.parse() {
            input.advance_to(&speculative);
            return Ok(Self::Ident(val));
        }
        let speculative = input.fork();
        if let Ok(val) = speculative.parse() {
            input.advance_to(&speculative);
            return Ok(Self::FunctionCall(val));
        }
        Err(input.error("Expected operand"))
    }
}
impl Parse for FunctionCall {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident: Function = input.parse()?;

        let content;
        syn::parenthesized!(content in input);
        let args = content.parse_terminated(Expr::parse, syn::token::Comma)?;
        let ret = Self {
            ident,
            args: args.into_iter().collect(),
        };
        
        Ok(ret)
    }
}

impl Parse for IdentOperand {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Let) {
            let _: Let = input.parse()?;
            let ident: Ident = input.parse()?;
            return Ok(Self {
                define: true,
                ident,
            });
        }
        let ident: Ident = input.parse()?;
        Ok(Self {
            define: false,
            ident,
        })
    }
}

impl Parse for UnaryOperation {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Token![!]) {
            let _: Token![!] = input.parse()?;
            return Ok(Self::BitwiseNot);
        }
        Err(input.error("Expected unary op"))
    }
}
impl Parse for BinaryOperation {
    fn parse(input: ParseStream) -> Result<Self> {
        use BinaryOperation::*;
        if input.peek(Token![+]) {
            let _: Token![+] = input.parse()?;
            return Ok(Add);
        }
        if input.peek(Token![-]) {
            let _: Token![-] = input.parse()?;
            return Ok(Sub);
        }
        if input.peek(Ident) {
            let ident: Ident = input.parse()?;
            if ident.to_string().to_lowercase() == "adc" {
                return Ok(AddWithCarry);
            } else {
                todo!()
                // compile_error!("Expected \"Adc\" found {:}",ident.to_string());
            }
        }
        if input.peek(syn::token::Slash) {
            let _: syn::token::Slash = input.parse()?;
            return Ok(Self::Div);
        }
        if input.peek(Token![*]) {
            let _: Token![*] = input.parse()?;
            return Ok(Self::Mul);
        }
        if input.peek(Token![&]) {
            let _: Token![&] = input.parse()?;
            return Ok(Self::BitwiseAnd);
        }
        if input.peek(Token![|]) {
            let _: Token![|] = input.parse()?;
            return Ok(Self::BitwiseOr);
        }
        if input.peek(Token![^]) {
            let _: Token![^] = input.parse()?;
            return Ok(Self::BitwiseXor);
        }
        if input.peek(Token![>>]) {
            let _: Token![>>] = input.parse()?;
            return Ok(Self::LogicalRightShift);
        }
        if input.peek(Token![<<]) {
            let _: Token![<<] = input.parse()?;
            return Ok(Self::LogicalLeftShift);
        }
        if input.peek(Ident) {
            let ident: Ident = input.parse()?;
            // Revisit this later
            if ident.to_string().to_lowercase() == "asr" {
                return Ok(ArithmeticRightShift);
            } else {
                todo!()
                // compile_error!("Expected \"Adc\" found {:}",ident.to_string());
            }
        }
        Err(input.error("Expected operation"))
    }
}

impl Into<proc_macro2::TokenStream> for Assign {
    fn into(self) -> proc_macro2::TokenStream {
        let dst: proc_macro2::TokenStream = self.dest.into();
        let rhs: proc_macro2::TokenStream = self.rhs.into();
        quote::quote! {
            Operation::Move { destination: #dst, source: #rhs }
        }
        .into()
    }
}
impl Into<proc_macro2::TokenStream> for UnOp {
    fn into(self) -> proc_macro2::TokenStream {
        let dst: proc_macro2::TokenStream = self.dest.into();
        let rhs: proc_macro2::TokenStream = self.rhs.into();
        match self.op {
            UnaryOperation::BitwiseNot => quote!(
                Operation::Not { destination: #dst, operand: #rhs }
            ),
        }
    }
}

impl Into<proc_macro2::TokenStream> for BinOp {
    fn into(self) -> proc_macro2::TokenStream {
        let dst: proc_macro2::TokenStream = self.dest.into();
        let rhs: proc_macro2::TokenStream = self.rhs.into();
        let lhs: proc_macro2::TokenStream = self.lhs.into();
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
impl Into<proc_macro2::TokenStream> for IRExpr {
    fn into(self) -> proc_macro2::TokenStream {
        match self {
            Self::Assign(assign) => assign.into(),
            Self::UnOp(unop) => unop.into(),
            Self::BinOp(binop) => binop.into(),
        }
    }
}

impl Declaration for IdentOperand {
    fn declare(&self, declerations: &mut Vec<proc_macro2::TokenStream>) {
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
    fn declare(&self, declerations: &mut Vec<proc_macro2::TokenStream>) {
        match self {
            Self::Ident(i) => i.declare(declerations),
            _ => {}
        }
    }
}
impl Declaration for Assign {
    fn declare(&self, declerations: &mut Vec<proc_macro2::TokenStream>) {
        self.dest.declare(declerations);
        // self.lhs.declare(declerations);
        self.rhs.declare(declerations);
    }
}
impl Declaration for UnOp {
    fn declare(&self, declerations: &mut Vec<proc_macro2::TokenStream>) {
        self.dest.declare(declerations);
        // self.lhs.declare(declerations);
        self.rhs.declare(declerations);
    }
}
impl Declaration for BinOp {
    fn declare(&self, declerations: &mut Vec<proc_macro2::TokenStream>) {
        self.dest.declare(declerations);
        self.lhs.declare(declerations);
        self.rhs.declare(declerations);
    }
}
impl Declaration for IRExpr {
    fn declare(&self, declerations: &mut Vec<proc_macro2::TokenStream>) {
        match self {
            Self::UnOp(o) => o.declare(declerations),
            Self::BinOp(o) => o.declare(declerations),
            Self::Assign(o) => o.declare(declerations),
        }
    }
}

trait Declaration {
    fn declare(&self, declerations: &mut Vec<proc_macro2::TokenStream>);
}
impl Into<TokenStream> for IR {
    fn into(self) -> TokenStream {
        let mut declerations: Vec<proc_macro2::TokenStream> = vec![];
        self.extensions
            .iter()
            .for_each(|el| el.declare(&mut declerations));

        let ext = self
            .extensions
            .into_iter()
            .map(|el| el.into())
            .collect::<Vec<proc_macro2::TokenStream>>();
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
#[proc_macro]
pub fn pseudo(item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as IR);
    let input: TokenStream = input.into();
    println!("Parsed : {input}");

    input
}
