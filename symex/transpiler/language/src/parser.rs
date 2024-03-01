use syn::parse::discouraged::Speculative;

use syn::parse::{Parse, ParseStream};
use syn::token::{Let, Token};
use syn::{Expr, Ident, Result, Token};

use crate::ast::*;
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
        let speculative = input.fork();

        if let Ok(unop) = speculative.parse() {
            input.advance_to(&speculative);
            return Ok(Self::UnOp(unop));
        }
        let speculative = input.fork();

        if let Ok(binop) = speculative.parse() {
            input.advance_to(&speculative);
            return Ok(Self::BinOp(binop));
        }

        let assign: Assign = input.parse()?;
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
