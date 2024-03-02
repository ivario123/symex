use self::intrinsic::{ConditionalJump, LocalAddress, SetNFlag, SetZFlag, SignExtend, ZeroExtend};
use crate::ast::*;
use syn::parse::discouraged::Speculative;
use syn::parse::{Parse, ParseStream};
use syn::token::{Let, Token};
use syn::{Expr, Ident, Lit, Result, Token};

impl Parse for IR {
    fn parse(input: ParseStream) -> Result<Self> {
        // Expected syntax : ret.extend[ .. ]
        let speculative = input.fork();
        let ret: Option<Ident> = match Ident::parse(&speculative) {
            Ok(ret) => {
                input.advance_to(&speculative);
                let _: Token![.] = input.parse()?;
                let token: Ident = input.parse()?;
                if token.to_string() != "extend".to_owned() {
                    return Err(input.error("Exptected extend"));
                }
                Some(ret)
            }
            _ => None,
        };
        let content;
        syn::bracketed!(content in input);
        let mut extensions: Vec<RustSyntax> = vec![];
        while !content.is_empty() {
            extensions.push(content.parse()?);
        }

        let ret = Self {
            ret,
            extensions: extensions.into_iter().collect(),
        };
        Ok(ret)
    }
}
impl Parse for RustSyntax {
    fn parse(input: ParseStream) -> Result<Self> {
        while !input.is_empty() {
            if input.peek(Token![if]) {
                let _: Token![if] = input.parse()?;
                // Maaaasive limit, this should be expanded in the future
                let e: Ident = input.parse()?;
                let content;
                syn::braced!(content in input);
                let happy_case: Box<RustSyntax> = Box::new(content.parse()?);
                let sad_case = if input.peek(Token![else]) {
                    let _: Token![else] = input.parse()?;
                    let content;
                    syn::braced!(content in input);
                    Some(Box::new(content.parse()?))
                } else {
                    None
                };
                return Ok(Self::If(e, happy_case, sad_case));
            }
            if input.peek(Token![for]) {
                let _: Token![for] = input.parse()?;
                let var: Ident = input.parse()?;
                let _: Token![in] = input.parse()?;
                let e: Expr = input.parse()?;
                let content;
                syn::braced!(content in input);
                let block: Box<RustSyntax> = Box::new(content.parse()?);
                return Ok(Self::For(var, e, block));
            }
            let mut ret = vec![];
            while !input.is_empty() {
                if input.peek(Token![if]) | input.peek(Token![for]) {
                    break;
                }
                let speculative = input.fork();
                match speculative.parse() {
                    Ok(val) => {
                        input.advance_to(&speculative);
                        ret.push(val);
                        let _: syn::token::Semi = input.parse()?;
                    }
                    _ => {
                        break;
                    }
                }
            }
            return Ok(Self::Exprs(ret));
        }
        Err(input.error("Expected either an expression, if statement or a for loop."))
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
        if let Ok(assign) = speculative.parse() {
            let speculative_speculative = speculative.fork();
            let token = syn::token::Semi::parse(&speculative_speculative);
            match token {
                Ok(_) => {
                    input.advance_to(&speculative);
                    return Ok(Self::Assign(assign));
                }
                _ => {}
            }
        }

        let speculative = input.fork();
        if let Ok(func) = speculative.parse() {
            let speculative_speculative = speculative.fork();
            let token = syn::token::Semi::parse(&speculative_speculative);
            match token {
                Ok(_) => {
                    input.advance_to(&speculative);
                    return Ok(Self::Function(func));
                }
                _ => {}
            }
        }

        let binop: BinOp = input.parse()?;
        Ok(Self::BinOp(binop))
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
        if !input.peek(syn::token::Semi) {
            return Err(input.error("Expected semi colon"));
        }
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
        let speculative = input.fork();
        if let Ok(el) = speculative.parse() {
            input.advance_to(&speculative);
            return Ok(Self::LocalAddress(el));
        }

        let speculative = input.fork();
        if let Ok(el) = speculative.parse() {
            input.advance_to(&speculative);
            return Ok(Self::ZeroExtend(el));
        }
        let speculative = input.fork();
        if let Ok(el) = speculative.parse() {
            input.advance_to(&speculative);
            return Ok(Self::SignExtend(el));
        }

        let speculative = input.fork();
        if let Ok(el) = speculative.parse() {
            input.advance_to(&speculative);
            return Ok(Self::ConditionalJump(el));
        }

        let speculative = input.fork();
        if let Ok(el) = speculative.parse() {
            input.advance_to(&speculative);
            return Ok(Self::SetNFlag(el));
        }
        Ok(Self::SetZFlag(input.parse()?))
    }
}
impl Parse for LocalAddress {
    fn parse(input: ParseStream) -> Result<Self> {
        let speculative = input.fork();
        let ident: Ident = speculative.parse()?;
        if ident.to_string().to_lowercase() != "localaddress".to_owned() {
            return Err(input.error("localaddress"));
        }
        input.advance_to(&speculative);
        let content;
        syn::parenthesized!(content in input);
        let name: Lit = content.parse()?;
        let _: Token![,] = content.parse()?;
        let bits: Lit = content.parse()?;
        if !content.is_empty() {
            return Err(content.error("Too many arguments"));
        }
        Ok(Self { name, bits })
    }
}
impl Parse for ZeroExtend {
    fn parse(input: ParseStream) -> Result<Self> {
        let speculative = input.fork();
        let ident: Ident = speculative.parse()?;
        if ident.to_string().to_lowercase() != "zeroextend".to_owned() {
            return Err(input.error("Expected zeroextend"));
        }
        input.advance_to(&speculative);
        let content;
        syn::parenthesized!(content in input);
        let op: Operand = content.parse()?;
        let _: Token![,] = content.parse()?;
        let n: Ident = content.parse()?;
        if !content.is_empty() {
            return Err(content.error("Too many arguments"));
        }
        Ok(Self {
            operand: op,
            bits: n,
        })
    }
}
impl Parse for SignExtend {
    fn parse(input: ParseStream) -> Result<Self> {
        let speculative = input.fork();
        let ident: Ident = speculative.parse()?;
        if ident.to_string().to_lowercase() != "signextend".to_owned() {
            return Err(input.error("Expected signextend"));
        }
        input.advance_to(&speculative);
        let content;
        syn::parenthesized!(content in input);
        let op: Operand = content.parse()?;
        let _: Token![,] = content.parse()?;
        let n: Ident = content.parse()?;
        if !content.is_empty() {
            return Err(content.error("Too many arguments"));
        }
        Ok(Self {
            operand: op,
            bits: n,
        })
    }
}
impl Parse for ConditionalJump {
    fn parse(input: ParseStream) -> Result<Self> {
        let speculative = input.fork();
        let ident: Ident = speculative.parse()?;
        if ident.to_string().to_lowercase() != "signextend".to_owned() {
            return Err(input.error("Expected signextend"));
        }
        input.advance_to(&speculative);
        let content;
        syn::parenthesized!(content in input);
        let op: Operand = content.parse()?;
        let _: Token![,] = content.parse()?;
        let condition: Ident = content.parse()?;
        if !content.is_empty() {
            return Err(content.error("Too many arguments"));
        }
        Ok(Self {
            operand: op,
            condition,
        })
    }
}
impl Parse for SetNFlag {
    fn parse(input: ParseStream) -> Result<Self> {
        let speculative = input.fork();
        let ident: Ident = speculative.parse()?;
        if ident.to_string().to_lowercase() != "setnflag".to_owned() {
            return Err(input.error("Expected setnflag"));
        }
        input.advance_to(&speculative);
        let content;
        syn::parenthesized!(content in input);
        let op: Operand = content.parse()?;
        if !content.is_empty() {
            return Err(content.error("Too many arguments"));
        }
        Ok(Self { operand: op })
    }
}
impl Parse for SetZFlag {
    fn parse(input: ParseStream) -> Result<Self> {
        let speculative = input.fork();
        let ident: Ident = speculative.parse()?;
        if ident.to_string().to_lowercase() != "setzflag".to_owned() {
            return Err(input.error("Expected setzflag"));
        }
        input.advance_to(&speculative);
        let content;
        syn::parenthesized!(content in input);
        let op: Operand = content.parse()?;
        if !content.is_empty() {
            return Err(content.error("Too many arguments"));
        }
        Ok(Self { operand: op })
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
            return Ok(Self::FunctionCall(val));
        }
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
