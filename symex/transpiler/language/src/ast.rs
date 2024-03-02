use syn::{Expr, Ident, Lit};
pub mod intrinsic;
use intrinsic::ZeroExtend;

use self::intrinsic::{ConditionalJump, LocalAddress, SetNFlag, SetZFlag, SignExtend};

#[derive(Debug, Clone)]
pub enum Function {
    Ident(Ident, Vec<Expr>),
    Intrinsic(Box<Intrinsic>),
}
#[derive(Debug, Clone)]
pub enum RustSyntax {
    /// TODO! Make this accept full expressions
    If(Ident, Box<RustSyntax>, Option<Box<RustSyntax>>),
    For(Ident, Expr, Box<RustSyntax>),
    Exprs(Vec<Box<IRExpr>>),
    RustExpr(Expr),
}
#[derive(Debug, Clone)]
pub enum Intrinsic {
    ZeroExtend(ZeroExtend),
    SignExtend(SignExtend),
    ConditionalJump(ConditionalJump),
    SetNFlag(SetNFlag),
    SetZFlag(SetZFlag),
    LocalAddress(LocalAddress),
}
#[derive(Debug, Clone)]
pub enum Operand {
    Expr(ExprOperand),
    Ident(IdentOperand),
    FunctionCall(Box<Function>),
}
#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub ident: Function,
    pub args: Vec<Expr>,
}
#[derive(Debug, Clone)]
pub enum ExprOperand {
    Paren(Expr),
    /// A chain like a.local(<args>).?
    Chain(Box<ExprOperand>, Vec<(Ident, Vec<Box<Operand>>)>),
    Ident(Ident),
    Literal(Lit),
    FunctionCall(Function),
}
#[derive(Debug, Clone)]
pub struct IdentOperand {
    /// Wether or not the pub struct was created with a let keyword
    pub define: bool,
    /// The identifier used
    pub ident: Ident,
}

#[derive(Debug, Clone)]
pub enum IRExpr {
    UnOp(UnOp),
    BinOp(BinOp),
    Assign(Assign),
    Function(Function),
}

#[derive(Debug, Clone)]
pub struct IR {
    /// This must be a [`Vec`]
    pub ret: Option<Ident>,
    // pub extensions: Vec<IRExpr>,
    pub extensions: Vec<RustSyntax>,
}

#[derive(Debug, Clone)]
pub enum BinaryOperation {
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
pub enum UnaryOperation {
    BitwiseNot,
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub dest: Operand,
    pub rhs: Operand,
}

#[derive(Debug, Clone)]
pub struct UnOp {
    pub dest: Operand,
    pub op: UnaryOperation,
    pub rhs: Operand,
}

#[derive(Debug, Clone)]
pub struct BinOp {
    pub dest: Operand,
    pub op: BinaryOperation,
    pub lhs: Operand,
    pub rhs: Operand,
}
