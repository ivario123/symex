
use syn::{Ident,Token};
use general_assembly::shift::Shift;

/// Intermediate representation
/// these are a chain of
pub enum ExprElements{
    Ident(Ident)

}

pub enum Operand{

}

pub enum Operation {
    Add,
    Sub,
    Adc,
    Mul,
    Div,
    Shift(Shift)
}

pub enum BinOp{
    
}
