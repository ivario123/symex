use syn::{Ident, Lit};

use super::Operand;

#[derive(Debug, Clone)]
pub struct ZeroExtend {
    pub operand: Operand,
    pub bits: Ident,
}

#[derive(Debug, Clone)]
pub struct SignExtend {
    pub operand: Operand,
    pub bits: Ident,
}

#[derive(Debug, Clone)]
pub struct LocalAddress {
    pub name: Lit,
    pub bits: Lit,
}

#[derive(Debug, Clone)]
pub struct ConditionalJump {
    pub operand: Operand,
    pub condition: Ident,
}

#[derive(Debug, Clone)]
pub struct SetNFlag {
    pub operand: Operand,
}

#[derive(Debug, Clone)]
pub struct SetZFlag {
    pub operand: Operand,
}

// TODO! Implement remaining set flag things
