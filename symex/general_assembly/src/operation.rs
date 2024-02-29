//! Defines all operations that are valid in [`GeneralAssembly`]

use crate::operand::Operand;
use crate::condition::Condition;
use crate::shift::Shift;

/// Represents a single operation
#[derive(Debug, Clone)]
pub enum Operation {
    /// No operation
    Nop,

    /// Moves the value in the source to the destination.
    /// If source is an address it is loaded from memmory
    /// and if destination is an address it is stored into memmory.
    Move {
        destination: Operand,
        source: Operand,
    },

    /// Adds
    /// destination = operand1 + operand2
    Add {
        destination: Operand,
        operand1: Operand,
        operand2: Operand,
    },

    /// Add with carry
    Adc {
        destination: Operand,
        operand1: Operand,
        operand2: Operand,
    },

    /// Subtracts
    /// destination = operand1 - operand2
    Sub {
        destination: Operand,
        operand1: Operand,
        operand2: Operand,
    },

    /// Multiply
    /// destination = operand1 * operand2
    Mul {
        destination: Operand,
        operand1: Operand,
        operand2: Operand,
    },

    /// And
    /// destination = operand1 & operand2
    And {
        destination: Operand,
        operand1: Operand,
        operand2: Operand,
    },

    /// Or
    /// destination = operand1 | operand2
    Or {
        destination: Operand,
        operand1: Operand,
        operand2: Operand,
    },

    /// Xor
    /// destination = operand1 ^ operand2
    Xor {
        destination: Operand,
        operand1: Operand,
        operand2: Operand,
    },

    /// Not
    /// destination = !operand
    Not {
        destination: Operand,
        operand: Operand,
    },

    /// General rotation or shift
    Shift {
        destination: Operand,
        operand: Operand,
        shift_n: Operand,
        shift_t: Shift,
    },

    /// Shift left
    /// destination = operand << shift
    Sl {
        destination: Operand,
        operand: Operand,
        shift: Operand,
    },

    /// Shift rigt logical
    /// destination = operand >> shift
    Srl {
        destination: Operand,
        operand: Operand,
        shift: Operand,
    },

    /// Shift rigt arithmetic
    /// destination = operand >> shift
    Sra {
        destination: Operand,
        operand: Operand,
        shift: Operand,
    },

    /// Rotating shift right
    /// Rotate the `operand` `shift` number of steps
    /// and store the result in `destination`.
    Sror {
        destination: Operand,
        operand: Operand,
        shift: Operand,
    },

    /// Zero extend
    /// Zero extends `bits` bits from operand and stores it in destination..
    ZeroExtend {
        destination: Operand,
        operand: Operand,
        bits: u32,
    },

    /// Sign extend
    SignExtend {
        destination: Operand,
        operand: Operand,
        bits: u32,
    },

    /// Conditional jump
    ConditionalJump {
        destination: Operand,
        condition: Condition,
    },

    /// Set the negative flag
    SetNFlag(Operand),

    /// Set the zero flag
    SetZFlag(Operand),

    /// Set the carry flag
    SetCFlag {
        operand1: Operand,
        operand2: Operand,
        sub: bool,
        carry: bool,
    },

    /// Sets the carry flag based on a given [`Shift`]
    SetCFlagShift {
        operand: Operand,
        shift_n: Operand,
        shift_t: Shift,
    },

    /// Set the carry flag based on a left shift
    SetCFlagShiftLeft { operand: Operand, shift: Operand },

    /// Set the carry flag based on a right shift logical
    SetCFlagSrl { operand: Operand, shift: Operand },

    /// Set the carry flag based on a right shift arithemtic
    SetCFlagSra { operand: Operand, shift: Operand },

    /// Set the carry flag based on a bit rotation
    SetCFlagRor(Operand),

    /// Set overfolow flag
    SetVFlag {
        operand1: Operand,
        operand2: Operand,
        sub: bool,
        carry: bool,
    },

    /// Do all the operations in operations for each operand.
    /// The current operand is stored in local as CurrentOperand.
    ForEach {
        operands: Vec<Operand>,
        operations: Vec<Operation>,
    },

    /// Sets the destination value to a fresh symbol
    Symbolic { destination: Operand, name: String },
}
