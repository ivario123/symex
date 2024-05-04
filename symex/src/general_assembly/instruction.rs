//! Describes a general assembly instruction.

use super::{state::GAState, DataWord};

/// Representing a cycle count for a instruction.
#[derive(Debug, Clone)]
pub enum CycleCount {
    /// Cycle count is a precalculated value
    Value(usize),

    /// Cycle count depends on execution state
    Function(fn(state: &GAState) -> usize),
}

/// Represents a general assembly instruction.
#[derive(Debug, Clone)]
pub struct Instruction {
    /// The size of the original machine instruction in number of bits.
    pub instruction_size: u32,

    /// A list of operations that will be executed in order when
    /// executing the instruction.
    pub operations: Vec<Operation>,

    /// The maximum number of cycles the instruction will take.
    /// This can depend on state and will be evaluated after the
    /// instruction has executed but before the next instruction.
    pub max_cycle: CycleCount,
}

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
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Condition {
    /// Equal Z = 1
    EQ,

    /// Not Equal Z = 0
    NE,

    /// Carry set C = 1
    CS,

    /// Carry clear C = 0
    CC,

    /// Negative N = 1
    MI,

    /// Positive or zero N = 0
    PL,

    /// Overflow V = 1
    VS,

    /// No overflow V = 0
    VC,

    /// Unsigned higher C = 1 AND Z = 0
    HI,

    /// Unsigned lower or equal C = 0 OR Z = 1
    LS,

    /// Signed higher or equal N = V
    GE,

    /// Signed lower N != V
    LT,

    /// Signed higher Z = 0 AND N = V
    GT,

    /// Signed lower or equal Z = 1 OR N != V
    LE,

    /// No condition always true
    None,
}

/// A operand representing some value.
#[derive(Debug, Clone)]
pub enum Operand {
    /// Representing a value in a register.
    Register(String),

    /// Representing an immediate value.
    Immidiate(DataWord),

    /// Representing the value stored in memory
    /// at the address stored in a local.
    AddressInLocal(String, u32),

    /// Representing the value stored in memory
    /// at the constant address.
    Address(DataWord, u32),

    /// Representing the value stored in memory
    /// at the address stored in a register offset
    /// by an constant value.
    AddressWithOffset {
        address: DataWord,
        offset_reg: String,
        width: u32,
    },

    /// Represent the value that is local to the instruction.
    Local(String),
}
