//! Describes a general assembly instruction.

use super::state::GAState;
pub(crate) use general_assembly::prelude::*;

/// Representing a cycle count for a instruction.
#[derive(Debug, Clone)]
pub enum CycleCount {
    /// Cycle count is a precalculated value
    Value(usize),

    // This might need to be reworked
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
