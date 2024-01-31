//! Describes the translator trait.
//! A translator that translates between machine code and general assembly instructions.

use super::{instruction::Instruction, RunConfig};

/// A translator
pub trait Translatable {
    /// Translate the given instruction into a GA instruction.
    fn translate(&self) -> Instruction;
}

/// Something that has hooks
pub trait Hookable {
    fn add_hooks(cfg: &mut RunConfig);
}
