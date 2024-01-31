//! Defines a generic architecture
//!
//! An architecture is in the scope of this crate
//! something that defines a instruction set that
//! can be translated in to general_assembly [`Instruction`]s.
//! Moreover the architecture may define a few
//! architecture specific hooks

pub mod arm;
use std::fmt::Debug;

use crate::general_assembly::{instruction::Instruction, RunConfig};

#[derive(Debug)]
/// General architecture related errors.
pub enum ArchError {
    /// Thrown when an unsupported architecture is requested.
    UnsuportedArchitechture,
    /// Thrown when an unsupported file type is used.
    IncorrectFileType,
    /// Thrown when the binary files fields are malformed.
    MalformedSection,
    /// Thrown when a specific required section does not exist in the binary
    MissingSection(&'static str),
    /// Thrown when a different module errors and that error is not convertible in to an [`ArchError`]
    ImplementorStringError(&'static str),
    /// Thrown when something goes wrong during instruction parsing.
    ParsingError(ParseError),
}

#[derive(Debug)]
pub enum ParseError {
    /// Input not long enough for an instruction.
    InsufficientInput,
    /// 32 bit instruction not long enough.
    MalfromedInstruction,
    /// Opcode not matching valid 32 bit instruction.
    InvalidInstruction,
    /// This instruction causes unpredictable behaviour.
    Unpredictable,
    /// Trying to access an invalid register.
    InvalidRegister,
    /// Invalid condition code used.
    InvalidCondition,
}

/// A generic architecture
///
/// Denotes that the implementer can be treated as an architecture in this crate.
pub trait Arch: Debug {
    /// Converts a slice of bytes to an [`Instruction`]
    fn translate(&self, buff: &'static [u8]) -> Result<Instruction, ArchError>;

    /// Adds the architecture specific hooks to the [`RunConfig`]
    fn add_hooks(&self, cfg: &mut RunConfig);
}
