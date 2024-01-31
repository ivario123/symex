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
    /// Thrown when an instruction was not parseable
    MalformedInstruction
}

/// A generic architecture
///
///
/// Denotes that the implementer can be treated as an architecture in this tool.
pub trait Arch: Debug {
    fn translate(&self, buff: &'static [u8]) -> Result<Instruction, ArchError>;
    fn add_hooks(&self, cfg: &mut RunConfig);
}
