//! Defines all types of shifts that are valid in the [`GeneralAssembly`] language

#[derive(Debug, Clone)]
pub enum Shift {
    /// Logical left shift
    Lsl,
    /// Logical right sift
    Lsr,
    /// Arithmetic right shift    
    Asr,
    /// Rotate right with extend
    Rrx,
    /// Rotate right
    Ror,
}
