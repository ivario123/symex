//! Defines all types of shifts that are valid in the [`Symex`](../../../)
//! General Assembly language.

#[derive(Debug, Clone)]
/// Enumerates all of the shift types defined in the [`Symex`](../../../)
/// General Assembly language.
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
