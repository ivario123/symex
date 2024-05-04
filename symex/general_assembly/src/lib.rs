//! [Symex](../../) General Assembly building blocks.
//!
//! This crate defines the building blocks for the [risc](https://www.arm.com/glossary/risc)
//! symex general assembly language. The instructions are composed of
//! [`Operand`](operand::Operand)s, [`Condition`](condition::Condition)s and
//! [`Shift`](shift::Shift)s composed in to
//! [`Operation`](operation::Operation)s. Which in turn can be composed in to
//! meta instructions that describe more complex instructions.

#![deny(warnings)]
#![deny(clippy::all)]
#![deny(missing_docs)]
#![deny(rustdoc::all)]

pub mod condition;
pub mod operand;
pub mod operation;
pub mod shift;

/// Re-exports the main exports of this crate.
pub mod prelude {
    pub use crate::{
        condition::Condition,
        operand::{DataHalfWord, DataWord, Operand},
        operation::Operation,
        shift::Shift,
    };
}
