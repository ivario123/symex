#![deny(warnings)]
#![deny(clippy::all)]
#![deny(rustdoc::all)]
// Add exceptions for things that are not error prone.
#![allow(clippy::new_without_default)]
#![allow(clippy::too_many_arguments)]
// TODO: Remove this and add crate level docs
#![allow(rustdoc::missing_crate_level_docs)]

pub mod elf_util;
pub mod general_assembly;
pub mod memory;
//#[cfg(not(feature = "llvm"))]
pub mod run_elf;
#[cfg(feature = "llvm")]
pub mod run_llvm;
pub mod smt;
#[cfg(feature = "llvm")]
pub mod util;
#[cfg(feature = "llvm")]
pub mod vm;
