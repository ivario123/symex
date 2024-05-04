//! Struct to configure the symbolic execution.
//! Here various types of custom hooks can be added to being able to simulate specific
//! setups in the symbolic execution. All hooks can be used to exchange generic functionality
//! with a provided function to carry out case specific functionality.
//!
//! Writing a hook function can meaningfully alter how the symbolic execution is carried out.
//! Therefore it is advised that one familiarizes oneself with the inner workings of Symex
//! executor before writing a hook function.

use regex::Regex;

use super::project::{
    MemoryHookAddress, MemoryReadHook, MemoryWriteHook, PCHook, RegisterReadHook, RegisterWriteHook,
};

/// Configures a symbolic execution run.
pub struct RunConfig {
    /// Indicate if the result of a completed path should be printed out or not.
    pub show_path_results: bool,

    /// Hooks here will be carried out instead of a instruction at a specified address or addresses.
    /// This address (or addresses) is determined by finding all subprogram items in the dwarf data that matches the here provided regular expression and taking the starting address from these.
    pub pc_hooks: Vec<(Regex, PCHook)>,

    /// A register read hook will run a function instead of reading from a specified register.
    /// There can only be one hook on a single register.
    pub register_read_hooks: Vec<(String, RegisterReadHook)>,

    /// A register write hook will run a function instead of writing to a specified register.
    /// There can only be one hook on a single register.
    pub register_write_hooks: Vec<(String, RegisterWriteHook)>,

    /// A memory write hook will run a function instead of writing to a single address or range of addresses.
    /// There can only be one hook on a single address but may be multiple on a range but only one hook will be run.
    /// The hook that will run on multiple possible matches is the hook for the matching single address if it exist
    /// otherwise the first matching range will be executed.
    /// As it is not guaranteed that the order is preserved it is recommended to ensure that there are no overlapping ranges.
    pub memory_write_hooks: Vec<(MemoryHookAddress, MemoryWriteHook)>,

    /// A memory read hook will run a function instead of read to a single address or range of addresses.
    /// There can only be one hook on a single address but may be multiple on a range but only one hook will be run.
    /// The hook that will run on multiple possible matches is the hook for the matching single address if it exist
    /// otherwise the first matching range will be executed.
    /// As it is not guaranteed that the order is preserved it is recommended to ensure that there are no overlapping ranges.
    pub memory_read_hooks: Vec<(MemoryHookAddress, MemoryReadHook)>,
}
