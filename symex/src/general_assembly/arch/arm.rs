//! Defines the supported ARM architectures
//!
//! ## Construction
//!
//! The [`Arm`] struct is used as a middle hand
//! for construction of the different ISAs
//! supported by this crate and presents
//! the ISAs as dyn [`Arch`] types.
pub mod v6;
pub mod v7;

use std::fmt::Display;

use object::{File, Object, ObjectSection};
use v6::ArmV6M;
use v7::ArmV7EM;

use super::{Arch, ArchError, Family};

/// Type level abstraction that serves as a constructor
///
/// This abstraction only servers as a constructor for the
/// different ARM instruction sets supported by this crate.
#[derive(Debug)]
pub struct Arm {}

impl Family for Arm {
    /// Tries to determine what ARM ISA the [`File`] is compiled for.
    ///
    /// Expects an elf file with corresponding
    /// .ARM.attributes section which provides the needed information
    /// about the compilation target.
    fn try_from(file: &File) -> Result<Box<dyn Arch>, ArchError> {
        let f = match file {
            File::Elf32(f) => Ok(f),
            _ => Err(ArchError::IncorrectFileType),
        }?;
        let section = match f.section_by_name(".ARM.attributes") {
            Some(section) => Ok(section),
            None => Err(ArchError::MissingSection(".ARM.attributes")),
        }?;
        let isa = arm_isa(&section)?;
        match isa {
            ArmIsa::ArmV6M => Ok(Box::new(ArmV6M {})),
            ArmIsa::ArmV7EM => Ok(Box::new(ArmV7EM {})),
        }
    }
}

impl Display for Arm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Generic ARM architecture")
    }
}

#[non_exhaustive]
#[allow(dead_code)]
enum ArmIsa {
    ArmV6M,
    ArmV7EM,
}

fn arm_isa<'a, T: ObjectSection<'a>>(section: &T) -> Result<ArmIsa, ArchError> {
    let data = section.data().map_err(|_| ArchError::MalformedSection)?;
    // Magic extraction
    //
    // the index here is from
    // https://github.com/ARM-software/abi-aa/blob/main/addenda32/addenda32.rst
    //
    // so are the f_cpu_arch values
    //
    // This offset might be a bit hacky
    let f_cpu_arch = match data.get(6 * 4 - 1) {
        Some(el) => Ok(el),
        None => Err(ArchError::MalformedSection),
    }?;

    match f_cpu_arch {
        // Cortex-m3, this should really be Arvm7M.
        10 => Ok(ArmIsa::ArmV7EM),

        12 => Ok(ArmIsa::ArmV6M),

        // Cortex-m4
        13 => Ok(ArmIsa::ArmV7EM),

        _ => Err(ArchError::UnsuportedArchitechture),
    }
}
