//! Defines the supported ARM architectures
//!
//! ## Construction
//!
//! The [`Arm`] struct is used as a middle hand
//! for construction of the different ISAs 
//! supported by this crate and presents 
//! the ISAs as dyn [`Arch`] types.

use super::{Arch, ArchError};
use crate::general_assembly::instruction::Instruction;
use crate::general_assembly::run_config::RunConfig;
use crate::general_assembly::translator::{Hookable, Translatable};
use object::{File, Object, ObjectSection};

/// Type level abstraction that serves as a constructor 
///
/// This abstraction only servers as a constructor for the 
/// different ARM instruction sets supported by this crate.
#[derive(Debug)]
pub struct Arm {}

/// Type level denotation for the Armv7-EM ISA.
#[derive(Debug)]
pub struct ArmV7EM {}
/// Type level denotation for the Armv7-EM ISA.
#[derive(Debug)]
pub struct ArmV6EM {}


#[non_exhaustive]
#[allow(dead_code)] // This is temporary and will be removed when V7 is supported
enum ArmIsa {
    ArmV6EM,
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
    let f_cpu_arch = match data.get(6 * 4 - 1) {
        Some(el) => Ok(el),
        None => Err(ArchError::MalformedSection),
    }?;

    match f_cpu_arch {
        12 => Ok(ArmIsa::ArmV6EM),
        // 13 => Ok(ArmIsa::ArmV7EM),
        _ => Err(ArchError::UnsuportedArchitechture),
    }
}

impl Arm {
    pub fn try_from(file: &File) -> Result<Box<dyn Arch>, ArchError> {
        let f = match file {
            File::Elf32(f) => Ok(f),
            _ => Err(ArchError::IncorrectFileType),
        }?;
        // Find the attributes section
        for section in f.sections() {
            let name = match section.name() {
                Ok(n) => n,
                Err(_) => continue,
            };
            if name == ".ARM.attributes" {
                let isa = arm_isa(&section)?;
                return match isa {
                    ArmIsa::ArmV6EM => Ok(Box::new(ArmV6EM {})),
                    ArmIsa::ArmV7EM => Ok(Box::new(ArmV7EM {})),
                };
            }
        }
        return Err(ArchError::MissingSection(".ARM.attributes"));
    }
}

impl Arch for ArmV6EM {
    fn add_hooks(&self, cfg: &mut RunConfig) {
        armv6_m_instruction_parser::instructons::Instruction::add_hooks(cfg)
    }
    fn translate(&self, buff: &[u8]) -> Result<Instruction, ArchError> {
        let ret = armv6_m_instruction_parser::parse(buff).map_err(|e| ArchError::ParsingError(format!("{:?}",e)))?;
        Ok(ret.translate())
    }
}

impl Arch for ArmV7EM {
    fn add_hooks(&self, _cfg: &mut RunConfig) {
        todo!()
    }

    fn translate(&self, _buff: &'static [u8]) -> Result<Instruction, ArchError> {
        todo!()
    }
}
