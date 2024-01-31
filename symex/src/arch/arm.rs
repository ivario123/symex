//! Defines common interface implementations for the ARM architecture

use super::{Arch, ArchError};
use crate::general_assembly::instruction::Instruction;
use crate::general_assembly::run_config::RunConfig;
use crate::general_assembly::translator::{Hookable, Translatable};
use object::{File, Object, ObjectSection};

#[non_exhaustive]
/// Enumerates all of the supported Arm instruction sets
pub enum ArmIsa {
    ArmV6EM,
    ArmV7EM,
}

/// Type level denotation for the Armv7-EM ISA.
#[derive(Debug)]
pub struct ArmV7EM {}
/// Type level denotation for the Armv7-EM ISA.
#[derive(Debug)]
pub struct ArmV6EM {}

fn arm_isa<'a, T: ObjectSection<'a>>(section: &T) -> Result<ArmIsa, ArchError> {
    let data = section.data().map_err(|_| ArchError::MalformedSection)?;
    // Magic extraction,
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

impl ArmIsa {
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
        // This jumping around is purely due to lifetimes. The lifetime of buff needs to be >=
        // 'static due to the error type for parse, if it were an enum this would not be a problem.
        let ret:Result<armv6_m_instruction_parser::instructons::Instruction,_> =armv6_m_instruction_parser::parse(buff);
        if ret.is_ok() {
           return Ok(ret.unwrap().translate())
        }
        Err( ArchError::MalformedInstruction)
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
