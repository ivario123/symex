use crate::arch::{Arch, ArchError};
use crate::general_assembly::instruction::Instruction;
use crate::general_assembly::run_config::RunConfig;

/// Type level denotation for the Armv7-EM ISA.
#[derive(Debug)]
pub struct ArmV7EM {}

impl Arch for ArmV7EM {
    fn add_hooks(&self, _cfg: &mut RunConfig) {
        todo!()
    }

    fn translate(&self, _buff: &'static [u8]) -> Result<Instruction, ArchError> {
        todo!()
    }
}
