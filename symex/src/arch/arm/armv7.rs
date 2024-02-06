use crate::arch::{Arch, ArchError};
use crate::general_assembly::instruction::Instruction;
use crate::general_assembly::run_config::RunConfig;

use yaxpeax_arch::{
    AddressDisplay, Arch as YaxArch, Decoder, Reader as YaxReader, ReaderBuilder, U8Reader,
};
use yaxpeax_arm::armv7::{ARMv7 as YaxArmV7, Instruction as InstructionV7,InstDecoder};

/// Type level denotation for the Armv7-EM ISA.
#[derive(Debug)]
pub struct ArmV7EM {
    decoder : InstDecoder
}

impl Default for ArmV7EM {
    fn default() -> Self {
        Self {
            decoder:InstDecoder::default()
        }
    }
}

impl Arch for ArmV7EM {
    fn add_hooks(&self, _cfg: &mut RunConfig) {}

    fn translate(&self, _buff: &'static [u8]) -> Result<Instruction, ArchError> {
        todo!()
    }
}

impl ArmV7EM {
    fn parse(&self, buff: &'static [u8]) -> Result<InstructionV7, ArchError> {
        let mut reader = ReaderBuilder::<u32, u8>::read_from(buff);

        let decode_res = self.decoder.decode(&mut reader);
        match decode_res {
            Ok(inst) => Ok(inst),
            Err(_e) => {
                todo!()
            }
        }
    }
}
