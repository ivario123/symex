pub mod decoder;
use crate::arch::{Arch, ArchError};
use crate::general_assembly::instruction::{CycleCount, Instruction, Operation};
use crate::general_assembly::run_config::RunConfig;
use crate::general_assembly::translator::Translatable;

use dissarmv7::prelude::*;
use yaxpeax_arch::{
    AddressDisplay, Arch as YaxArch, Decoder, Reader as YaxReader, ReaderBuilder, U8Reader,
};
use yaxpeax_arm::armv7::{ARMv7 as YaxArmV7, InstDecoder, Instruction as InstructionV7};

/// Type level denotation for the Armv7-EM ISA.
#[derive(Debug)]
pub struct ArmV7EM {}

impl Default for ArmV7EM {
    fn default() -> Self {
        Self {}
    }
}

impl Arch for ArmV7EM {
    fn add_hooks(&self, _cfg: &mut RunConfig) {}

    fn translate(&self, buff: &'static [u8]) -> Result<Instruction, ArchError> {
        let mut buff: dissarmv7::buffer::PeekableBuffer<u8, _> =
            buff.iter().cloned().to_owned().into();
        let instr = dissarmv7::ASM::parse_exact::<_,1>(&mut buff);
        println!("{:?}", instr);
        return Ok(Instruction {
            instruction_size: 16,
            operations: vec![Operation::Nop],
            max_cycle: CycleCount::Value(1),
        });
        // let parsed = self.parse(buff)?;
        // println!("{}", parsed);
        // todo!();
    }
}

impl ArmV7EM {
    fn parse(&self, buff: &'static [u8]) -> Result<InstructionV7, ArchError> {
        // Should be close to constant time, need to look in to it further
        // TODO! Look in to this further
        todo!()
    }
}

#[cfg(test)]
mod test {
    use super::ArmV7EM;

    #[test]
    fn test_decode_orr() {
        let data: &[u8] = &[0x94, 0x02, 0x1e, 0x32];
        println!("{}", ArmV7EM::default().parse(data).unwrap());
    }
}
