use crate::arch::{Arch, ArchError, ParseError};
use crate::elf_util::Variable;
use crate::general_assembly::instruction::{CycleCount, Instruction, Operation};
use crate::general_assembly::project::{PCHook, RegisterReadHook, RegisterWriteHook};
use crate::general_assembly::run_config::RunConfig;
use crate::general_assembly::state::GAState;
use crate::general_assembly::translator::Translatable;

use dissarmv7::decoder::*;
use dissarmv7::{prelude::*,ParseSingle};
use regex::Regex;

/// Type level denotation for the Armv7-EM ISA.
#[derive(Debug)]
pub struct ArmV7EM {}

impl Default for ArmV7EM {
    fn default() -> Self {
        Self {}
    }
}

impl Arch for ArmV7EM {
    fn add_hooks(&self, cfg: &mut RunConfig) {
        let symbolic_sized = |state: &mut GAState| {
            let value_ptr = state.get_register("R0".to_owned())?;
            let size = state.get_register("R1".to_owned())?.get_constant().unwrap() * 8;
            let name = "any".to_owned() + &state.marked_symbolic.len().to_string();
            let symb_value = state.ctx.unconstrained(size as u32, &name);
            state.marked_symbolic.push(Variable {
                name: Some(name),
                value: symb_value.clone(),
                ty: crate::elf_util::ExpressionType::Integer(size as usize),
            });
            state.memory.write(&value_ptr, symb_value)?;

            let lr = state.get_register("LR".to_owned())?;
            state.set_register("PC".to_owned(), lr)?;
            Ok(())
        };

        cfg.pc_hooks.push((
            Regex::new(r"^symbolic_size<.+>$").unwrap(),
            PCHook::Intrinsic(symbolic_sized),
        ));

        let read_pc: RegisterReadHook = |state| {
            let two = state.ctx.from_u64(1, 32);
            let pc = state.get_register("PC".to_owned()).unwrap();
            Ok(pc.add(&two))
        };

        let write_pc: RegisterWriteHook = |state, value| state.set_register("PC".to_owned(), value);

        cfg.register_read_hooks.push(("PC+".to_owned(), read_pc));
        cfg.register_write_hooks.push(("PC+".to_owned(), write_pc));
    }



    fn translate(&self, buff: &'static [u8]) -> Result<Instruction, ArchError> {
        let mut buff: dissarmv7::buffer::PeekableBuffer<u8, _> =
            buff.iter().cloned().to_owned().into();
        let instr = Thumb::parse_single(&mut buff)
            .map_err(|e| ArchError::ParsingError(e.into()))?;

        println!("{:?}", instr);
        let ops: Vec<Operation> = instr.1.convert();
        println!("Ops : {ops:?}");
        Ok(Instruction {
            instruction_size: instr.0 as u32,
            operations: ops,
            max_cycle: CycleCount::Value(1),
        })
        // let parsed = self.parse(buff)?;
        // println!("{}", parsed);
        // todo!();
    }
}

impl ArmV7EM {
    fn parse(&self, buff: &'static [u8]) -> Result<Thumb, ArchError> {
        // Should be close to constant time, need to look in to it further
        // TODO! Look in to this further
        todo!()
    }
}

// impl From<dissarmv7::ArchError> for ParseError {
//     fn from(value: dissarmv7::ArchError) -> Self {
//         match value {
//             dissarmv7::ArchError::InvalidCondition => ParseError::InvalidCondition,
//             dissarmv7::ArchError::InvalidRegister(_) => ParseError::InvalidRegister,
//             dissarmv7::ArchError::InvalidField(_) => ParseError::InvalidInstruction,
//         }
//     }
// }
impl From<dissarmv7::ParseError> for ParseError {
    fn from(value: dissarmv7::ParseError) -> Self {
        println!("Err: {value:?}");
        match value {
            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::ArmV7EM;

    #[test]
    fn test_decode_orr() {
        // let data: &[u8] = &[0x94, 0x02, 0x1e, 0x32];
        // println!("{}", ArmV7EM::default().parse(data).unwrap());
    }
}
