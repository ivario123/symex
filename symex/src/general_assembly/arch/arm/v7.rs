use crate::elf_util::{ExpressionType, Variable};
use crate::general_assembly::arch::{Arch, ArchError, ParseError};
use crate::general_assembly::instruction::Instruction;
use crate::general_assembly::project::{
    MemoryHookAddress, MemoryReadHook, PCHook, RegisterReadHook, RegisterWriteHook,
};
use crate::general_assembly::run_config::RunConfig;
use crate::general_assembly::state::GAState;
use general_assembly::operation::Operation;

use decoder::Convert;
use disarmv7::prelude::Operation as V7Operation;
use disarmv7::prelude::*;
use regex::Regex;

#[rustfmt::skip]
pub mod decoder;
pub mod compare;
pub mod timing;
use compare::LocalEq;

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
                ty: ExpressionType::Integer(size as usize),
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
            let pc = state.get_register("PC".to_owned()).unwrap();
            // let pc = pc.simplify();
            let size = state.current_instruction.clone().unwrap().instruction_size;
            let two = state.ctx.from_u64((size as u64) / 8u64, 32);
            let four = state.ctx.from_u64(4, 32);
            Ok(pc.sub(&two).add(&four).simplify())
        };

        let read_sp: RegisterReadHook = |state| {
            let two = state.ctx.from_u64((!(0b11u32)) as u64, 32);
            let sp = state.get_register("SP".to_owned()).unwrap();
            let sp = sp.simplify();
            Ok(sp.and(&two))
        };

        let write_pc: RegisterWriteHook = |state, value| state.set_register("PC".to_owned(), value);
        let write_sp: RegisterWriteHook = |state, value| {
            state.set_register(
                "SP".to_owned(),
                value, /* .and(&state.ctx.from_u64((!(0b11u32)) as u64, 32)) */
            )
        };

        cfg.register_read_hooks.push(("PC+".to_owned(), read_pc));
        cfg.register_write_hooks.push(("PC+".to_owned(), write_pc));
        cfg.register_read_hooks.push(("SP&".to_owned(), read_sp));
        cfg.register_write_hooks.push(("SP&".to_owned(), write_sp));

        // reset allways done
        let read_reset_done: MemoryReadHook = |state, _addr| {
            let value = state.ctx.from_u64(0xffff_ffff, 32);
            Ok(value)
        };
        cfg.memory_read_hooks
            .push((MemoryHookAddress::Single(0x4000c008), read_reset_done));
    }

    fn translate(&self, buff: &[u8], state: &GAState) -> Result<Instruction, ArchError> {
        let buff2 = buff;
        let v6_instr = armv6_m_instruction_parser::parse(buff2);

        let mut buff: disarmv7::buffer::PeekableBuffer<u8, _> = buff.iter().cloned().into();

        let instr = V7Operation::parse(&mut buff).map_err(|e| ArchError::ParsingError(e.into()))?;
        let timing = Self::cycle_count_m4_core(&instr.1);
        println!("instr : {instr:?}");
        let ops: Vec<Operation> = instr.clone().convert(state.get_in_conditional_block());

        match v6_instr {
            Ok(v6) => {
                v6.equal(&instr);
            }
            _ => {}
        }

        Ok(Instruction {
            instruction_size: instr.0 as u32,
            operations: ops,
            max_cycle: timing,
            memory_access: Self::memory_access(&instr.1),
        })
    }
}

impl From<disarmv7::ParseError> for ParseError {
    fn from(value: disarmv7::ParseError) -> Self {
        println!("Err: {value:?}");
        match value {
            _ => todo!(),
        }
    }
}
