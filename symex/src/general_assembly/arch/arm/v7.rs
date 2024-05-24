use std::{collections::HashSet, fmt::Display};

use decoder::Convert;
use disarmv7::prelude::{Operation as V7Operation, *};
use general_assembly::operation::Operation;
use regex::Regex;

use crate::{
    elf_util::{ExpressionType, Variable},
    general_assembly::{
        arch::{Arch, ArchError, ArchStateExtension, ParseError},
        instruction::Instruction,
        project::{MemoryHookAddress, MemoryReadHook, PCHook, RegisterReadHook, RegisterWriteHook},
        run_config::RunConfig,
        state::GAState,
        Result as SuperResult,
    },
    smt::DExpr,
};

#[rustfmt::skip]
pub mod decoder;
pub mod compare;
#[cfg(test)]
pub mod test;
pub mod timing;

/// Type level denotation for the Armv7-EM ISA.
#[derive(Debug, Default, Clone)]
pub struct ArmV7EM {}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct ArmV7EMStateExt {
    has_jumped: bool,
    first_branch_occurance: bool,
    branch_map: HashSet<(u64, u64)>,
    prev_cycle_latch: usize,
    pred_horizon: usize,
    prev_instr: Option<V7Operation>,
}

impl Display for ArmV7EMStateExt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ArmV7EM state extensions")
    }
}

impl ArchStateExtension for ArmV7EMStateExt {
    fn as_any(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

impl ArmV7EMStateExt {
    pub fn set_is_predictable(&mut self, pred: bool) {
        self.pred_horizon = match pred {
            true => (self.pred_horizon + 1).min(1),
            false => 0,
        };
    }

    pub fn get_p(&self) -> usize {
        3 - self.pred_horizon
    }
}

impl Arch for ArmV7EM {
    fn ext(&self) -> Box<dyn ArchStateExtension> {
        Box::new(ArmV7EMStateExt {
            has_jumped: false,
            first_branch_occurance: false,
            branch_map: HashSet::new(),
            prev_cycle_latch: 0,
            pred_horizon: 0,
            prev_instr: None,
        })
    }

    fn add_hooks(&self, cfg: &mut RunConfig) {
        let branch_update = |state: &mut GAState, value: DExpr| -> SuperResult<()> {
            // Safe to assume that PC exists.
            let pc = match value.get_constant() {
                Some(val) => val,
                None => {
                    return state.set_register_bypass_hooks("PC".to_owned(), value);
                }
            };
            let old_pc = state.last_pc;

            let diff = match &state.current_instruction {
                Some(instr) => instr.instruction_size / 8,
                None => return state.set_register_bypass_hooks("PC".to_owned(), value),
            };

            let ext = state.as_ext::<ArmV7EMStateExt>();

            if pc > old_pc && (pc - old_pc == diff.into()) {
                ext.first_branch_occurance = false;
                ext.has_jumped = false;
                return state.set_register_bypass_hooks("PC".to_owned(), value);
            }

            ext.has_jumped = true;

            let did_not_exist = ext.branch_map.insert((old_pc, pc));

            if did_not_exist {
                ext.first_branch_occurance = true;
                // println!("First time {:?} -> {:?}", old_pc, pc);
            } else {
                ext.first_branch_occurance = false;
            }
            // Bypass the hooks as we would loop for ever otherwise.
            state.set_register_bypass_hooks("PC".to_owned(), value)
        };

        cfg.register_write_hooks
            .push(("PC".to_owned(), branch_update));

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

        // §B1.4 Specifies that R[15] => Addr(Current instruction) + 4
        //
        // This can be translated in to
        //
        // PC - Size(prev instruction) / 8 + 4
        // as PC points to the next instruction, we
        //
        //
        // Or we can simply take the previous PC + 4.
        let read_pc: RegisterReadHook = |state| {
            let new_pc = state
                .ctx
                .from_u64(state.last_pc + 4, state.project.get_word_size())
                .simplify();
            Ok(new_pc)
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
                value.and(&state.ctx.from_u64((!(0b11u32)) as u64, 32)),
            )?;
            let sp = state.get_register("SP".to_owned()).unwrap();
            let sp = sp.simplify();
            state.set_register("SP".to_owned(), sp)
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

    fn translate(&self, buff: &[u8], state: &mut GAState) -> Result<Instruction, ArchError> {
        let mut buff: disarmv7::buffer::PeekableBuffer<u8, _> = buff.iter().cloned().into();

        let instr = V7Operation::parse(&mut buff).map_err(|e| ArchError::ParsingError(e.into()))?;
        let timing = Self::cycle_count_m4_core(&instr.1, state);
        let ops: Vec<Operation> = instr.clone().convert(state.get_in_conditional_block());

        Ok(Instruction {
            instruction_size: instr.0 as u32,
            operations: ops,
            max_cycle: timing,
            memory_access: Self::memory_access(&instr.1),
        })
    }

    fn as_any(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

impl Display for ArmV7EM {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ARMv7-M")
    }
}

impl From<disarmv7::ParseError> for ParseError {
    fn from(value: disarmv7::ParseError) -> Self {
        match value {
            disarmv7::ParseError::Undefined => ParseError::InvalidInstruction,
            disarmv7::ParseError::ArchError(aerr) => match aerr {
                disarmv7::prelude::arch::ArchError::InvalidCondition => {
                    ParseError::InvalidCondition
                }
                disarmv7::prelude::arch::ArchError::InvalidRegister(_) => {
                    ParseError::InvalidRegister
                }
                disarmv7::prelude::arch::ArchError::InvalidField(_) => {
                    ParseError::MalfromedInstruction
                }
            },
            disarmv7::ParseError::Unpredictable => ParseError::Unpredictable,
            disarmv7::ParseError::Invalid16Bit(_) | disarmv7::ParseError::Invalid32Bit(_) => {
                ParseError::InvalidInstruction
            }
            disarmv7::ParseError::InvalidField(_) => ParseError::MalfromedInstruction,
            disarmv7::ParseError::Incomplete32Bit => ParseError::InsufficientInput,
            disarmv7::ParseError::InternalError(info) => ParseError::Generic(info),
            disarmv7::ParseError::IncompleteParser => {
                ParseError::Generic("Encountered instruction that is not yet supported.")
            }
            disarmv7::ParseError::InvalidCondition => ParseError::InvalidCondition,
            disarmv7::ParseError::IncompleteProgram => ParseError::InsufficientInput,
            disarmv7::ParseError::InvalidRegister(_) => ParseError::InvalidRegister,
            disarmv7::ParseError::PartiallyParsed(error, _) => (*error).into(),
        }
    }
}
