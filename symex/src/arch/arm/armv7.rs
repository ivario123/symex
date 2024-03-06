use crate::arch::{Arch, ArchError, ParseError};
use crate::elf_util::Variable;
use crate::general_assembly::instruction::{CycleCount, Instruction, Operation};
use crate::general_assembly::project::{PCHook, RegisterReadHook, RegisterWriteHook};
use crate::general_assembly::run_config::RunConfig;
use crate::general_assembly::state::GAState;
use crate::general_assembly::translator::Translatable;

use dissarmv7::decoder::*;
use dissarmv7::{prelude::*, ParseSingle};
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
        let instr =
            Thumb::parse_single(&mut buff).map_err(|e| ArchError::ParsingError(e.into()))?;
        println!("instr : {instr:?}");
        let ops: Vec<Operation> = instr.1.clone().convert();
        Ok(Instruction {
            instruction_size: instr.0 as u32,
            operations: ops,
            max_cycle: Self::cycle_count_m4_core(&instr.1),
        })
    }
}

impl ArmV7EM {
    fn cycle_count_m4_core(instr: &Thumb) -> CycleCount {
        match instr {
            Thumb::AdcImmediate(_)
            | Thumb::AdcRegister(_)
            | Thumb::AddImmediate(_)
            | Thumb::AddRegister(_)
            | Thumb::AddSPImmediate(_) => CycleCount::Value(1),
            // 1 + P
            // TODO! validate this
            Thumb::AddSPRegister(_) => CycleCount::Value(1 + 3),
            Thumb::Adr(_) => CycleCount::Value(1),
            Thumb::AndImmediate(_) | Thumb::AndRegister(_) => CycleCount::Value(1),
            Thumb::AsrImmediate(_) | Thumb::AsrRegister(_) => CycleCount::Value(1),
            Thumb::B(_) => {
                let counter = |state: &GAState| match state.get_has_jumped() {
                    true => 1 + 3,
                    false => 1,
                };
                CycleCount::Function(counter)
            }
            Thumb::Bfc(_) => CycleCount::Value(1),
            Thumb::Bfi(_) => CycleCount::Value(1),
            Thumb::BicImmediate(_) | Thumb::BicRegister(_) => CycleCount::Value(1),
            Thumb::Bkpt(_) => todo!("This requires a model of the debug host"),
            Thumb::Bl(_) => CycleCount::Value(1 + 3),
            Thumb::Blx(_) => CycleCount::Value(1 + 3),
            Thumb::Bx(_) => CycleCount::Value(1 + 3),
            Thumb::Cbz(_) => {
                let counter = |state: &GAState| match state.get_has_jumped() {
                    true => 1 + 3,
                    false => 1,
                };
                CycleCount::Function(counter)
            }
            Thumb::Clrex(_) => {
                let counter = |state: &GAState| match state.get_has_jumped() {
                    true => 1 + 3,
                    false => 1,
                };
                CycleCount::Function(counter)
            }
            Thumb::Clz(_) => CycleCount::Value(1),
            Thumb::CmnImmediate(_) | Thumb::CmnRegister(_) => CycleCount::Value(1),
            Thumb::CmpImmediate(_) | Thumb::CmpRegister(_) => CycleCount::Value(1),
            Thumb::Cps(_) => CycleCount::Value(2),
            Thumb::Dbg(_) => CycleCount::Value(1),
            Thumb::Dmb(_) => todo!("This requires a model of barriers"),
            Thumb::Dsb(_) => todo!("This requires a model of barriers"),
            Thumb::EorImmediate(_) | Thumb::EorRegister(_) => CycleCount::Value(1),
            Thumb::Isb(_) => todo!("This requires a model of barriers"),
            Thumb::It(_) => CycleCount::Value(1),
            Thumb::Ldm(ldm) => {
                let pc = ldm.registers.regs.contains(&Register::PC);
                let n = ldm.registers.regs.len();
                let mut count = 1 + n;
                if pc {
                    // TODO! Model pipeline better
                    count += 3;
                }
                CycleCount::Value(count)
            }
            Thumb::Ldmdb(_) => CycleCount::Value(1),
            // TODO! Add in pre load hints
            Thumb::LdrImmediate(el) => match (el.rt, el.rn) {
                (_, Register::PC) => CycleCount::Value(2),
                (Register::PC, _) => CycleCount::Value(2 + 3),
                _ => CycleCount::Value(2),
            },
            Thumb::LdrLiteral(el) => match el.rt {
                Register::PC => CycleCount::Value(2),
                _ => CycleCount::Value(2),
            },
            Thumb::LdrRegister(el) => match (el.rt, el.rn) {
                (Register::PC, Register::PC) => CycleCount::Value(2),
                (Register::PC, _) => CycleCount::Value(2 + 3),
                _ => CycleCount::Value(2),
            },
            Thumb::LdrbImmediate(_) | Thumb::LdrbLiteral(_) | Thumb::LdrbRegister(_) => {
                CycleCount::Value(2)
            }
            Thumb::Ldrbt(_) => CycleCount::Value(2),
            Thumb::LdrdImmediate(_ldrd) => CycleCount::Value(1 + 2),
            Thumb::LdrdLiteral(_) => CycleCount::Value(1 + 2),
            // TODO! This requires a model of semaphores
            Thumb::Ldrex(_) | Thumb::Ldrexb(_) | Thumb::Ldrexh(_) => CycleCount::Value(2),
            Thumb::LdrhImmediate(_) | Thumb::LdrhLiteral(_) | Thumb::LdrhRegister(_) => {
                CycleCount::Value(2)
            }
            Thumb::Ldrht(_) => CycleCount::Value(1),
            Thumb::LdrsbImmediate(_) | Thumb::LdrsbLiteral(_) | Thumb::LdrsbRegister(_) => {
                CycleCount::Value(2)
            }
            Thumb::Ldrsbt(_) => CycleCount::Value(2),
            Thumb::LdrshImmediate(_) | Thumb::LdrshLiteral(_) | Thumb::LdrshRegister(_) => {
                CycleCount::Value(2)
            }
            Thumb::Ldrsht(_) => CycleCount::Value(2),
            Thumb::Ldrt(_) => CycleCount::Value(2),
            Thumb::LslImmediate(_) | Thumb::LslRegister(_) => CycleCount::Value(1),
            Thumb::LsrImmediate(_) | Thumb::LsrRegister(_) => CycleCount::Value(1),
            Thumb::Mla(_) | Thumb::Mls(_) => CycleCount::Value(2),
            Thumb::MovImmediate(_) | Thumb::MovImmediatePlain(_) | Thumb::MovReg(_) => {
                CycleCount::Value(1)
            }
            Thumb::Movt(_) => CycleCount::Value(1),
            Thumb::Mrs(_) => CycleCount::Value(2),
            Thumb::Msr(_) => CycleCount::Value(2),
            Thumb::Mul(_) => CycleCount::Value(1),
            Thumb::MvnImmediate(_) | Thumb::MvnRegister(_) => CycleCount::Value(1),
            Thumb::Nop(_) => CycleCount::Value(1),
            Thumb::OrnImmediate(_) | Thumb::OrnRegister(_) => CycleCount::Value(1),
            Thumb::OrrImmediate(_) | Thumb::OrrRegister(_) => CycleCount::Value(1),
            Thumb::Pkh(_) => CycleCount::Value(1),
            Thumb::PldImmediate(_) => todo!("Add in preload hints"),
            Thumb::PldLiteral(_) => todo!("Add in preload hints"),
            Thumb::PldRegister(_) => todo!("Add in preload hints"),
            Thumb::PliImmediate(_) => todo!("Add in preload hints"),
            Thumb::PliRegister(_) => todo!("Add in preload hints"),
            Thumb::Pop(pop) => {
                let ret = match pop.registers.regs.contains(&Register::PC) {
                    true => 3,
                    _ => 0,
                };
                CycleCount::Value(1 + pop.registers.regs.len() + ret)
            }
            Thumb::Push(push) => CycleCount::Value(1 + push.registers.regs.len()),
            Thumb::Qadd(_) => CycleCount::Value(1),
            Thumb::Qadd16(_) => CycleCount::Value(1),
            Thumb::Qadd8(_) => CycleCount::Value(1),
            Thumb::Qasx(_) => CycleCount::Value(1),
            Thumb::Qdadd(_) => CycleCount::Value(1),
            Thumb::Qdsub(_) => CycleCount::Value(1),
            Thumb::Qsax(_) => CycleCount::Value(1),
            Thumb::Qsub(_) => CycleCount::Value(1),
            Thumb::Qsub16(_) => CycleCount::Value(1),
            Thumb::Qsub8(_) => CycleCount::Value(1),
            Thumb::Rbit(_) => CycleCount::Value(1),
            Thumb::Rev(_) => CycleCount::Value(1),
            Thumb::Rev16(_) => CycleCount::Value(1),
            Thumb::Revsh(_) => CycleCount::Value(1),
            Thumb::RorImmediate(_) | Thumb::RorRegister(_) => CycleCount::Value(1),
            Thumb::Rrx(_) => CycleCount::Value(1),
            Thumb::RsbImmediate(_) | Thumb::RsbRegister(_) => CycleCount::Value(1),
            Thumb::Sadd16(_) => CycleCount::Value(1),
            Thumb::Sadd8(_) => CycleCount::Value(1),
            Thumb::Sasx(_) => CycleCount::Value(1),
            Thumb::SbcImmediate(_) | Thumb::SbcRegister(_) => CycleCount::Value(1),
            Thumb::Sbfx(_) => CycleCount::Value(1),
            // TODO! Add way to find wether or not this is 12 or 2
            Thumb::Sdiv(_) => CycleCount::Value(12),
            Thumb::Sel(_) => CycleCount::Value(1),
            Thumb::Sev(_) => CycleCount::Value(1),
            Thumb::Shadd16(_) => CycleCount::Value(1),
            Thumb::Shadd8(_) => CycleCount::Value(1),
            Thumb::Shasx(_) => CycleCount::Value(1),
            Thumb::Shsax(_) => CycleCount::Value(1),
            Thumb::Shsub16(_) => CycleCount::Value(1),
            Thumb::Shsub8(_) => CycleCount::Value(1),
            Thumb::Smla(_) => CycleCount::Value(1),
            Thumb::Smlad(_) => CycleCount::Value(1),
            Thumb::Smlal(_) => CycleCount::Value(1),
            Thumb::SmlalSelective(_) => CycleCount::Value(1),
            Thumb::Smlald(_) => CycleCount::Value(1),
            Thumb::Smlaw(_) => CycleCount::Value(1),
            Thumb::Smlsd(_) => CycleCount::Value(1),
            Thumb::Smlsld(_) => CycleCount::Value(1),
            Thumb::Smmla(_) => CycleCount::Value(1),
            Thumb::Smmls(_) => CycleCount::Value(1),
            Thumb::Smmul(_) => CycleCount::Value(1),
            Thumb::Smuad(_) => CycleCount::Value(1),
            Thumb::Smul(_) => CycleCount::Value(1),
            Thumb::Smull(_) => CycleCount::Value(1),
            Thumb::Smulw(_) => CycleCount::Value(1),
            Thumb::Smusd(_) => CycleCount::Value(1),
            Thumb::Ssat(_) | Thumb::Ssat16(_) => CycleCount::Value(1),
            Thumb::Ssax(_) => CycleCount::Value(1),
            Thumb::Ssub16(_) => CycleCount::Value(1),
            Thumb::Ssub8(_) => CycleCount::Value(1),
            Thumb::Stm(stm) => CycleCount::Value(1 + stm.registers.regs.len()),
            Thumb::Stmdb(_) => CycleCount::Value(1),
            Thumb::StrImmediate(_) | Thumb::StrRegister(_) => CycleCount::Value(2),
            Thumb::StrbImmediate(_) | Thumb::StrbRegister(_) => CycleCount::Value(2),
            Thumb::Strbt(_) => CycleCount::Value(2),
            Thumb::StrdImmediate(_strd) => CycleCount::Value(1 + 2),
            Thumb::Strex(_) => CycleCount::Value(2),
            Thumb::Strexb(_) => CycleCount::Value(2),
            Thumb::Strexh(_) => CycleCount::Value(2),
            Thumb::StrhImmediate(_) | Thumb::StrhRegister(_) => CycleCount::Value(2),
            Thumb::Strht(_) => CycleCount::Value(2),
            // TODO! Validate B here
            Thumb::Strt(_) => CycleCount::Value(2),
            Thumb::SubImmediate(_) | Thumb::SubRegister(_) => CycleCount::Value(1),
            Thumb::SubSpMinusImmediate(_) => CycleCount::Value(1),
            Thumb::SubSpMinusReg(_) => CycleCount::Value(1),

            Thumb::Sxtab(_) => CycleCount::Value(1),

            Thumb::Sxtab16(_) => CycleCount::Value(1),
            Thumb::Sxtah(_) => CycleCount::Value(1),
            Thumb::Sxtb(_) => CycleCount::Value(1),
            Thumb::Sxtb16(_) => CycleCount::Value(1),
            Thumb::Sxth(_) => CycleCount::Value(1),
            Thumb::Tb(_) => CycleCount::Value(2 + 3),
            // TODO!  The docs do not mention any cycle count for this
            // might be incorret
            Thumb::TeqImmediate(_) | Thumb::TeqRegister(_) => CycleCount::Value(1),
            Thumb::TstImmediate(_) | Thumb::TstRegister(_) => CycleCount::Value(1),
            Thumb::Uadd16(_) => CycleCount::Value(1),
            Thumb::Uadd8(_) => CycleCount::Value(1),
            Thumb::Uasx(_) => CycleCount::Value(1),
            Thumb::Ubfx(_) => CycleCount::Value(1),
            Thumb::Udf(_) => CycleCount::Value(1),
            // TODO! Add way to check if this is 12 or 2
            Thumb::Udiv(_) => CycleCount::Value(12),
            Thumb::Uhadd16(_) => CycleCount::Value(1),
            Thumb::Uhadd8(_) => CycleCount::Value(1),
            Thumb::Uhasx(_) => CycleCount::Value(1),
            Thumb::Uhsax(_) => CycleCount::Value(1),
            Thumb::Uhsub16(_) => CycleCount::Value(1),
            Thumb::Uhsub8(_) => CycleCount::Value(1),
            Thumb::Umaal(_) => CycleCount::Value(1),
            Thumb::Umlal(_) => CycleCount::Value(1),
            Thumb::Umull(_) => CycleCount::Value(1),
            Thumb::Uqadd16(_) => CycleCount::Value(1),
            Thumb::Uqadd8(_) => CycleCount::Value(1),
            Thumb::Uqasx(_) => CycleCount::Value(1),
            Thumb::Uqsax(_) => CycleCount::Value(1),
            Thumb::Uqsub16(_) => CycleCount::Value(1),
            Thumb::Uqsub8(_) => CycleCount::Value(1),
            Thumb::Uqsad8(_) => CycleCount::Value(1),
            Thumb::Usada8(_) => CycleCount::Value(1),
            Thumb::Usad8(_) => CycleCount::Value(1),
            Thumb::Usat(_) | Thumb::Usat16(_) => CycleCount::Value(1),
            Thumb::Usax(_) => CycleCount::Value(1),
            Thumb::Usub16(_) => CycleCount::Value(1),
            Thumb::Usub8(_) => CycleCount::Value(1),
            Thumb::Uxtab(_) => CycleCount::Value(1),
            Thumb::Uxtab16(_) => CycleCount::Value(1),
            Thumb::Uxtah(_) => CycleCount::Value(1),
            Thumb::Uxtb(_) => CycleCount::Value(1),
            Thumb::Uxtb16(_) => CycleCount::Value(1),
            Thumb::Uxth(_) => CycleCount::Value(1),
            Thumb::Wfe(_) => todo!("This requires a model of events"),
            Thumb::Wfi(_) => todo!("This requires a model of interrupts"),

            // This assumes that we have no core running
            Thumb::Yield(_) => CycleCount::Value(1),
            Thumb::Svx(_) => todo!(),
            Thumb::Stc(_) => todo!(),
            Thumb::Mcr(_) => todo!(),
            Thumb::Mrc(_) => todo!(),
            Thumb::Mrrc(_) => todo!(),
            Thumb::Mcrr(_) => todo!(),
            Thumb::Cdp(_) => todo!(),
            Thumb::Ldc(_) => todo!(),
        }
    }
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
