use std::fmt::Display;

use armv6_m_instruction_parser::instructons::Operation as V6Operation;
use decoder::Convert;
use disarmv7::prelude::{Operation as V7Operation, *};
use general_assembly::operation::Operation;
use regex::Regex;

use crate::{
    elf_util::{ExpressionType, Variable},
    general_assembly::{
        arch::{Arch, ArchError, ParseError},
        instruction::Instruction,
        project::{MemoryHookAddress, MemoryReadHook, PCHook, RegisterReadHook, RegisterWriteHook},
        run_config::RunConfig,
        state::GAState,
    },
};

#[rustfmt::skip]
pub mod decoder;
pub mod compare;
#[cfg(test)]
pub mod test;
pub mod timing;

use self::compare::LocalInto;

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

    fn translate(&self, buff: &[u8], state: &GAState) -> Result<Instruction, ArchError> {
        let mut buff: disarmv7::buffer::PeekableBuffer<u8, _> = buff.iter().cloned().into();

        let instr = V7Operation::parse(&mut buff).map_err(|e| ArchError::ParsingError(e.into()))?;
        let timing = Self::cycle_count_m4_core(&instr.1);
        let ops: Vec<Operation> = instr.clone().convert(state.get_in_conditional_block());

        Ok(Instruction {
            instruction_size: instr.0 as u32,
            operations: ops,
            max_cycle: timing,
            memory_access: Self::memory_access(&instr.1),
        })
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
            _ => todo!(),
        }
    }
}

impl LocalInto<V7Operation> for V6Operation {
    #[allow(unused_variables)]
    fn into(self) -> V7Operation {
        match self {
            V6Operation::ADCReg { m, n, d } => operation::AdcRegister::builder()
                .set_rm(m.local_into())
                .set_s(Some(SetFlags::InITBlock(false)))
                .set_rn(n.local_into())
                .set_rd(Some(d.local_into()))
                .set_shift(None)
                .complete()
                .into(),
            V6Operation::ADDImm { imm, n, d } => operation::AddImmediate::builder()
                .set_s(Some(SetFlags::InITBlock(false)))
                .set_rd(d.into_option())
                .set_rn(n.local_into())
                .set_imm(imm)
                .complete()
                .into(),
            V6Operation::ADDReg { m, n, d } => operation::AddRegister::builder()
                .set_rm(m.local_into())
                .set_rd(d.into_option())
                .set_rn(n.local_into())
                .set_s(Some(SetFlags::InITBlock(false)))
                .set_shift(None)
                .complete()
                .into(),
            V6Operation::ADDImmSP { d, imm } => operation::AddSPImmediate::builder()
                .set_rd(d.into_option())
                .set_s(Some(false))
                .set_imm(imm)
                .complete()
                .into(),
            V6Operation::ADDRegSP { d, m } => operation::AddSPRegister::builder()
                .set_s(Some(false))
                .set_rd(d.into_option())
                .set_rm(m.local_into())
                .set_shift(None)
                .complete()
                .into(),
            V6Operation::ADR { d, imm } => operation::Adr::builder()
                .set_rd(d.local_into())
                .set_add(true)
                .set_imm(imm)
                .complete()
                .into(),
            V6Operation::ANDReg { m, dn } => operation::AndRegister::builder()
                .set_s(Some(SetFlags::InITBlock(false)))
                .set_rd(dn.clone().into_option())
                .set_rn(dn.local_into())
                .set_rm(m.local_into())
                .set_shift(None)
                .complete()
                .into(),
            V6Operation::ASRImm { imm, m, d } => operation::AsrImmediate::builder()
                .set_s(Some(SetFlags::InITBlock(false)))
                .set_rd(d.local_into())
                .set_rm(m.local_into())
                .set_imm(imm)
                .complete()
                .into(),
            V6Operation::ASRReg { m, dn } => operation::AsrRegister::builder()
                .set_rm(m.local_into())
                .set_s(Some(SetFlags::InITBlock(false)))
                .set_rd(dn.clone().local_into())
                .set_rn(dn.local_into())
                .complete()
                .into(),
            V6Operation::B { cond, imm } => operation::B::builder()
                .set_condition(cond.local_into())
                .set_imm(imm)
                .complete()
                .into(),
            V6Operation::BICReg { m, dn } => operation::BicRegister::builder()
                .set_s(Some(SetFlags::InITBlock(false)))
                .set_rd(dn.clone().into_option())
                .set_rn(dn.local_into())
                .set_rm(m.local_into())
                .set_shift(None)
                .complete()
                .into(),
            V6Operation::BKPT { imm } => operation::Bkpt::builder().set_imm(imm).complete().into(),
            V6Operation::BL { imm } => operation::Bl::builder().set_imm(imm).complete().into(),
            V6Operation::BLXReg { m } => operation::Blx::builder()
                .set_rm(m.local_into())
                .complete()
                .into(),
            V6Operation::BX { m } => operation::Bx::builder()
                .set_rm(m.local_into())
                .complete()
                .into(),
            V6Operation::CMNReg { m, n } => operation::CmnRegister::builder()
                .set_rn(n.local_into())
                .set_rm(m.local_into())
                .set_shift(None)
                .complete()
                .into(),
            V6Operation::CMPImm { n, imm } => operation::CmpImmediate::builder()
                .set_rn(n.local_into())
                .set_imm(imm)
                .complete()
                .into(),
            V6Operation::CMPReg { m, n } => operation::CmpRegister::builder()
                .set_rn(n.local_into())
                .set_rm(m.local_into())
                .set_shift(None)
                .complete()
                .into(),
            V6Operation::CPS { im } => todo!(),
            V6Operation::CPY => todo!(),
            V6Operation::DMB { option } => todo!(),
            V6Operation::DSB { option } => todo!(),
            V6Operation::EORReg { m, dn } => operation::EorRegister::builder()
                .set_s(Some(SetFlags::InITBlock(false)))
                .set_rd(dn.clone().into_option())
                .set_rn(dn.local_into())
                .set_rm(m.local_into())
                .set_shift(None)
                .complete()
                .into(),
            V6Operation::ISB { option } => todo!(),
            V6Operation::LDM { n, reg_list } => operation::Ldm::builder()
                .set_w(None)
                .set_rn(n.local_into())
                .set_registers(RegisterList {
                    registers: reg_list.iter().map(|el| el.local_into()).collect(),
                })
                .complete()
                .into(),
            V6Operation::LDRImm { imm, n, t } => operation::LdrImmediate::builder()
                .set_w(None)
                .set_add(true)
                .set_index(true)
                .set_rt(t.local_into())
                .set_rn(n.local_into())
                .set_imm(imm)
                .complete()
                .into(),
            V6Operation::LDRLiteral { t, imm } => operation::LdrLiteral::builder()
                .set_rt(t.local_into())
                .set_add(true)
                .set_imm(imm)
                .complete()
                .into(),
            V6Operation::LDRReg { m, n, t } => operation::LdrRegister::builder()
                .set_w(None)
                .set_rt(t.local_into())
                .set_rn(n.local_into())
                .set_rm(m.local_into())
                .set_shift(None)
                .complete()
                .into(),
            V6Operation::LDRBImm { imm, n, t } => todo!(),
            V6Operation::LDRBReg { m, n, t } => todo!(),
            V6Operation::LDRHImm { imm, n, t } => todo!(),
            V6Operation::LDRHReg { m, n, t } => todo!(),
            V6Operation::LDRSBReg { m, n, t } => todo!(),
            V6Operation::LDRSH { m, n, t } => todo!(),
            V6Operation::LSLImm { imm, m, d } => todo!(),
            V6Operation::LSLReg { m, dn } => todo!(),
            V6Operation::LSRImm { imm, m, d } => todo!(),
            V6Operation::LSRReg { m, dn } => todo!(),
            V6Operation::MOVImm { d, imm } => todo!(),
            V6Operation::MOVReg { m, d, set_flags } => todo!(),
            V6Operation::MRS { d, sysm } => todo!(),
            V6Operation::MSRReg { n, sysm } => todo!(),
            V6Operation::MUL { n, dm } => todo!(),
            V6Operation::MVNReg { m, d } => todo!(),
            V6Operation::NOP => todo!(),
            V6Operation::ORRReg { m, dn } => todo!(),
            V6Operation::POP { reg_list } => todo!(),
            V6Operation::PUSH { reg_list } => todo!(),
            V6Operation::REV { m, d } => todo!(),
            V6Operation::REV16 { m, d } => todo!(),
            V6Operation::REVSH { m, d } => todo!(),
            V6Operation::RORReg { m, dn } => todo!(),
            V6Operation::RSBImm { n, d } => todo!(),
            V6Operation::SBCReg { m, dn } => todo!(),
            V6Operation::SEV => todo!(),
            V6Operation::STM { n, reg_list } => todo!(),
            V6Operation::STRImm { imm, n, t } => todo!(),
            V6Operation::STRReg { m, n, t } => todo!(),
            V6Operation::STRBImm { imm, n, t } => todo!(),
            V6Operation::STRBReg { m, n, t } => todo!(),
            V6Operation::STRHImm { imm, n, t } => todo!(),
            V6Operation::STRHReg { m, n, t } => todo!(),
            V6Operation::SUBImm { imm, n, d } => todo!(),
            V6Operation::SUBReg { m, n, d } => todo!(),
            V6Operation::SUBImmSP { imm } => todo!(),
            V6Operation::SVC { imm } => todo!(),
            V6Operation::SXTB { m, d } => todo!(),
            V6Operation::SXTH { m, d } => todo!(),
            V6Operation::TSTReg { m, n } => todo!(),
            V6Operation::UDF { imm } => todo!(),
            V6Operation::UXTB { m, d } => todo!(),
            V6Operation::UXTH { m, d } => todo!(),
            V6Operation::WFE => todo!(),
            V6Operation::WFI => todo!(),
            V6Operation::YIELD => todo!(),
        }
    }
}
