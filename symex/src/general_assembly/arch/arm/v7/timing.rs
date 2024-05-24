use disarmv7::{
    arch::Condition,
    prelude::{Operation as V7Operation, Register},
};

// use general_assembly::operation::Operation;
use crate::general_assembly::{
    arch::arm::v7::ArmV7EMStateExt,
    instruction::CycleCount,
    state::GAState,
};

impl super::ArmV7EM {
    pub fn memory_access(instr: &V7Operation) -> bool {
        use V7Operation::*;
        match instr {
            AdcImmediate(_) | AdcRegister(_) | AddImmediate(_) | AddRegister(_)
            | AddSPImmediate(_) | AddSPRegister(_) | Adr(_) | AndImmediate(_) | AndRegister(_)
            | AsrImmediate(_) | AsrRegister(_) | B(_) | Bfc(_) | Bfi(_) | BicImmediate(_)
            | BicRegister(_) | Bkpt(_) | Bl(_) | Blx(_) | Bx(_) | Cbz(_) | Clrex(_) | Clz(_)
            | CmnImmediate(_) | CmnRegister(_) | CmpImmediate(_) | CmpRegister(_) | Cps(_)
            | Dbg(_) | Dmb(_) | Dsb(_) | EorImmediate(_) | EorRegister(_) | Isb(_) | It(_) => false,
            Ldm(_) | Ldmdb(_) | LdrImmediate(_) | LdrLiteral(_) | LdrRegister(_)
            | LdrbImmediate(_) | LdrbLiteral(_) | LdrbRegister(_) | Ldrbt(_) | LdrdImmediate(_)
            | LdrdLiteral(_) | Ldrex(_) | Ldrexb(_) | Ldrexh(_) | LdrhImmediate(_)
            | LdrhLiteral(_) | LdrhRegister(_) | Ldrht(_) | LdrsbImmediate(_) | LdrsbLiteral(_)
            | LdrsbRegister(_) | Ldrsbt(_) | LdrshImmediate(_) | LdrshLiteral(_)
            | LdrshRegister(_) | Ldrsht(_) | Ldrt(_) => true,
            LslImmediate(_) | LslRegister(_) | LsrImmediate(_) | LsrRegister(_) => false,
            Mcrr(_) | Mla(_) | Mls(_) | MovImmediate(_) | MovRegister(_) | Movt(_) | Mrrc(_)
            | Mrs(_) | Msr(_) | Mul(_) | MvnImmediate(_) | MvnRegister(_) | Nop(_)
            | OrnImmediate(_) | OrnRegister(_) | OrrImmediate(_) | OrrRegister(_) | Pkh(_) => false,
            PldImmediate(_) | PldLiteral(_) | PldRegister(_) | PliImmediate(_) | PliRegister(_) => {
                true
            }
            Pop(_) => true,
            Push(_) => true,
            Qadd(_) | Qadd16(_) | Qadd8(_) | Qasx(_) | Qdadd(_) | Qdsub(_) | Qsax(_) | Qsub(_)
            | Qsub16(_) | Qsub8(_) | Rbit(_) | Rev(_) | Rev16(_) | Revsh(_) | RorImmediate(_)
            | RorRegister(_) | Rrx(_) | RsbImmediate(_) | RsbRegister(_) | Sadd16(_) | Sadd8(_)
            | Sasx(_) | SbcImmediate(_) | SbcRegister(_) | Sbfx(_) | Sdiv(_) | Sel(_) | Sev(_)
            | Shadd16(_) | Shadd8(_) | Shasx(_) | Shsax(_) | Shsub16(_) | Shsub8(_) | Smla(_)
            | Smlad(_) | Smlal(_) | SmlalSelective(_) | Smlald(_) | Smlaw(_) | Smlsd(_)
            | Smlsld(_) | Smmla(_) | Smmls(_) | Smmul(_) | Smuad(_) | Smul(_) | Smull(_)
            | Smulw(_) | Smusd(_) | Ssat(_) | Ssat16(_) | Ssax(_) | Ssub16(_) | Ssub8(_) => false,
            Stm(_) | Stmdb(_) | StrImmediate(_) | StrRegister(_) | StrbImmediate(_)
            | StrbRegister(_) | Strbt(_) | StrdImmediate(_) | Strex(_) | Strexb(_) | Strexh(_)
            | StrhImmediate(_) | StrhRegister(_) | Strht(_) | Strt(_) => true,
            Stc(_)
            | SubImmediate(_)
            | SubRegister(_)
            | SubSpMinusImmediate(_)
            | SubSpMinusRegister(_)
            | Sxtab(_)
            | Sxtab16(_)
            | Sxtah(_)
            | Sxtb(_)
            | Sxtb16(_)
            | Sxth(_)
            | Tb(_)
            | TeqImmediate(_)
            | TeqRegister(_)
            | TstImmediate(_)
            | TstRegister(_)
            | Uadd16(_)
            | Uadd8(_)
            | Uasx(_)
            | Ubfx(_)
            | Udf(_)
            | Udiv(_)
            | Uhadd16(_)
            | Uhadd8(_)
            | Uhasx(_)
            | Uhsax(_)
            | Uhsub16(_)
            | Uhsub8(_)
            | Umaal(_)
            | Umlal(_)
            | Umull(_)
            | Uqadd16(_)
            | Uqadd8(_)
            | Uqasx(_)
            | Uqsax(_)
            | Uqsub16(_)
            | Uqsub8(_)
            | Uqsad8(_)
            | Usada8(_)
            | Usad8(_)
            | Usat(_)
            | Usat16(_)
            | Usax(_)
            | Usub16(_)
            | Usub8(_)
            | Uxtab(_)
            | Uxtab16(_)
            | Uxtah(_)
            | Uxtb(_)
            | Uxtb16(_)
            | Uxth(_)
            | Wfe(_)
            | Wfi(_)
            | Yield(_)
            | Svc(_)
            | Mcr(_)
            | Mrc(_)
            | Cdp(_)
            | LdcImmediate(_)
            | LdcLiteral(_) => false,
        }
    }

    pub fn trivially_predictable(instr: &V7Operation) -> bool {
        match instr {
            V7Operation::AdcImmediate(add) => add.rd.unwrap_or(add.rn) != Register::PC,
            V7Operation::AdcRegister(add) => add.rd.unwrap_or(add.rn) != Register::PC,
            V7Operation::AddImmediate(add) => add.rd.unwrap_or(add.rn) != Register::PC,
            V7Operation::AddRegister(add) => add.rd.unwrap_or(add.rn) != Register::PC,
            V7Operation::AddSPImmediate(add) => add.rd.unwrap_or(Register::SP) != Register::PC,
            V7Operation::AddSPRegister(add) => add.rd.unwrap_or(Register::SP) != Register::PC,
            V7Operation::Adr(adr) => adr.rd != Register::PC,
            V7Operation::AndImmediate(and) => and.rd.unwrap_or(and.rn) != Register::PC,
            V7Operation::AndRegister(and) => and.rd.unwrap_or(and.rn) != Register::PC,
            V7Operation::AsrImmediate(asr) => asr.rd != Register::PC,
            V7Operation::AsrRegister(asr) => asr.rd != Register::PC,
            V7Operation::B(b) => b.condition == Condition::None,
            V7Operation::Bfc(bf) => bf.rd != Register::PC,
            V7Operation::Bfi(bf) => bf.rd != Register::PC,
            V7Operation::BicImmediate(bic) => bic.rd.unwrap_or(bic.rn) != Register::PC,
            V7Operation::BicRegister(bic) => bic.rd.unwrap_or(bic.rn) != Register::PC,
            V7Operation::Bkpt(_) => true,
            V7Operation::Bl(_bl) => true,
            V7Operation::Blx(_blx) => true,
            V7Operation::Bx(_bx) => true,
            V7Operation::Cbz(_cbz) => false,
            V7Operation::Cdp(_cdp) => true,
            V7Operation::Clrex(_) => todo!(),
            V7Operation::Clz(clz) => clz.rd != Register::PC,
            V7Operation::CmnImmediate(_) => true,
            V7Operation::CmnRegister(_) => true,
            V7Operation::CmpImmediate(_) => true,
            V7Operation::CmpRegister(_) => true,
            V7Operation::Cps(_) => true,
            V7Operation::Dbg(_) => true,
            V7Operation::Dmb(_) => todo!(),
            V7Operation::Dsb(_) => todo!(),
            V7Operation::EorImmediate(eor) => eor.rd.unwrap_or(eor.rn) != Register::PC,
            V7Operation::EorRegister(eor) => eor.rd.unwrap_or(eor.rn) != Register::PC,
            V7Operation::Isb(_) => todo!(),
            V7Operation::It(_) => false,
            V7Operation::Ldm(ldm) => ldm.registers.registers.iter().all(|el| *el != Register::PC),
            V7Operation::Ldmdb(ldm) => ldm.registers.registers.iter().all(|el| *el != Register::PC),
            V7Operation::LdrImmediate(ldr) => ldr.rt != Register::PC,
            V7Operation::LdrLiteral(ldr) => ldr.rt != Register::PC,
            V7Operation::LdrRegister(ldr) => ldr.rt != Register::PC,
            V7Operation::LdrbImmediate(ldr) => ldr.rt != Register::PC,
            V7Operation::LdrbLiteral(ldr) => ldr.rt != Register::PC,
            V7Operation::LdrbRegister(ldr) => ldr.rt != Register::PC,
            V7Operation::Ldrbt(ldr) => ldr.rt != Register::PC,
            V7Operation::LdrdImmediate(ldr) => ldr.rt != Register::PC && ldr.rt2 != Register::PC,
            V7Operation::LdrdLiteral(ldr) => ldr.rt != Register::PC && ldr.rt2 != Register::PC,
            V7Operation::Ldrex(_) => todo!(),
            V7Operation::Ldrexb(_) => todo!(),
            V7Operation::Ldrexh(_) => todo!(),
            V7Operation::LdrhImmediate(ldr) => ldr.rt != Register::PC,
            V7Operation::LdrhLiteral(ldr) => ldr.rt != Register::PC,
            V7Operation::LdrhRegister(ldr) => ldr.rt != Register::PC,
            V7Operation::Ldrht(ldr) => ldr.rt != Register::PC,
            V7Operation::LdrsbImmediate(ldr) => ldr.rt != Register::PC,
            V7Operation::LdrsbLiteral(ldr) => ldr.rt != Register::PC,
            V7Operation::LdrsbRegister(ldr) => ldr.rt != Register::PC,
            V7Operation::Ldrsbt(ldr) => ldr.rt != Register::PC,
            V7Operation::LdrshImmediate(ldr) => ldr.rt != Register::PC,
            V7Operation::LdrshLiteral(ldr) => ldr.rt != Register::PC,
            V7Operation::LdrshRegister(ldr) => ldr.rt != Register::PC,
            V7Operation::Ldrsht(ldr) => ldr.rt != Register::PC,
            V7Operation::Ldrt(ldr) => ldr.rt != Register::PC,
            V7Operation::LdcImmediate(_) => true,
            V7Operation::LdcLiteral(_) => true,
            V7Operation::LslImmediate(lsl) => lsl.rd != Register::PC,
            V7Operation::LslRegister(lsl) => lsl.rd != Register::PC,
            V7Operation::LsrImmediate(lsr) => lsr.rd != Register::PC,
            V7Operation::LsrRegister(lsr) => lsr.rd != Register::PC,
            V7Operation::Mcrr(_) => todo!(),
            V7Operation::Mcr(_) => todo!(),
            V7Operation::Mla(mla) => mla.rd != Register::PC,
            V7Operation::Mls(mls) => mls.rd != Register::PC,
            V7Operation::MovImmediate(mv) => mv.rd != Register::PC,
            V7Operation::MovRegister(mv) => mv.rd != Register::PC,
            V7Operation::Movt(mv) => mv.rd != Register::PC,
            V7Operation::Mrrc(_) => todo!(),
            V7Operation::Mrc(_) => todo!(),
            V7Operation::Mrs(_) => todo!(),
            V7Operation::Msr(_) => todo!(),
            V7Operation::Mul(mul) => mul.rd.unwrap_or(mul.rn) != Register::PC,
            V7Operation::MvnImmediate(mv) => mv.rd != Register::PC,
            V7Operation::MvnRegister(mv) => mv.rd != Register::PC,
            V7Operation::Nop(_) => true,
            V7Operation::OrnImmediate(or) => or.rd.unwrap_or(or.rn) != Register::PC,
            V7Operation::OrnRegister(or) => or.rd.unwrap_or(or.rn) != Register::PC,
            V7Operation::OrrImmediate(or) => or.rd.unwrap_or(or.rn) != Register::PC,
            V7Operation::OrrRegister(or) => or.rd.unwrap_or(or.rn) != Register::PC,
            V7Operation::Pkh(pkh) => pkh.rd.unwrap_or(pkh.rn) != Register::PC,
            V7Operation::PldImmediate(_) => todo!(),
            V7Operation::PldLiteral(_) => todo!(),
            V7Operation::PldRegister(_) => todo!(),
            V7Operation::PliImmediate(_) => todo!(),
            V7Operation::PliRegister(_) => todo!(),
            V7Operation::Pop(pop) => !pop.registers.registers.contains(&Register::PC),
            V7Operation::Push(_) => true,
            V7Operation::Qadd(add) => add.rd.unwrap_or(add.rn) != Register::PC,
            V7Operation::Qadd16(add) => add.rd.unwrap_or(add.rn) != Register::PC,
            V7Operation::Qadd8(add) => add.rd.unwrap_or(add.rn) != Register::PC,
            V7Operation::Qasx(add) => add.rd.unwrap_or(add.rn) != Register::PC,
            V7Operation::Qdadd(add) => add.rd.unwrap_or(add.rn) != Register::PC,
            V7Operation::Qdsub(add) => add.rd.unwrap_or(add.rn) != Register::PC,
            V7Operation::Qsax(add) => add.rd.unwrap_or(add.rn) != Register::PC,
            V7Operation::Qsub(add) => add.rd.unwrap_or(add.rn) != Register::PC,
            V7Operation::Qsub16(add) => add.rd.unwrap_or(add.rn) != Register::PC,
            V7Operation::Qsub8(add) => add.rd.unwrap_or(add.rn) != Register::PC,
            V7Operation::Rbit(r) => r.rd != Register::PC,
            V7Operation::Rev(r) => r.rd != Register::PC,
            V7Operation::Rev16(r) => r.rd != Register::PC,
            V7Operation::Revsh(r) => r.rd != Register::PC,
            V7Operation::RorImmediate(r) => r.rd != Register::PC,
            V7Operation::RorRegister(r) => r.rd != Register::PC,
            V7Operation::Rrx(r) => r.rd != Register::PC,
            V7Operation::RsbImmediate(r) => r.rd.unwrap_or(r.rn) != Register::PC,
            V7Operation::RsbRegister(r) => r.rd.unwrap_or(r.rn) != Register::PC,
            V7Operation::Sadd16(_) => todo!(),
            V7Operation::Sadd8(_) => todo!(),
            V7Operation::Sasx(_) => todo!(),
            V7Operation::SbcImmediate(_) => todo!(),
            V7Operation::SbcRegister(_) => todo!(),
            V7Operation::Sbfx(_) => todo!(),
            V7Operation::Sdiv(_) => todo!(),
            V7Operation::Sel(_) => todo!(),
            V7Operation::Sev(_) => todo!(),
            V7Operation::Svc(_) => todo!(),
            V7Operation::Shadd16(_) => todo!(),
            V7Operation::Shadd8(_) => todo!(),
            V7Operation::Shasx(_) => todo!(),
            V7Operation::Shsax(_) => todo!(),
            V7Operation::Shsub16(_) => todo!(),
            V7Operation::Shsub8(_) => todo!(),
            V7Operation::Smla(_) => todo!(),
            V7Operation::Smlad(_) => todo!(),
            V7Operation::Smlal(_) => todo!(),
            V7Operation::SmlalSelective(_) => todo!(),
            V7Operation::Smlald(_) => todo!(),
            V7Operation::Smlaw(_) => todo!(),
            V7Operation::Smlsd(_) => todo!(),
            V7Operation::Smlsld(_) => todo!(),
            V7Operation::Smmla(_) => todo!(),
            V7Operation::Smmls(_) => todo!(),
            V7Operation::Smmul(_) => todo!(),
            V7Operation::Smuad(_) => todo!(),
            V7Operation::Smul(_) => todo!(),
            V7Operation::Smull(_) => todo!(),
            V7Operation::Smulw(_) => todo!(),
            V7Operation::Smusd(_) => todo!(),
            V7Operation::Ssat(_) => todo!(),
            V7Operation::Ssat16(_) => todo!(),
            V7Operation::Ssax(_) => todo!(),
            V7Operation::Ssub16(_) => todo!(),
            V7Operation::Ssub8(_) => todo!(),
            V7Operation::Stm(_) => true,
            V7Operation::Stmdb(_) => true,
            V7Operation::StrImmediate(_) => true,
            V7Operation::StrRegister(_) => true,
            V7Operation::StrbImmediate(_) => true,
            V7Operation::StrbRegister(_) => true,
            V7Operation::Strbt(_) => true,
            V7Operation::StrdImmediate(_) => true,
            V7Operation::Strex(_) => todo!(),
            V7Operation::Strexb(_) => todo!(),
            V7Operation::Strexh(_) => todo!(),
            V7Operation::StrhImmediate(_) => true,
            V7Operation::StrhRegister(_) => true,
            V7Operation::Strht(_) => true,
            V7Operation::Strt(_) => true,
            V7Operation::SubImmediate(sub) => sub.rd.unwrap_or(sub.rn) != Register::PC,
            V7Operation::SubRegister(sub) => sub.rd.unwrap_or(sub.rn) != Register::PC,
            V7Operation::Stc(_) => todo!(),
            V7Operation::SubSpMinusImmediate(sub) => sub.rd.unwrap_or(Register::SP) != Register::PC,
            V7Operation::SubSpMinusRegister(sub) => sub.rd.unwrap_or(Register::SP) != Register::PC,
            V7Operation::Sxtab(_) => todo!(),
            V7Operation::Sxtab16(_) => todo!(),
            V7Operation::Sxtah(_) => todo!(),
            V7Operation::Sxtb(_) => todo!(),
            V7Operation::Sxtb16(_) => todo!(),
            V7Operation::Sxth(_) => todo!(),
            V7Operation::Tb(_) => true,
            V7Operation::TeqImmediate(_) => true,
            V7Operation::TeqRegister(_) => true,
            V7Operation::TstImmediate(_) => true,
            V7Operation::TstRegister(_) => true,
            V7Operation::Uadd16(ux) => ux.rd.unwrap_or(ux.rn) != Register::PC,
            V7Operation::Uadd8(ux) => ux.rd.unwrap_or(ux.rn) != Register::PC,
            V7Operation::Uasx(ux) => ux.rd.unwrap_or(ux.rn) != Register::PC,
            V7Operation::Ubfx(ux) => ux.rd != Register::PC,
            V7Operation::Udf(_ux) => todo!(),
            V7Operation::Udiv(ux) => ux.rd.unwrap_or(ux.rn) != Register::PC,
            V7Operation::Uhadd16(ux) => ux.rd.unwrap_or(ux.rn) != Register::PC,
            V7Operation::Uhadd8(ux) => ux.rd.unwrap_or(ux.rn) != Register::PC,
            V7Operation::Uhasx(ux) => ux.rd.unwrap_or(ux.rn) != Register::PC,
            V7Operation::Uhsax(ux) => ux.rd.unwrap_or(ux.rn) != Register::PC,
            V7Operation::Uhsub16(ux) => ux.rd.unwrap_or(ux.rn) != Register::PC,
            V7Operation::Uhsub8(ux) => ux.rd.unwrap_or(ux.rn) != Register::PC,
            V7Operation::Umaal(ux) => ux.rdlo != Register::PC && ux.rdhi != Register::PC,
            V7Operation::Umlal(ux) => ux.rdlo != Register::PC && ux.rdhi != Register::PC,
            V7Operation::Umull(ux) => ux.rdlo != Register::PC && ux.rdhi != Register::PC,
            V7Operation::Uqadd16(ux) => ux.rd.unwrap_or(ux.rn) != Register::PC,
            V7Operation::Uqadd8(ux) => ux.rd.unwrap_or(ux.rn) != Register::PC,
            V7Operation::Uqasx(ux) => ux.rd.unwrap_or(ux.rn) != Register::PC,
            V7Operation::Uqsax(ux) => ux.rd.unwrap_or(ux.rn) != Register::PC,
            V7Operation::Uqsub16(ux) => ux.rd.unwrap_or(ux.rn) != Register::PC,
            V7Operation::Uqsub8(ux) => ux.rd.unwrap_or(ux.rn) != Register::PC,
            V7Operation::Uqsad8(ux) => ux.rd.unwrap_or(ux.rn) != Register::PC,
            V7Operation::Usada8(ux) => ux.rd != Register::PC,
            V7Operation::Usad8(ux) => ux.rd.unwrap_or(ux.rn) != Register::PC,
            V7Operation::Usat(ux) => ux.rd != Register::PC,
            V7Operation::Usat16(ux) => ux.rd != Register::PC,
            V7Operation::Usax(ux) => ux.rd.unwrap_or(ux.rn) != Register::PC,
            V7Operation::Usub16(ux) => ux.rd.unwrap_or(ux.rn) != Register::PC,
            V7Operation::Usub8(ux) => ux.rd.unwrap_or(ux.rn) != Register::PC,
            V7Operation::Uxtab(ux) => ux.rd.unwrap_or(ux.rn) != Register::PC,
            V7Operation::Uxtab16(ux) => ux.rd.unwrap_or(ux.rn) != Register::PC,
            V7Operation::Uxtah(ux) => ux.rd.unwrap_or(ux.rn) != Register::PC,
            V7Operation::Uxtb(ux) => ux.rd != Register::PC,
            V7Operation::Uxtb16(ux) => ux.rd.unwrap_or(ux.rm) != Register::PC,
            V7Operation::Uxth(ux) => ux.rd != Register::PC,
            V7Operation::Wfe(_) => todo!(),
            V7Operation::Wfi(_) => todo!(),
            V7Operation::Yield(_) => todo!(),
        }
    }

    pub fn cycle_count_m4_core(instr: &V7Operation, state: &mut GAState) -> CycleCount {
        println!("Running OPERATION : {:?}", instr);
        let pipeline = |state: &mut GAState| match state.get_last_instruction() {
            Some(instr) => match instr.memory_access {
                true => 1,
                false => 2,
            },
            _ => 2,
        };
        let if_pc = |reg: Register, value: usize| {
            if reg == Register::PC {
                CycleCount::Function(|state: &mut GAState| {
                    let ext = state.as_ext::<ArmV7EMStateExt>();
                    let first_branch_occurance = ext.first_branch_occurance;
                    if !first_branch_occurance {
                        1 + 1
                    } else {
                        1 + ext.get_p()
                    }
                })
            } else {
                CycleCount::Value(value)
            }
        };
        let branch_predict = |_value: usize| -> CycleCount {
            let func = |state: &mut GAState| {
                let ext = state.as_ext::<ArmV7EMStateExt>();
                let first_branch_occurance = ext.first_branch_occurance;
                if !first_branch_occurance && ext.has_jumped {
                    1 + 1
                } else {
                    1 + ext.get_p()
                }
            };
            CycleCount::Function(func)
        };
        let branch_predict_single_cycle_if_not_taken = || -> CycleCount {
            let func = |state: &mut GAState| {
                let ext = state.as_ext::<ArmV7EMStateExt>();
                let first_branch_occurance = ext.first_branch_occurance;
                if !first_branch_occurance && ext.has_jumped {
                    1 + 1
                } else if !ext.has_jumped {
                    1
                } else {
                    1 + ext.get_p()
                }
            };
            CycleCount::Function(func)
        };
        let ret = match instr {
            V7Operation::AdcImmediate(_) | V7Operation::AdcRegister(_) => CycleCount::Value(1),
            V7Operation::AddImmediate(add) => if_pc(add.rd.unwrap_or(add.rn), 1),
            V7Operation::AddRegister(add) => if_pc(add.rd.unwrap_or(add.rn), 1),
            V7Operation::AddSPImmediate(add) => if_pc(add.rd.unwrap_or(Register::SP), 1),
            V7Operation::AddSPRegister(add) => if_pc(add.rd.unwrap_or(Register::SP), 1),
            V7Operation::Adr(_) => CycleCount::Value(1),
            V7Operation::AndImmediate(_) | V7Operation::AndRegister(_) => CycleCount::Value(1),
            V7Operation::AsrImmediate(_) | V7Operation::AsrRegister(_) => CycleCount::Value(1),
            V7Operation::B(_b) => branch_predict_single_cycle_if_not_taken(),
            V7Operation::Bfc(_) => CycleCount::Value(1),
            V7Operation::Bfi(_) => CycleCount::Value(1),
            V7Operation::BicImmediate(_) | V7Operation::BicRegister(_) => CycleCount::Value(1),
            V7Operation::Bkpt(_) => CycleCount::Value(0),
            V7Operation::Bl(_) => branch_predict(1),
            V7Operation::Blx(_) => branch_predict(1),
            V7Operation::Bx(_) => branch_predict(1),
            V7Operation::Cbz(_) => branch_predict_single_cycle_if_not_taken(),
            V7Operation::Clrex(_) => CycleCount::Value(1),
            V7Operation::Clz(_) => CycleCount::Value(1),
            V7Operation::CmnImmediate(_) | V7Operation::CmnRegister(_) => CycleCount::Value(1),
            V7Operation::CmpImmediate(_) | V7Operation::CmpRegister(_) => CycleCount::Value(1),
            // This is very unclear in the docs, might be 1 or 2
            V7Operation::Cps(_) => CycleCount::Value(2),
            // This is very unclear in the docs, might be 1 or 2
            V7Operation::Dbg(_) => CycleCount::Value(1),
            V7Operation::Dmb(_) => todo!("DMB : This requires a model barriers"),
            V7Operation::Dsb(_) => todo!("DSB : This requires a model of barriers"),
            V7Operation::EorImmediate(_) | V7Operation::EorRegister(_) => CycleCount::Value(1),
            V7Operation::Isb(_) => todo!("ISB : This requires a model of barriers"),
            // TODO! Add detection for wether this is folded or not, if it is the value here is 0
            V7Operation::It(_) => {
                let counter = |state: &mut GAState| match state.get_last_instruction() {
                    Some(instr) => match instr.instruction_size {
                        16 => 0,
                        _ => 1,
                    },
                    None => 1,
                };

                CycleCount::Function(counter)
            }
            V7Operation::Ldm(ldm) => {
                let pc = ldm.registers.registers.contains(&Register::PC);
                let n = ldm.registers.registers.len();
                let mut count = 1 + n;
                if pc {
                    // TODO! Model pipeline better
                    // These are not conditional so we should be fine to just
                    // assume that the predictor can predict this.
                    count += 1;
                }
                CycleCount::Value(count)
            }
            V7Operation::Ldmdb(ldm) => {
                let pc = ldm.registers.registers.contains(&Register::PC);
                let n = ldm.registers.registers.len();
                let mut count = 1 + n;
                if pc {
                    // TODO! Model pipeline better
                    // These are not conditional so we should be fine to just
                    // assume that the predictor can predict this.
                    count += 1;
                }
                CycleCount::Value(count)
            }
            // TODO! Add in pre load hints
            V7Operation::LdrImmediate(el) => match (el.rt, el.rn) {
                (Register::PC, _) => CycleCount::Function(|state: &mut GAState| {
                    let ext = state.as_ext::<ArmV7EMStateExt>();
                    let first_branch_occurance = ext.first_branch_occurance;
                    if !first_branch_occurance && ext.has_jumped {
                        2 + 1
                    } else {
                        2 + ext.get_p()
                    }
                }),
                (_, Register::PC) => CycleCount::Value(2),
                _ => CycleCount::Function(pipeline),
            },
            V7Operation::LdrLiteral(el) => match el.rt {
                Register::PC => CycleCount::Function(|state: &mut GAState| {
                    let ext = state.as_ext::<ArmV7EMStateExt>();
                    let first_branch_occurance = ext.first_branch_occurance;
                    if !first_branch_occurance && ext.has_jumped {
                        2 + 1
                    } else {
                        2 + ext.get_p()
                    }
                }),
                _ => CycleCount::Function(pipeline),
            },
            V7Operation::LdrRegister(el) => match (el.rt, el.rn) {
                (Register::PC, Register::PC) => CycleCount::Value(2),
                (Register::PC, _) => CycleCount::Function(|state: &mut GAState| {
                    let ext = state.as_ext::<ArmV7EMStateExt>();
                    let first_branch_occurance = ext.first_branch_occurance;
                    if !first_branch_occurance && ext.has_jumped {
                        2 + 1
                    } else {
                        2 + ext.get_p()
                    }
                }),
                _ => CycleCount::Function(pipeline),
            },
            V7Operation::LdrbImmediate(_)
            | V7Operation::LdrbLiteral(_)
            | V7Operation::LdrbRegister(_) => CycleCount::Value(2),
            V7Operation::Ldrbt(_) => CycleCount::Value(2),
            V7Operation::LdrdImmediate(_ldrd) => CycleCount::Value(1 + 2),
            V7Operation::LdrdLiteral(_) => CycleCount::Value(1 + 2),
            // TODO! This requires a model of semaphores
            V7Operation::Ldrex(_) | V7Operation::Ldrexb(_) | V7Operation::Ldrexh(_) => {
                todo!("Ldrex : Requires a model of the system")
            }
            // TODO! Add in model of contigous loads to allow next load to be single cycle
            V7Operation::LdrhImmediate(_)
            | V7Operation::LdrhLiteral(_)
            | V7Operation::LdrhRegister(_)
            | V7Operation::Ldrht(_)
            | V7Operation::LdrsbImmediate(_)
            | V7Operation::LdrsbLiteral(_)
            | V7Operation::LdrsbRegister(_)
            | V7Operation::Ldrsbt(_)
            | V7Operation::LdrshImmediate(_)
            | V7Operation::LdrshLiteral(_)
            | V7Operation::LdrshRegister(_)
            | V7Operation::Ldrsht(_)
            | V7Operation::Ldrt(_) => CycleCount::Function(pipeline),
            V7Operation::LslImmediate(_) | V7Operation::LslRegister(_) => CycleCount::Value(1),
            V7Operation::LsrImmediate(_) | V7Operation::LsrRegister(_) => CycleCount::Value(1),
            V7Operation::Mla(_) | V7Operation::Mls(_) => CycleCount::Value(2),
            V7Operation::MovImmediate(mov) => if_pc(mov.rd, 1),
            V7Operation::MovRegister(mov) => if_pc(mov.rd, 1),
            V7Operation::Movt(_) => CycleCount::Value(1),
            // Might be one or two.
            V7Operation::Mrs(_) => CycleCount::Value(2),
            // Might be one or two.
            V7Operation::Msr(_) => CycleCount::Value(2),
            V7Operation::Mul(_) => CycleCount::Value(1),
            V7Operation::MvnImmediate(_) | V7Operation::MvnRegister(_) => CycleCount::Value(1),
            V7Operation::Nop(_) => CycleCount::Value(1),
            V7Operation::OrnImmediate(_) | V7Operation::OrnRegister(_) => CycleCount::Value(1),
            V7Operation::OrrImmediate(_) | V7Operation::OrrRegister(_) => CycleCount::Value(1),
            V7Operation::Pkh(_) => CycleCount::Value(1),
            V7Operation::PldImmediate(_) => todo!("Add in preload hints"),
            V7Operation::PldLiteral(_) => todo!("Add in preload hints"),
            V7Operation::PldRegister(_) => todo!("Add in preload hints"),
            V7Operation::PliImmediate(_) => todo!("Add in preload hints"),
            V7Operation::PliRegister(_) => todo!("Add in preload hints"),
            V7Operation::Pop(pop) => {
                // TODO! Redo parts of symex to support branch prediction here.

                // TODO! Validate this, it might be incorrect
                // The documentation gives us
                // POP {<reglist>, PC} => 1 + N + P
                // This seems to count N as the number or registers in <reglist>
                // if we assume this to be the case we can subtract one from P
                // as that is the same as subtracting one from N

                // This might just be 1 Since the return statement *should* allways be
                // predictable
                let ret = match pop.registers.registers.contains(&Register::PC) {
                    true => 1,
                    _ => 0,
                };
                CycleCount::Value(1 + pop.registers.registers.len() + ret)
            }
            V7Operation::Push(push) => CycleCount::Value(1 + push.registers.registers.len()),
            V7Operation::Qadd(_) => CycleCount::Value(1),
            V7Operation::Qadd16(_) => CycleCount::Value(1),
            V7Operation::Qadd8(_) => CycleCount::Value(1),
            V7Operation::Qasx(_) => CycleCount::Value(1),
            V7Operation::Qdadd(_) => CycleCount::Value(1),
            V7Operation::Qdsub(_) => CycleCount::Value(1),
            V7Operation::Qsax(_) => CycleCount::Value(1),
            V7Operation::Qsub(_) => CycleCount::Value(1),
            V7Operation::Qsub16(_) => CycleCount::Value(1),
            V7Operation::Qsub8(_) => CycleCount::Value(1),
            V7Operation::Rbit(_) => CycleCount::Value(1),
            V7Operation::Rev(_) => CycleCount::Value(1),
            V7Operation::Rev16(_) => CycleCount::Value(1),
            V7Operation::Revsh(_) => CycleCount::Value(1),
            V7Operation::RorImmediate(_) | V7Operation::RorRegister(_) => CycleCount::Value(1),
            V7Operation::Rrx(_) => CycleCount::Value(1),
            V7Operation::RsbImmediate(_) | V7Operation::RsbRegister(_) => CycleCount::Value(1),
            V7Operation::Sadd16(_) => CycleCount::Value(1),
            V7Operation::Sadd8(_) => CycleCount::Value(1),
            V7Operation::Sasx(_) => CycleCount::Value(1),
            V7Operation::SbcImmediate(_) | V7Operation::SbcRegister(_) => CycleCount::Value(1),
            V7Operation::Sbfx(_) => CycleCount::Value(1),
            // TODO! Add way to find wether or not this is 12 or 2
            V7Operation::Sdiv(_) => CycleCount::Value(12),
            V7Operation::Sel(_) => CycleCount::Value(1),
            V7Operation::Sev(_) => CycleCount::Value(1),
            V7Operation::Shadd16(_) => CycleCount::Value(1),
            V7Operation::Shadd8(_) => CycleCount::Value(1),
            V7Operation::Shasx(_) => CycleCount::Value(1),
            V7Operation::Shsax(_) => CycleCount::Value(1),
            V7Operation::Shsub16(_) => CycleCount::Value(1),
            V7Operation::Shsub8(_) => CycleCount::Value(1),
            V7Operation::Smla(_) => CycleCount::Value(1),
            V7Operation::Smlad(_) => CycleCount::Value(1),
            V7Operation::Smlal(_) => CycleCount::Value(1),
            V7Operation::SmlalSelective(_) => CycleCount::Value(1),
            V7Operation::Smlald(_) => CycleCount::Value(1),
            V7Operation::Smlaw(_) => CycleCount::Value(1),
            V7Operation::Smlsd(_) => CycleCount::Value(1),
            V7Operation::Smlsld(_) => CycleCount::Value(1),
            V7Operation::Smmla(_) => CycleCount::Value(1),
            V7Operation::Smmls(_) => CycleCount::Value(1),
            V7Operation::Smmul(_) => CycleCount::Value(1),
            V7Operation::Smuad(_) => CycleCount::Value(1),
            V7Operation::Smul(_) => CycleCount::Value(1),
            V7Operation::Smull(_) => CycleCount::Value(1),
            V7Operation::Smulw(_) => CycleCount::Value(1),
            V7Operation::Smusd(_) => CycleCount::Value(1),
            V7Operation::Ssat(_) | V7Operation::Ssat16(_) => CycleCount::Value(1),
            V7Operation::Ssax(_) => CycleCount::Value(1),
            V7Operation::Ssub16(_) => CycleCount::Value(1),
            V7Operation::Ssub8(_) => CycleCount::Value(1),
            V7Operation::Stm(stm) => CycleCount::Value(1 + stm.registers.registers.len()),
            V7Operation::Stmdb(stm) => CycleCount::Value(1 + stm.registers.registers.len()),
            V7Operation::StrImmediate(_) | V7Operation::StrRegister(_) => {
                CycleCount::Function(pipeline)
            }
            V7Operation::StrbImmediate(_) | V7Operation::StrbRegister(_) => {
                CycleCount::Function(pipeline)
            }
            V7Operation::Strbt(_) => CycleCount::Value(2),
            // N is two here
            V7Operation::StrdImmediate(_strd) => CycleCount::Value(1 + 2),
            V7Operation::Strex(_) => todo!("STREX : requires a model of the system"),
            V7Operation::Strexb(_) => todo!("STREXB : requires a model of the system"),
            V7Operation::Strexh(_) => todo!("STREXH : requires a model of the system"),
            V7Operation::StrhImmediate(_)
            | V7Operation::StrhRegister(_)
            | V7Operation::Strht(_)
            | V7Operation::Strt(_) => CycleCount::Function(pipeline),
            V7Operation::SubImmediate(_) | V7Operation::SubRegister(_) => CycleCount::Value(1),
            V7Operation::SubSpMinusImmediate(_) => CycleCount::Value(1),
            V7Operation::SubSpMinusRegister(_) => CycleCount::Value(1),

            V7Operation::Sxtab(_) => CycleCount::Value(1),

            V7Operation::Sxtab16(_) => CycleCount::Value(1),
            V7Operation::Sxtah(_) => CycleCount::Value(1),
            V7Operation::Sxtb(_) => CycleCount::Value(1),
            V7Operation::Sxtb16(_) => CycleCount::Value(1),
            V7Operation::Sxth(_) => CycleCount::Value(1),
            V7Operation::Tb(_) => CycleCount::Function(|state: &mut GAState| {
                let ext = state.as_ext::<ArmV7EMStateExt>();

                let first_branch_occurance = ext.first_branch_occurance;
                if !first_branch_occurance && ext.has_jumped {
                    2 + 1
                } else {
                    2 + ext.get_p()
                }
            }),
            // TODO!  The docs do not mention any cycle count for this
            // might be incorret
            V7Operation::TeqImmediate(_) | V7Operation::TeqRegister(_) => CycleCount::Value(1),
            V7Operation::TstImmediate(_) | V7Operation::TstRegister(_) => CycleCount::Value(1),
            V7Operation::Uadd16(_) => CycleCount::Value(1),
            V7Operation::Uadd8(_) => CycleCount::Value(1),
            V7Operation::Uasx(_) => CycleCount::Value(1),
            V7Operation::Ubfx(_) => CycleCount::Value(1),
            V7Operation::Udf(_) => CycleCount::Value(1),
            // TODO! Add way to check if this is 12 or 2
            V7Operation::Udiv(_) => CycleCount::Value(12),
            V7Operation::Uhadd16(_) => CycleCount::Value(1),
            V7Operation::Uhadd8(_) => CycleCount::Value(1),
            V7Operation::Uhasx(_) => CycleCount::Value(1),
            V7Operation::Uhsax(_) => CycleCount::Value(1),
            V7Operation::Uhsub16(_) => CycleCount::Value(1),
            V7Operation::Uhsub8(_) => CycleCount::Value(1),
            V7Operation::Umaal(_) => CycleCount::Value(1),
            V7Operation::Umlal(_) => CycleCount::Value(1),
            V7Operation::Umull(_) => CycleCount::Value(1),
            V7Operation::Uqadd16(_) => CycleCount::Value(1),
            V7Operation::Uqadd8(_) => CycleCount::Value(1),
            V7Operation::Uqasx(_) => CycleCount::Value(1),
            V7Operation::Uqsax(_) => CycleCount::Value(1),
            V7Operation::Uqsub16(_) => CycleCount::Value(1),
            V7Operation::Uqsub8(_) => CycleCount::Value(1),
            V7Operation::Uqsad8(_) => CycleCount::Value(1),
            V7Operation::Usada8(_) => CycleCount::Value(1),
            V7Operation::Usad8(_) => CycleCount::Value(1),
            V7Operation::Usat(_) | V7Operation::Usat16(_) => CycleCount::Value(1),
            V7Operation::Usax(_) => CycleCount::Value(1),
            V7Operation::Usub16(_) => CycleCount::Value(1),
            V7Operation::Usub8(_) => CycleCount::Value(1),
            V7Operation::Uxtab(_) => CycleCount::Value(1),
            V7Operation::Uxtab16(_) => CycleCount::Value(1),
            V7Operation::Uxtah(_) => CycleCount::Value(1),
            V7Operation::Uxtb(_) => CycleCount::Value(1),
            V7Operation::Uxtb16(_) => CycleCount::Value(1),
            V7Operation::Uxth(_) => CycleCount::Value(1),
            V7Operation::Wfe(_) => todo!("This requires a model of events"),
            V7Operation::Wfi(_) => todo!("This requires a model of interrupts"),

            // This assumes that we have no core running
            V7Operation::Yield(_) => CycleCount::Value(1),
            V7Operation::Svc(_) => todo!(),
            V7Operation::Stc(_)
            | V7Operation::Mcr(_)
            | V7Operation::Mrc(_)
            | V7Operation::Mrrc(_)
            | V7Operation::Mcrr(_)
            | V7Operation::Cdp(_)
            | V7Operation::LdcImmediate(_)
            | V7Operation::LdcLiteral(_) => todo!(),
        };
        {
            let ex = state.as_ext::<ArmV7EMStateExt>();
            if let Some(prev_instr) = ex.prev_instr.take() {
                let is_pred = Self::trivially_predictable(&prev_instr);
                ex.set_is_predictable(is_pred);
            }
            ex.prev_instr = Some(instr.clone());
        }
        ret
    }
}
