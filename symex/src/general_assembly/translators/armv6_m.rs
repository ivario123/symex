//! Translator for the armv6-m instruction set

use armv6_m_instruction_parser::{
    instructons::{Instruction, Operation},
    registers::{Register, SpecialRegister},
};
use regex::Regex;
use tracing::trace;

use crate::{
    elf_util::{ExpressionType, Variable},
    general_assembly::{
        instruction::{Condition, CycleCount, Operand},
        project::{PCHook, RegisterReadHook, RegisterWriteHook},
        state::GAState,
        translator::Translatable,
        DataWord, RunConfig,
    },
};

type GAInstruction = crate::general_assembly::instruction::Instruction;
type GAOperation = crate::general_assembly::instruction::Operation;
type ArmCodition = armv6_m_instruction_parser::conditions::Condition;

fn cycle_count_m0plus_core(operation: &Operation) -> CycleCount {
    match operation {
        Operation::ADCReg { m: _, n: _, d: _ } => CycleCount::Value(1),
        Operation::ADDImm { imm: _, n: _, d: _ } => CycleCount::Value(1),
        Operation::ADDReg { m: _, n: _, d } => {
            let max_cycle = if *d == Register::PC { 2 } else { 1 };
            CycleCount::Value(max_cycle)
        }
        Operation::ADDImmSP { d: _, imm: _ } => CycleCount::Value(1),
        Operation::ADDRegSP { d: _, m: _ } => CycleCount::Value(1),
        Operation::ADR { d: _, imm: _ } => CycleCount::Value(1),
        Operation::ANDReg { m: _, dn: _ } => CycleCount::Value(1),
        Operation::ASRImm { imm: _, m: _, d: _ } => CycleCount::Value(1),
        Operation::ASRReg { m: _, dn: _ } => CycleCount::Value(1),
        Operation::B { cond: _, imm: _ } => {
            let max_cycle: fn(state: &GAState) -> usize = |state| {
                if state.get_has_jumped() {
                    2
                } else {
                    1
                }
            };
            CycleCount::Function(max_cycle)
        }
        Operation::BICReg { m: _, dn: _ } => CycleCount::Value(1),
        Operation::BKPT { imm: _ } => CycleCount::Value(0),
        Operation::BL { imm: _ } => CycleCount::Value(3),
        Operation::BLXReg { m: _ } => CycleCount::Value(2),
        Operation::BX { m: _ } => CycleCount::Value(2),
        Operation::CMNReg { m: _, n: _ } => CycleCount::Value(1),
        Operation::CMPImm { n: _, imm: _ } => CycleCount::Value(1),
        Operation::CMPReg { m: _, n: _ } => CycleCount::Value(1),
        Operation::CPS { im: _ } => CycleCount::Value(1),
        Operation::CPY => {
            // this is not a real instruction is equvelatn to mov
            unreachable!()
        }
        Operation::DMB { option: _ } => CycleCount::Value(3),
        Operation::DSB { option: _ } => CycleCount::Value(3),
        Operation::EORReg { m: _, dn: _ } => CycleCount::Value(1),
        Operation::ISB { option: _ } => CycleCount::Value(3),
        Operation::LDM { n: _, reg_list } => {
            let max_cycle = 1 + reg_list.len();
            CycleCount::Value(max_cycle)
        }

        // \/\/\/\/ Can be one depending on core implementation and address \/\/\/\/
        Operation::LDRImm { imm: _, n: _, t: _ } => CycleCount::Value(2),
        Operation::LDRLiteral { t: _, imm: _ } => CycleCount::Value(2),
        Operation::LDRReg { m: _, n: _, t: _ } => CycleCount::Value(2),
        Operation::LDRBImm { imm: _, n: _, t: _ } => CycleCount::Value(2),
        Operation::LDRBReg { m: _, n: _, t: _ } => CycleCount::Value(2),
        Operation::LDRHImm { imm: _, n: _, t: _ } => CycleCount::Value(2),
        Operation::LDRHReg { m: _, n: _, t: _ } => CycleCount::Value(2),
        Operation::LDRSBReg { m: _, n: _, t: _ } => CycleCount::Value(2),
        Operation::LDRSH { m: _, n: _, t: _ } => CycleCount::Value(2),
        // /\/\/\/\ Can be one depending on core implementation and address /\/\/\/\
        Operation::LSLImm { imm: _, m: _, d: _ } => CycleCount::Value(1),
        Operation::LSLReg { m: _, dn: _ } => CycleCount::Value(1),
        Operation::LSRImm { imm: _, m: _, d: _ } => CycleCount::Value(1),
        Operation::LSRReg { m: _, dn: _ } => CycleCount::Value(1),
        Operation::MOVImm { d: _, imm: _ } => CycleCount::Value(1),
        Operation::MOVReg {
            m: _,
            d,
            set_flags: _,
        } => {
            let max_cycle = if *d == Register::PC { 2 } else { 1 };
            CycleCount::Value(max_cycle)
        }
        Operation::MRS { d: _, sysm: _ } => CycleCount::Value(3),
        Operation::MSRReg { n: _, sysm: _ } => CycleCount::Value(3),
        Operation::MUL { n: _, dm: _ } => {
            CycleCount::Value(32) // Can be one depending on core implementation might be able to read this from somewhere.
        }
        Operation::MVNReg { m: _, d: _ } => CycleCount::Value(1),
        Operation::NOP => CycleCount::Value(1),
        Operation::ORRReg { m: _, dn: _ } => CycleCount::Value(1),
        Operation::POP { reg_list } => {
            let max_cycle = if reg_list.contains(&Register::PC) {
                3
            } else {
                1
            } + reg_list.len();
            CycleCount::Value(max_cycle)
        }
        Operation::PUSH { reg_list } => CycleCount::Value(1 + reg_list.len()),
        Operation::REV { m: _, d: _ } => CycleCount::Value(1),
        Operation::REV16 { m: _, d: _ } => CycleCount::Value(1),
        Operation::REVSH { m: _, d: _ } => CycleCount::Value(1),
        Operation::RORReg { m: _, dn: _ } => CycleCount::Value(1),
        Operation::RSBImm { n: _, d: _ } => CycleCount::Value(1),
        Operation::SBCReg { m: _, dn: _ } => CycleCount::Value(1),
        Operation::SEV => CycleCount::Value(1),
        Operation::STM { n: _, reg_list } => CycleCount::Value(1 + reg_list.len()),

        // \/\/\/\/ Can be one depending on core implementation and address \/\/\/\/
        Operation::STRImm { imm: _, n: _, t: _ } => CycleCount::Value(2),
        Operation::STRReg { m: _, n: _, t: _ } => CycleCount::Value(2),
        Operation::STRBImm { imm: _, n: _, t: _ } => CycleCount::Value(2),
        Operation::STRBReg { m: _, n: _, t: _ } => CycleCount::Value(2),
        Operation::STRHImm { imm: _, n: _, t: _ } => CycleCount::Value(2),
        Operation::STRHReg { m: _, n: _, t: _ } => CycleCount::Value(2),
        // /\/\/\/\ Can be one depending on core implementation and address /\/\/\/\
        Operation::SUBImm { imm: _, n: _, d: _ } => CycleCount::Value(1),
        Operation::SUBReg { m: _, n: _, d: _ } => CycleCount::Value(1),
        Operation::SUBImmSP { imm: _ } => CycleCount::Value(1),
        Operation::SVC { imm: _ } => CycleCount::Value(0),
        Operation::SXTB { m: _, d: _ } => CycleCount::Value(1),
        Operation::SXTH { m: _, d: _ } => CycleCount::Value(1),
        Operation::TSTReg { m: _, n: _ } => CycleCount::Value(1),
        Operation::UDFT1 { imm: _ } => {
            // generates a undefined exeption just panic for now
            unimplemented!()
        }
        Operation::UDFT2 { imm: _ } => {
            // generates a undefined exeption just panic for now
            unimplemented!()
        }
        Operation::UXTB { m: _, d: _ } => CycleCount::Value(1),
        Operation::UXTH { m: _, d: _ } => CycleCount::Value(1),
        Operation::WFE => todo!(),
        Operation::WFI => todo!(),
        Operation::YIELD => todo!(),
    }
}

#[allow(dead_code)]
fn cycle_count_m0_core(operation: &Operation) -> CycleCount {
    match operation {
        Operation::ADCReg { m: _, n: _, d: _ } => CycleCount::Value(1),
        Operation::ADDImm { imm: _, n: _, d: _ } => CycleCount::Value(1),
        Operation::ADDReg { m: _, n: _, d } => {
            let max_cycle = if *d == Register::PC { 3 } else { 1 };
            CycleCount::Value(max_cycle)
        }
        Operation::ADDImmSP { d: _, imm: _ } => CycleCount::Value(1),
        Operation::ADDRegSP { d: _, m: _ } => CycleCount::Value(1),
        Operation::ADR { d: _, imm: _ } => CycleCount::Value(1),
        Operation::ANDReg { m: _, dn: _ } => CycleCount::Value(1),
        Operation::ASRImm { imm: _, m: _, d: _ } => CycleCount::Value(1),
        Operation::ASRReg { m: _, dn: _ } => CycleCount::Value(1),
        Operation::B { cond: _, imm: _ } => {
            let max_cycle: fn(state: &GAState) -> usize = |state| {
                if state.get_has_jumped() {
                    3
                } else {
                    1
                }
            };
            CycleCount::Function(max_cycle)
        }
        Operation::BICReg { m: _, dn: _ } => CycleCount::Value(1),
        Operation::BKPT { imm: _ } => CycleCount::Value(0),
        Operation::BL { imm: _ } => CycleCount::Value(4),
        Operation::BLXReg { m: _ } => CycleCount::Value(3),
        Operation::BX { m: _ } => CycleCount::Value(3),
        Operation::CMNReg { m: _, n: _ } => CycleCount::Value(1),
        Operation::CMPImm { n: _, imm: _ } => CycleCount::Value(1),
        Operation::CMPReg { m: _, n: _ } => CycleCount::Value(1),
        Operation::CPS { im: _ } => CycleCount::Value(1),
        Operation::CPY => {
            // this is not a real instruction is equvelatn to mov
            unreachable!()
        }
        Operation::DMB { option: _ } => CycleCount::Value(4),
        Operation::DSB { option: _ } => CycleCount::Value(4),
        Operation::EORReg { m: _, dn: _ } => CycleCount::Value(1),
        Operation::ISB { option: _ } => CycleCount::Value(4),
        Operation::LDM { n: _, reg_list } => {
            let max_cycle = 1 + reg_list.len();
            CycleCount::Value(max_cycle)
        }
        Operation::LDRImm { imm: _, n: _, t: _ } => CycleCount::Value(2),
        Operation::LDRLiteral { t: _, imm: _ } => CycleCount::Value(2),
        Operation::LDRReg { m: _, n: _, t: _ } => CycleCount::Value(2),
        Operation::LDRBImm { imm: _, n: _, t: _ } => CycleCount::Value(2),
        Operation::LDRBReg { m: _, n: _, t: _ } => CycleCount::Value(2),
        Operation::LDRHImm { imm: _, n: _, t: _ } => CycleCount::Value(2),
        Operation::LDRHReg { m: _, n: _, t: _ } => CycleCount::Value(2),
        Operation::LDRSBReg { m: _, n: _, t: _ } => CycleCount::Value(2),
        Operation::LDRSH { m: _, n: _, t: _ } => CycleCount::Value(2),
        Operation::LSLImm { imm: _, m: _, d: _ } => CycleCount::Value(1),
        Operation::LSLReg { m: _, dn: _ } => CycleCount::Value(1),
        Operation::LSRImm { imm: _, m: _, d: _ } => CycleCount::Value(1),
        Operation::LSRReg { m: _, dn: _ } => CycleCount::Value(1),
        Operation::MOVImm { d: _, imm: _ } => CycleCount::Value(1),
        Operation::MOVReg {
            m: _,
            d,
            set_flags: _,
        } => {
            let max_cycle = if *d == Register::PC { 3 } else { 1 };
            CycleCount::Value(max_cycle)
        }
        Operation::MRS { d: _, sysm: _ } => CycleCount::Value(4),
        Operation::MSRReg { n: _, sysm: _ } => CycleCount::Value(4),
        Operation::MUL { n: _, dm: _ } => {
            CycleCount::Value(32) // Can be one depending on core implementation might be able to read this from somewhere.
        }
        Operation::MVNReg { m: _, d: _ } => CycleCount::Value(1),
        Operation::NOP => CycleCount::Value(1),
        Operation::ORRReg { m: _, dn: _ } => CycleCount::Value(1),
        Operation::POP { reg_list } => {
            let max_cycle = if reg_list.contains(&Register::PC) {
                4
            } else {
                1
            } + reg_list.len();
            CycleCount::Value(max_cycle)
        }
        Operation::PUSH { reg_list } => CycleCount::Value(1 + reg_list.len()),
        Operation::REV { m: _, d: _ } => CycleCount::Value(1),
        Operation::REV16 { m: _, d: _ } => CycleCount::Value(1),
        Operation::REVSH { m: _, d: _ } => CycleCount::Value(1),
        Operation::RORReg { m: _, dn: _ } => CycleCount::Value(1),
        Operation::RSBImm { n: _, d: _ } => CycleCount::Value(1),
        Operation::SBCReg { m: _, dn: _ } => CycleCount::Value(1),
        Operation::SEV => CycleCount::Value(1),
        Operation::STM { n: _, reg_list } => CycleCount::Value(1 + reg_list.len()),
        Operation::STRImm { imm: _, n: _, t: _ } => CycleCount::Value(2),
        Operation::STRReg { m: _, n: _, t: _ } => CycleCount::Value(2),
        Operation::STRBImm { imm: _, n: _, t: _ } => CycleCount::Value(2),
        Operation::STRBReg { m: _, n: _, t: _ } => CycleCount::Value(2),
        Operation::STRHImm { imm: _, n: _, t: _ } => CycleCount::Value(2),
        Operation::STRHReg { m: _, n: _, t: _ } => CycleCount::Value(2),
        Operation::SUBImm { imm: _, n: _, d: _ } => CycleCount::Value(1),
        Operation::SUBReg { m: _, n: _, d: _ } => CycleCount::Value(1),
        Operation::SUBImmSP { imm: _ } => CycleCount::Value(1),
        Operation::SVC { imm: _ } => CycleCount::Value(0),
        Operation::SXTB { m: _, d: _ } => CycleCount::Value(1),
        Operation::SXTH { m: _, d: _ } => CycleCount::Value(1),
        Operation::TSTReg { m: _, n: _ } => CycleCount::Value(1),
        Operation::UDFT1 { imm: _ } => {
            // generates a undefined exeption just panic for now
            unimplemented!()
        }
        Operation::UDFT2 { imm: _ } => {
            // generates a undefined exeption just panic for now
            unimplemented!()
        }
        Operation::UXTB { m: _, d: _ } => CycleCount::Value(1),
        Operation::UXTH { m: _, d: _ } => CycleCount::Value(1),
        Operation::WFE => todo!(),
        Operation::WFI => todo!(),
        Operation::YIELD => todo!(),
    }
}

impl Translatable for Instruction {
    fn translate(&self) -> GAInstruction {
        let operations = match &self.operation {
            Operation::ADCReg { m, n, d } => {
                let dest = arm_register_to_ga_operand(d);
                let mreg = arm_register_to_ga_operand(m);
                let nreg = arm_register_to_ga_operand(n);

                vec![
                    GAOperation::Adc {
                        destination: dest.clone(),
                        operand1: nreg.clone(),
                        operand2: mreg.clone(),
                    },
                    GAOperation::SetNFlag(dest.clone()),
                    GAOperation::SetZFlag(dest),
                    GAOperation::SetCFlag {
                        operand1: nreg.clone(),
                        operand2: mreg.clone(),
                        sub: false,
                        carry: true,
                    },
                    GAOperation::SetVFlag {
                        operand1: nreg,
                        operand2: mreg,
                        sub: false,
                        carry: true,
                    },
                ]
            }
            Operation::ADDImm { imm, n, d } => {
                let dest = arm_register_to_ga_operand(d);
                let imm = Operand::Immidiate(DataWord::Word32(*imm));
                let nreg = arm_register_to_ga_operand(n);
                let op_local = Operand::Local("op".to_owned());

                vec![
                    GAOperation::Move {
                        destination: op_local.clone(),
                        source: nreg.clone(),
                    },
                    GAOperation::Add {
                        destination: dest.clone(),
                        operand1: nreg,
                        operand2: imm.clone(),
                    },
                    GAOperation::SetNFlag(dest.clone()),
                    GAOperation::SetZFlag(dest),
                    GAOperation::SetCFlag {
                        operand1: op_local.clone(),
                        operand2: imm.clone(),
                        sub: false,
                        carry: false,
                    },
                    GAOperation::SetVFlag {
                        operand1: op_local,
                        operand2: imm,
                        sub: false,
                        carry: false,
                    },
                ]
            }
            Operation::ADDReg { m, n, d } => {
                let dest = arm_register_to_ga_operand(d);
                let mreg = arm_register_to_ga_operand(m);
                let nreg = arm_register_to_ga_operand(n);
                let m_local = Operand::Local("m".to_owned());
                let n_local = Operand::Local("n".to_owned());

                vec![
                    GAOperation::Move {
                        destination: m_local.clone(),
                        source: mreg.clone(),
                    },
                    GAOperation::Move {
                        destination: n_local.clone(),
                        source: nreg.clone(),
                    },
                    GAOperation::Add {
                        destination: dest.clone(),
                        operand1: nreg,
                        operand2: mreg,
                    },
                    GAOperation::SetNFlag(dest.clone()),
                    GAOperation::SetZFlag(dest),
                    GAOperation::SetCFlag {
                        operand1: n_local.clone(),
                        operand2: m_local.clone(),
                        sub: false,
                        carry: false,
                    },
                    GAOperation::SetVFlag {
                        operand1: n_local,
                        operand2: m_local,
                        sub: false,
                        carry: false,
                    },
                ]
            }
            Operation::ADDImmSP { d, imm } => vec![GAOperation::Add {
                destination: arm_register_to_ga_operand(d),
                operand1: arm_register_to_ga_operand(&Register::SP),
                operand2: Operand::Immidiate(DataWord::Word32(*imm)),
            }],
            Operation::ADDRegSP { d, m } => vec![GAOperation::Add {
                destination: arm_register_to_ga_operand(d),
                operand1: arm_register_to_ga_operand(&Register::SP),
                operand2: arm_register_to_ga_operand(m),
            }],
            Operation::ADR { d, imm } => {
                let imm = imm;
                vec![
                    GAOperation::Add {
                        destination: Operand::Local("addr".to_owned()),
                        operand1: Operand::Register("PC".to_owned()),
                        operand2: Operand::Immidiate(DataWord::Word32(2)),
                    },
                    GAOperation::And {
                        destination: Operand::Local("addr".to_owned()),
                        operand1: Operand::Local("addr".to_owned()),
                        operand2: Operand::Immidiate(DataWord::Word32(!0b11)),
                    },
                    GAOperation::Add {
                        destination: arm_register_to_ga_operand(d),
                        operand1: Operand::Local("addr".to_owned()),
                        operand2: Operand::Immidiate(DataWord::Word32(*imm)),
                    },
                ]
            }
            Operation::ANDReg { m, dn } => vec![
                GAOperation::And {
                    destination: arm_register_to_ga_operand(dn),
                    operand1: arm_register_to_ga_operand(dn),
                    operand2: arm_register_to_ga_operand(m),
                },
                GAOperation::SetNFlag(arm_register_to_ga_operand(dn)),
                GAOperation::SetZFlag(arm_register_to_ga_operand(dn)),
            ],
            Operation::ASRImm { imm, m, d } => vec![
                GAOperation::Move {
                    destination: Operand::Local("m".to_owned()),
                    source: arm_register_to_ga_operand(m),
                },
                GAOperation::Sra {
                    destination: arm_register_to_ga_operand(d),
                    operand: arm_register_to_ga_operand(m),
                    shift: Operand::Immidiate(DataWord::Word32(*imm)),
                },
                GAOperation::SetNFlag(arm_register_to_ga_operand(d)),
                GAOperation::SetZFlag(arm_register_to_ga_operand(d)),
                GAOperation::SetCFlagSra {
                    operand: Operand::Local("m".to_owned()),
                    shift: Operand::Immidiate(DataWord::Word32(*imm)),
                },
            ],
            Operation::ASRReg { m, dn } => {
                let dnreg = arm_register_to_ga_operand(dn);
                let mreg = arm_register_to_ga_operand(m);
                let n_local = Operand::Local("n".to_owned());
                let shift_local = Operand::Local("shift".to_owned());

                vec![
                    GAOperation::Move {
                        destination: n_local.clone(),
                        source: dnreg.clone(),
                    },
                    GAOperation::And {
                        destination: shift_local.clone(),
                        operand1: mreg,
                        operand2: Operand::Immidiate(DataWord::Word32(0xff)),
                    },
                    GAOperation::Sra {
                        destination: dnreg.clone(),
                        operand: dnreg.clone(),
                        shift: shift_local.clone(),
                    },
                    GAOperation::SetNFlag(dnreg.clone()),
                    GAOperation::SetZFlag(dnreg),
                    GAOperation::SetCFlagSra {
                        operand: n_local,
                        shift: shift_local,
                    },
                ]
            }
            Operation::B { cond, imm } => {
                let imm = imm + 2; // Beacause arm always adds as a 32 bit instruction.

                vec![
                    GAOperation::Add {
                        destination: Operand::Local("new_pc".to_owned()),
                        operand1: Operand::Register("PC".to_owned()),
                        operand2: Operand::Immidiate(DataWord::Word32(imm)),
                    },
                    GAOperation::ConditionalJump {
                        destination: Operand::Local("new_pc".to_owned()),
                        condition: arm_cond_to_ga_cond(cond),
                    },
                ]
            }
            Operation::BICReg { m, dn } => {
                let reg_m = arm_register_to_ga_operand(m);
                let reg_dn = arm_register_to_ga_operand(dn);

                vec![
                    GAOperation::Not {
                        destination: Operand::Local("mask".to_owned()),
                        operand: reg_m,
                    },
                    GAOperation::And {
                        destination: reg_dn.clone(),
                        operand1: reg_dn,
                        operand2: Operand::Local("mask".to_owned()),
                    },
                ]
            }
            Operation::BKPT { imm: _ } => vec![],
            Operation::BL { imm } => vec![
                GAOperation::Move {
                    destination: Operand::Local("PC".to_owned()),
                    source: Operand::Register("PC".to_owned()),
                },
                GAOperation::Move {
                    destination: Operand::Register("LR".to_owned()),
                    source: Operand::Local("PC".to_owned()),
                },
                GAOperation::Add {
                    destination: Operand::Local("newPC".to_owned()),
                    operand1: Operand::Local("PC".to_owned()),
                    operand2: Operand::Immidiate(DataWord::Word32(*imm)),
                },
                GAOperation::Move {
                    destination: Operand::Register("PC".to_owned()),
                    source: Operand::Local("newPC".to_owned()),
                },
            ],
            Operation::BLXReg { m } => vec![
                GAOperation::Move {
                    destination: arm_register_to_ga_operand(&Register::LR),
                    source: Operand::Register("PC".to_owned()),
                },
                GAOperation::Move {
                    destination: Operand::Register("PC".to_owned()),
                    source: arm_register_to_ga_operand(m),
                },
            ],
            Operation::BX { m } => {
                let reg = arm_register_to_ga_operand(m);
                let destination = Operand::Register("PC".to_owned());
                vec![GAOperation::Move {
                    destination,
                    source: reg,
                }]
            }
            Operation::CMNReg { m, n } => {
                let m = arm_register_to_ga_operand(m);
                let n = arm_register_to_ga_operand(n);
                vec![
                    GAOperation::Add {
                        destination: Operand::Local("result".to_owned()),
                        operand1: n.clone(),
                        operand2: m.clone(),
                    },
                    GAOperation::SetNFlag(Operand::Local("result".to_owned())),
                    GAOperation::SetZFlag(Operand::Local("result".to_owned())),
                    GAOperation::SetCFlag {
                        operand1: n.clone(),
                        operand2: m.clone(),
                        sub: false,
                        carry: false,
                    },
                    GAOperation::SetVFlag {
                        operand1: n,
                        operand2: m,
                        sub: false,
                        carry: false,
                    },
                ]
            }
            Operation::CMPImm { n, imm } => {
                let op_n = arm_register_to_ga_operand(n);
                let op_imm = Operand::Immidiate(DataWord::Word32(*imm));
                vec![
                    GAOperation::Sub {
                        destination: Operand::Local("result".to_owned()),
                        operand1: op_n.clone(),
                        operand2: op_imm.clone(),
                    },
                    GAOperation::SetNFlag(Operand::Local("result".to_owned())),
                    GAOperation::SetZFlag(Operand::Local("result".to_owned())),
                    GAOperation::SetCFlag {
                        operand1: op_n.clone(),
                        operand2: op_imm.clone(),
                        sub: true,
                        carry: false,
                    },
                    GAOperation::SetVFlag {
                        operand1: op_n.clone(),
                        operand2: op_imm.clone(),
                        sub: true,
                        carry: false,
                    },
                ]
            }
            Operation::CMPReg { m, n } => {
                let op_n = arm_register_to_ga_operand(n);
                let op_m = arm_register_to_ga_operand(m);
                vec![
                    GAOperation::Sub {
                        destination: Operand::Local("result".to_owned()),
                        operand1: op_n.clone(),
                        operand2: op_m.clone(),
                    },
                    GAOperation::SetNFlag(Operand::Local("result".to_owned())),
                    GAOperation::SetZFlag(Operand::Local("result".to_owned())),
                    GAOperation::SetCFlag {
                        operand1: op_n.clone(),
                        operand2: op_m.clone(),
                        sub: true,
                        carry: false,
                    },
                    GAOperation::SetVFlag {
                        operand1: op_n.clone(),
                        operand2: op_m.clone(),
                        sub: true,
                        carry: false,
                    },
                ]
            }
            Operation::CPS { im: _ } => {
                // change processor state do nothig for now but should probably be modeled
                // in armv6-m it is only used to enable disable interupts
                vec![]
            }
            Operation::CPY => {
                // this is not a real instruction is equvelatn to mov
                unreachable!()
            }
            Operation::DMB { option: _ } => {
                // data barier do nothig as data barier is not modeled yet
                vec![]
            }
            Operation::DSB { option: _ } => {
                // data barier do nothig as data barier is not modeled yet
                vec![]
            }
            Operation::EORReg { m, dn } => {
                let dn = arm_register_to_ga_operand(dn);
                vec![
                    GAOperation::Xor {
                        destination: dn.clone(),
                        operand1: dn.clone(),
                        operand2: arm_register_to_ga_operand(m),
                    },
                    GAOperation::SetNFlag(dn.clone()),
                    GAOperation::SetZFlag(dn),
                ]
            }
            Operation::ISB { option: _ } => {
                // flushes pipeline do nothig as pipeline is not modeled
                vec![]
            }
            Operation::LDM { n, reg_list } => {
                let mut operations: Vec<GAOperation> = vec![GAOperation::Move {
                    destination: Operand::Local("Address".to_owned()),
                    source: arm_register_to_ga_operand(n),
                }];
                for reg in reg_list {
                    // write register to memory
                    operations.push(GAOperation::Move {
                        destination: Operand::AddressInLocal("Address".to_owned(), 32),
                        source: arm_register_to_ga_operand(reg),
                    });
                    // update address
                    operations.push(GAOperation::Add {
                        destination: Operand::Local("Address".to_owned()),
                        operand1: Operand::Local("Address".to_owned()),
                        operand2: Operand::Immidiate(DataWord::Word32(4)),
                    })
                }
                if reg_list.contains(n) {
                    // addre reg not in reg list writeback
                    operations.push(GAOperation::Move {
                        destination: arm_register_to_ga_operand(n),
                        source: Operand::Local("Address".to_owned()),
                    });
                }
                operations
            }
            Operation::LDRImm { imm, n, t } => vec![
                GAOperation::Add {
                    destination: Operand::Local("addr".to_owned()),
                    operand1: arm_register_to_ga_operand(n),
                    operand2: Operand::Immidiate(DataWord::Word32(*imm)),
                },
                GAOperation::Move {
                    destination: arm_register_to_ga_operand(t),
                    source: Operand::AddressInLocal("addr".to_owned(), 32),
                },
            ],
            Operation::LDRLiteral { t, imm } => vec![
                GAOperation::Add {
                    destination: Operand::Local("addr".to_owned()),
                    operand1: arm_register_to_ga_operand(&Register::PC),
                    operand2: Operand::Immidiate(DataWord::Word32(2)),
                },
                GAOperation::And {
                    destination: Operand::Local("addr".to_owned()),
                    operand1: Operand::Local("addr".to_owned()),
                    operand2: Operand::Immidiate(DataWord::Word32(!0b11)),
                },
                GAOperation::Add {
                    destination: Operand::Local("addr".to_owned()),
                    operand1: Operand::Local("addr".to_owned()),
                    operand2: Operand::Immidiate(DataWord::Word32(*imm)),
                },
                GAOperation::Move {
                    destination: arm_register_to_ga_operand(t),
                    source: Operand::AddressInLocal("addr".to_owned(), 32),
                },
            ],
            Operation::LDRReg { m, n, t } => vec![
                GAOperation::Add {
                    destination: Operand::Local("addr".to_owned()),
                    operand1: arm_register_to_ga_operand(n),
                    operand2: arm_register_to_ga_operand(m),
                },
                GAOperation::Move {
                    destination: arm_register_to_ga_operand(t),
                    source: Operand::AddressInLocal("addr".to_owned(), 32),
                },
            ],
            Operation::LDRBImm { imm, n, t } => vec![
                GAOperation::Add {
                    destination: Operand::Local("addr".to_owned()),
                    operand1: arm_register_to_ga_operand(n),
                    operand2: Operand::Immidiate(DataWord::Word32(*imm)),
                },
                GAOperation::Move {
                    destination: arm_register_to_ga_operand(t),
                    source: Operand::AddressInLocal("addr".to_owned(), 8),
                },
                GAOperation::ZeroExtend {
                    destination: arm_register_to_ga_operand(t),
                    operand: arm_register_to_ga_operand(t),
                    bits: 8,
                },
            ],
            Operation::LDRBReg { m, n, t } => vec![
                GAOperation::Add {
                    destination: Operand::Local("addr".to_owned()),
                    operand1: arm_register_to_ga_operand(n),
                    operand2: arm_register_to_ga_operand(m),
                },
                GAOperation::Move {
                    destination: arm_register_to_ga_operand(t),
                    source: Operand::AddressInLocal("addr".to_owned(), 8),
                },
                GAOperation::ZeroExtend {
                    destination: arm_register_to_ga_operand(t),
                    operand: arm_register_to_ga_operand(t),
                    bits: 8,
                },
            ],
            Operation::LDRHImm { imm, n, t } => vec![
                GAOperation::Add {
                    destination: Operand::Local("addr".to_owned()),
                    operand1: arm_register_to_ga_operand(n),
                    operand2: Operand::Immidiate(DataWord::Word32(*imm)),
                },
                GAOperation::Move {
                    destination: arm_register_to_ga_operand(t),
                    source: Operand::AddressInLocal("addr".to_owned(), 16),
                },
                GAOperation::ZeroExtend {
                    destination: arm_register_to_ga_operand(t),
                    operand: arm_register_to_ga_operand(t),
                    bits: 16,
                },
            ],
            Operation::LDRHReg { m, n, t } => vec![
                GAOperation::Add {
                    destination: Operand::Local("addr".to_owned()),
                    operand1: arm_register_to_ga_operand(n),
                    operand2: arm_register_to_ga_operand(m),
                },
                GAOperation::Move {
                    destination: arm_register_to_ga_operand(t),
                    source: Operand::AddressInLocal("addr".to_owned(), 16),
                },
                GAOperation::ZeroExtend {
                    destination: arm_register_to_ga_operand(t),
                    operand: arm_register_to_ga_operand(t),
                    bits: 16,
                },
            ],
            Operation::LDRSBReg { m, n, t } => vec![
                GAOperation::Add {
                    destination: Operand::Local("addr".to_owned()),
                    operand1: arm_register_to_ga_operand(n),
                    operand2: arm_register_to_ga_operand(m),
                },
                GAOperation::Move {
                    destination: arm_register_to_ga_operand(t),
                    source: Operand::AddressInLocal("addr".to_owned(), 8),
                },
                GAOperation::SignExtend {
                    destination: arm_register_to_ga_operand(t),
                    operand: arm_register_to_ga_operand(t),
                    bits: 8,
                },
            ],
            Operation::LDRSH { m, n, t } => vec![
                GAOperation::Add {
                    destination: Operand::Local("addr".to_owned()),
                    operand1: arm_register_to_ga_operand(n),
                    operand2: arm_register_to_ga_operand(m),
                },
                GAOperation::Move {
                    destination: arm_register_to_ga_operand(t),
                    source: Operand::AddressInLocal("addr".to_owned(), 16),
                },
                GAOperation::SignExtend {
                    destination: arm_register_to_ga_operand(t),
                    operand: arm_register_to_ga_operand(t),
                    bits: 8,
                },
            ],
            Operation::LSLImm { imm, m, d } => vec![
                GAOperation::Sl {
                    destination: arm_register_to_ga_operand(d),
                    operand: arm_register_to_ga_operand(m),
                    shift: Operand::Immidiate(DataWord::Word32(*imm)),
                },
                GAOperation::SetNFlag(arm_register_to_ga_operand(d)),
                GAOperation::SetZFlag(arm_register_to_ga_operand(d)),
            ],
            Operation::LSLReg { m, dn } => vec![
                GAOperation::And {
                    destination: Operand::Local("shift".to_owned()),
                    operand1: arm_register_to_ga_operand(m),
                    operand2: Operand::Immidiate(DataWord::Word32(0xff)),
                },
                GAOperation::Sl {
                    destination: arm_register_to_ga_operand(dn),
                    operand: arm_register_to_ga_operand(dn),
                    shift: Operand::Local("shift".to_owned()),
                },
                GAOperation::SetNFlag(arm_register_to_ga_operand(dn)),
                GAOperation::SetZFlag(arm_register_to_ga_operand(dn)),
            ],
            Operation::LSRImm { imm, m, d } => vec![
                GAOperation::Srl {
                    destination: arm_register_to_ga_operand(d),
                    operand: arm_register_to_ga_operand(m),
                    shift: Operand::Immidiate(DataWord::Word32(*imm)),
                },
                GAOperation::SetNFlag(arm_register_to_ga_operand(d)),
                GAOperation::SetZFlag(arm_register_to_ga_operand(d)),
            ],
            Operation::LSRReg { m, dn } => vec![
                GAOperation::And {
                    destination: Operand::Local("shift".to_owned()),
                    operand1: arm_register_to_ga_operand(m),
                    operand2: Operand::Immidiate(DataWord::Word32(0xff)),
                },
                GAOperation::Srl {
                    destination: arm_register_to_ga_operand(dn),
                    operand: arm_register_to_ga_operand(dn),
                    shift: Operand::Local("shift".to_owned()),
                },
                GAOperation::SetNFlag(arm_register_to_ga_operand(dn)),
                GAOperation::SetZFlag(arm_register_to_ga_operand(dn)),
            ],
            Operation::MOVImm { d, imm } => {
                let destination = arm_register_to_ga_operand(d);
                let source = Operand::Immidiate(DataWord::Word32(*imm));

                vec![
                    GAOperation::Move {
                        destination: destination.clone(),
                        source,
                    },
                    GAOperation::SetNFlag(destination.clone()),
                    GAOperation::SetZFlag(destination),
                ]
            }
            Operation::MOVReg { m, d, set_flags } => {
                let destination = arm_register_to_ga_operand(d);
                let source = arm_register_to_ga_operand(m);
                let mut operations = vec![GAOperation::Move {
                    destination: destination.clone(),
                    source: source.clone(),
                }];

                if *set_flags {
                    operations.push(GAOperation::SetNFlag(destination.clone()));
                    operations.push(GAOperation::SetZFlag(destination));
                }
                operations
            }
            Operation::MRS { d, sysm } => vec![GAOperation::Move {
                destination: arm_register_to_ga_operand(d),
                source: arm_special_register_to_operand(sysm),
            }],
            Operation::MSRReg { n, sysm } => vec![GAOperation::Move {
                source: arm_register_to_ga_operand(n),
                destination: arm_special_register_to_operand(sysm),
            }],
            Operation::MUL { n, dm } => {
                let n = arm_register_to_ga_operand(n);
                let dm = arm_register_to_ga_operand(dm);

                vec![
                    GAOperation::Mul {
                        destination: dm.clone(),
                        operand1: n,
                        operand2: dm.clone(),
                    },
                    GAOperation::SetNFlag(dm.clone()),
                    GAOperation::SetZFlag(dm.clone()),
                ]
            }
            Operation::MVNReg { m, d } => {
                let m = arm_register_to_ga_operand(m);
                let d = arm_register_to_ga_operand(d);

                vec![
                    GAOperation::Not {
                        destination: d.clone(),
                        operand: m,
                    },
                    GAOperation::SetNFlag(d.clone()),
                    GAOperation::SetZFlag(d),
                ]
            }
            Operation::NOP => vec![],
            Operation::ORRReg { m, dn } => {
                let m = arm_register_to_ga_operand(m);
                let dn = arm_register_to_ga_operand(dn);

                vec![
                    GAOperation::Or {
                        destination: dn.clone(),
                        operand1: dn.clone(),
                        operand2: m,
                    },
                    GAOperation::SetNFlag(dn.clone()),
                    GAOperation::SetZFlag(dn.clone()),
                ]
            }
            Operation::POP { reg_list } => {
                let mut operations: Vec<GAOperation> = vec![];
                // set up base address
                operations.push(GAOperation::Move {
                    destination: Operand::Local("Address".to_owned()),
                    source: Operand::Register("SP".to_owned()),
                });
                for reg in reg_list {
                    // write register to memory
                    operations.push(GAOperation::Move {
                        source: Operand::AddressInLocal("Address".to_owned(), 32),
                        destination: arm_register_to_ga_operand(reg),
                    });
                    // update address
                    operations.push(GAOperation::Add {
                        destination: Operand::Local("Address".to_owned()),
                        operand1: Operand::Local("Address".to_owned()),
                        operand2: Operand::Immidiate(DataWord::Word32(4)),
                    })
                }
                // update SP
                operations.push(GAOperation::Add {
                    destination: Operand::Register("SP".to_owned()),
                    operand1: Operand::Register("SP".to_owned()),
                    operand2: Operand::Immidiate(DataWord::Word32((4 * reg_list.len()) as u32)),
                });

                operations
            }
            Operation::PUSH { reg_list } => {
                let mut operations: Vec<GAOperation> = vec![];
                // set up base address
                operations.push(GAOperation::Sub {
                    destination: Operand::Local("Address".to_owned()),
                    operand1: Operand::Register("SP".to_owned()),
                    operand2: Operand::Immidiate(DataWord::Word32((4 * reg_list.len()) as u32)),
                });
                for reg in reg_list {
                    // write register to memory
                    operations.push(GAOperation::Move {
                        destination: Operand::AddressInLocal("Address".to_owned(), 32),
                        source: arm_register_to_ga_operand(reg),
                    });
                    // update address
                    operations.push(GAOperation::Add {
                        destination: Operand::Local("Address".to_owned()),
                        operand1: Operand::Local("Address".to_owned()),
                        operand2: Operand::Immidiate(DataWord::Word32(4)),
                    })
                }
                // update SP
                operations.push(GAOperation::Sub {
                    destination: Operand::Register("SP".to_owned()),
                    operand1: Operand::Register("SP".to_owned()),
                    operand2: Operand::Immidiate(DataWord::Word32((4 * reg_list.len()) as u32)),
                });

                operations
            }
            Operation::REV { m, d } => {
                let m = arm_register_to_ga_operand(m);
                let d = arm_register_to_ga_operand(d);
                let b1 = Operand::Local("b1".to_owned());
                let b2 = Operand::Local("B2".to_owned());
                let b3 = Operand::Local("B3".to_owned());
                let b4 = Operand::Local("B4".to_owned());

                let b1_mask = Operand::Immidiate(DataWord::Word32(0x000000ff));
                let b2_mask = Operand::Immidiate(DataWord::Word32(0x0000ff00));
                let b3_mask = Operand::Immidiate(DataWord::Word32(0x00ff0000));
                let b4_mask = Operand::Immidiate(DataWord::Word32(0xff000000));

                vec![
                    // set destination to 0
                    GAOperation::Move {
                        destination: d.clone(),
                        source: Operand::Immidiate(DataWord::Word32(0)),
                    },
                    // extract all bytes
                    GAOperation::And {
                        destination: b1.clone(),
                        operand1: m.clone(),
                        operand2: b1_mask,
                    },
                    GAOperation::And {
                        destination: b2.clone(),
                        operand1: m.clone(),
                        operand2: b2_mask,
                    },
                    GAOperation::And {
                        destination: b3.clone(),
                        operand1: m.clone(),
                        operand2: b3_mask,
                    },
                    GAOperation::And {
                        destination: b4.clone(),
                        operand1: m.clone(),
                        operand2: b4_mask,
                    },
                    // shift all bytes
                    GAOperation::Sl {
                        destination: b1.clone(),
                        operand: b1.clone(),
                        shift: Operand::Immidiate(DataWord::Word32(24)),
                    },
                    GAOperation::Sl {
                        destination: b2.clone(),
                        operand: b2.clone(),
                        shift: Operand::Immidiate(DataWord::Word32(8)),
                    },
                    GAOperation::Srl {
                        destination: b3.clone(),
                        operand: b3.clone(),
                        shift: Operand::Immidiate(DataWord::Word32(8)),
                    },
                    GAOperation::Srl {
                        destination: b4.clone(),
                        operand: b4.clone(),
                        shift: Operand::Immidiate(DataWord::Word32(24)),
                    },
                    // or in to destination
                    GAOperation::Or {
                        destination: d.clone(),
                        operand1: d.clone(),
                        operand2: b1.clone(),
                    },
                    GAOperation::Or {
                        destination: d.clone(),
                        operand1: d.clone(),
                        operand2: b2.clone(),
                    },
                    GAOperation::Or {
                        destination: d.clone(),
                        operand1: d.clone(),
                        operand2: b3.clone(),
                    },
                    GAOperation::Or {
                        destination: d.clone(),
                        operand1: d.clone(),
                        operand2: b4.clone(),
                    },
                ]
            }
            Operation::REV16 { m, d } => {
                let m = arm_register_to_ga_operand(m);
                let d = arm_register_to_ga_operand(d);
                let b1 = Operand::Local("b1".to_owned());
                let b2 = Operand::Local("B2".to_owned());
                let b3 = Operand::Local("B3".to_owned());
                let b4 = Operand::Local("B4".to_owned());

                let b1_mask = Operand::Immidiate(DataWord::Word32(0x000000ff));
                let b2_mask = Operand::Immidiate(DataWord::Word32(0x0000ff00));
                let b3_mask = Operand::Immidiate(DataWord::Word32(0x00ff0000));
                let b4_mask = Operand::Immidiate(DataWord::Word32(0xff000000));

                vec![
                    // set destination to 0
                    GAOperation::Move {
                        destination: d.clone(),
                        source: Operand::Immidiate(DataWord::Word32(0)),
                    },
                    // extract all bytes
                    GAOperation::And {
                        destination: b1.clone(),
                        operand1: m.clone(),
                        operand2: b1_mask,
                    },
                    GAOperation::And {
                        destination: b2.clone(),
                        operand1: m.clone(),
                        operand2: b2_mask,
                    },
                    GAOperation::And {
                        destination: b3.clone(),
                        operand1: m.clone(),
                        operand2: b3_mask,
                    },
                    GAOperation::And {
                        destination: b4.clone(),
                        operand1: m.clone(),
                        operand2: b4_mask,
                    },
                    // shift all bytes
                    GAOperation::Sl {
                        destination: b1.clone(),
                        operand: b1.clone(),
                        shift: Operand::Immidiate(DataWord::Word32(8)),
                    },
                    GAOperation::Srl {
                        destination: b2.clone(),
                        operand: b2.clone(),
                        shift: Operand::Immidiate(DataWord::Word32(8)),
                    },
                    GAOperation::Sl {
                        destination: b3.clone(),
                        operand: b3.clone(),
                        shift: Operand::Immidiate(DataWord::Word32(8)),
                    },
                    GAOperation::Srl {
                        destination: b4.clone(),
                        operand: b4.clone(),
                        shift: Operand::Immidiate(DataWord::Word32(8)),
                    },
                    // or in to destination
                    GAOperation::Or {
                        destination: d.clone(),
                        operand1: d.clone(),
                        operand2: b1.clone(),
                    },
                    GAOperation::Or {
                        destination: d.clone(),
                        operand1: d.clone(),
                        operand2: b2.clone(),
                    },
                    GAOperation::Or {
                        destination: d.clone(),
                        operand1: d.clone(),
                        operand2: b3.clone(),
                    },
                    GAOperation::Or {
                        destination: d.clone(),
                        operand1: d.clone(),
                        operand2: b4.clone(),
                    },
                ]
            }
            Operation::REVSH { m, d } => {
                let m = arm_register_to_ga_operand(m);
                let d = arm_register_to_ga_operand(d);
                let b1 = Operand::Local("b1".to_owned());
                let b2 = Operand::Local("B2".to_owned());

                let b1_mask = Operand::Immidiate(DataWord::Word32(0x000000ff));
                let b2_mask = Operand::Immidiate(DataWord::Word32(0x0000ff00));

                vec![
                    // set destination to 0
                    GAOperation::Move {
                        destination: d.clone(),
                        source: Operand::Immidiate(DataWord::Word32(0)),
                    },
                    // extract all bytes
                    GAOperation::And {
                        destination: b1.clone(),
                        operand1: m.clone(),
                        operand2: b1_mask,
                    },
                    GAOperation::And {
                        destination: b2.clone(),
                        operand1: m.clone(),
                        operand2: b2_mask,
                    },
                    // shift all bytes
                    GAOperation::Sl {
                        destination: b1.clone(),
                        operand: b1.clone(),
                        shift: Operand::Immidiate(DataWord::Word32(8)),
                    },
                    GAOperation::Srl {
                        destination: b2.clone(),
                        operand: b2.clone(),
                        shift: Operand::Immidiate(DataWord::Word32(8)),
                    },
                    // or in to destination
                    GAOperation::Or {
                        destination: d.clone(),
                        operand1: d.clone(),
                        operand2: b1.clone(),
                    },
                    GAOperation::Or {
                        destination: d.clone(),
                        operand1: d.clone(),
                        operand2: b2.clone(),
                    },
                    // sign extend
                    GAOperation::SignExtend {
                        destination: d.clone(),
                        operand: d.clone(),
                        bits: 16,
                    },
                ]
            }
            Operation::RORReg { m, dn } => {
                let m = arm_register_to_ga_operand(m);
                let dn = arm_register_to_ga_operand(dn);
                let shift = Operand::Local("shift".to_owned());
                let mask = Operand::Immidiate(DataWord::Word32(0xff));
                vec![
                    GAOperation::And {
                        destination: shift.clone(),
                        operand1: m,
                        operand2: mask,
                    },
                    GAOperation::Sror {
                        destination: dn.clone(),
                        operand: dn.clone(),
                        shift: shift,
                    },
                    GAOperation::SetNFlag(dn.clone()),
                    GAOperation::SetZFlag(dn.clone()),
                    GAOperation::SetCFlagRor(dn),
                ]
            }
            Operation::RSBImm { n, d } => {
                let n = arm_register_to_ga_operand(n);
                let d = arm_register_to_ga_operand(d);
                let local_n = Operand::Local("n".to_owned());
                let zero = Operand::Immidiate(DataWord::Word32(0));

                vec![
                    GAOperation::Move {
                        destination: local_n.clone(),
                        source: n.clone(),
                    },
                    GAOperation::Sub {
                        destination: d.clone(),
                        operand1: zero.clone(),
                        operand2: n,
                    },
                    GAOperation::SetNFlag(d.clone()),
                    GAOperation::SetZFlag(d),
                    GAOperation::SetCFlag {
                        operand1: zero.clone(),
                        operand2: local_n.clone(),
                        sub: true,
                        carry: false,
                    },
                    GAOperation::SetVFlag {
                        operand1: zero,
                        operand2: local_n,
                        sub: true,
                        carry: false,
                    },
                ]
            }
            Operation::SBCReg { m, dn } => {
                let m = arm_register_to_ga_operand(m);
                let dn = arm_register_to_ga_operand(dn);
                let not_m = Operand::Local("not_m".to_owned());
                let local_n = Operand::Local("n".to_owned());

                vec![
                    GAOperation::Move {
                        destination: local_n.clone(),
                        source: dn.clone(),
                    },
                    GAOperation::Not {
                        destination: not_m.clone(),
                        operand: m,
                    },
                    GAOperation::Adc {
                        destination: dn.clone(),
                        operand1: dn.clone(),
                        operand2: not_m.clone(),
                    },
                    GAOperation::SetNFlag(dn.clone()),
                    GAOperation::SetZFlag(dn.clone()),
                    GAOperation::SetCFlag {
                        operand1: local_n.clone(),
                        operand2: not_m.clone(),
                        sub: false,
                        carry: true,
                    },
                    GAOperation::SetVFlag {
                        operand1: local_n,
                        operand2: not_m,
                        sub: false,
                        carry: true,
                    },
                ]
            }
            Operation::SEV => {
                // sends a hint event to all cores, multicore is not modeled so do nothing for now
                vec![]
            }
            Operation::STM { n, reg_list } => {
                let n = arm_register_to_ga_operand(n);
                let addr = Operand::Local("addr".to_owned());
                let to_addr = Operand::AddressInLocal("addr".to_owned(), 32);
                let four = Operand::Immidiate(DataWord::Word32(4));
                let mut operations = vec![GAOperation::Move {
                    destination: addr.clone(),
                    source: n.clone(),
                }];

                for reg in reg_list {
                    let reg = arm_register_to_ga_operand(reg);
                    operations.push(GAOperation::Move {
                        destination: to_addr.clone(),
                        source: reg,
                    });
                    operations.push(GAOperation::Add {
                        destination: addr.clone(),
                        operand1: addr.clone(),
                        operand2: four.clone(),
                    });
                }

                operations.push(GAOperation::Move {
                    destination: n,
                    source: addr,
                });

                operations
            }
            Operation::STRImm { imm, n, t } => {
                let imm = Operand::Immidiate(DataWord::Word32(*imm));
                let n = arm_register_to_ga_operand(n);
                let t = arm_register_to_ga_operand(t);
                let addr = Operand::Local("addr".to_owned());
                let to_addr = Operand::AddressInLocal("addr".to_owned(), 32);

                vec![
                    GAOperation::Add {
                        destination: addr.clone(),
                        operand1: n,
                        operand2: imm,
                    },
                    GAOperation::Move {
                        destination: to_addr,
                        source: t,
                    },
                ]
            }
            Operation::STRReg { m, n, t } => {
                let m = arm_register_to_ga_operand(m);
                let n = arm_register_to_ga_operand(n);
                let t = arm_register_to_ga_operand(t);
                let addr = Operand::Local("addr".to_owned());
                let to_addr = Operand::AddressInLocal("addr".to_owned(), 32);

                vec![
                    GAOperation::Add {
                        destination: addr.clone(),
                        operand1: n,
                        operand2: m,
                    },
                    GAOperation::Move {
                        destination: to_addr,
                        source: t,
                    },
                ]
            }
            Operation::STRBImm { imm, n, t } => {
                let imm = Operand::Immidiate(DataWord::Word32(*imm));
                let n = arm_register_to_ga_operand(n);
                let t = arm_register_to_ga_operand(t);
                let addr = Operand::Local("addr".to_owned());
                let to_addr = Operand::AddressInLocal("addr".to_owned(), 8);

                vec![
                    GAOperation::Add {
                        destination: addr.clone(),
                        operand1: n,
                        operand2: imm,
                    },
                    GAOperation::Move {
                        destination: to_addr,
                        source: t,
                    },
                ]
            }
            Operation::STRBReg { m, n, t } => {
                let m = arm_register_to_ga_operand(m);
                let n = arm_register_to_ga_operand(n);
                let t = arm_register_to_ga_operand(t);
                let addr = Operand::Local("addr".to_owned());
                let to_addr = Operand::AddressInLocal("addr".to_owned(), 8);

                vec![
                    GAOperation::Add {
                        destination: addr.clone(),
                        operand1: n,
                        operand2: m,
                    },
                    GAOperation::Move {
                        destination: to_addr,
                        source: t,
                    },
                ]
            }
            Operation::STRHImm { imm, n, t } => {
                let imm = Operand::Immidiate(DataWord::Word32(*imm));
                let n = arm_register_to_ga_operand(n);
                let t = arm_register_to_ga_operand(t);
                let addr = Operand::Local("addr".to_owned());
                let to_addr = Operand::AddressInLocal("addr".to_owned(), 16);

                vec![
                    GAOperation::Add {
                        destination: addr.clone(),
                        operand1: n,
                        operand2: imm,
                    },
                    GAOperation::Move {
                        destination: to_addr,
                        source: t,
                    },
                ]
            }
            Operation::STRHReg { m, n, t } => {
                let m = arm_register_to_ga_operand(m);
                let n = arm_register_to_ga_operand(n);
                let t = arm_register_to_ga_operand(t);
                let addr = Operand::Local("addr".to_owned());
                let to_addr = Operand::AddressInLocal("addr".to_owned(), 16);

                vec![
                    GAOperation::Add {
                        destination: addr.clone(),
                        operand1: n,
                        operand2: m,
                    },
                    GAOperation::Move {
                        destination: to_addr,
                        source: t,
                    },
                ]
            }
            Operation::SUBImm { imm, n, d } => {
                let imm = Operand::Immidiate(DataWord::Word32(*imm));
                let n = arm_register_to_ga_operand(n);
                let d = arm_register_to_ga_operand(d);
                let local_n = Operand::Local("n".to_owned());

                vec![
                    GAOperation::Move {
                        destination: local_n.clone(),
                        source: n.clone(),
                    },
                    GAOperation::Sub {
                        destination: d.clone(),
                        operand1: n,
                        operand2: imm.clone(),
                    },
                    GAOperation::SetNFlag(d.clone()),
                    GAOperation::SetZFlag(d),
                    GAOperation::SetCFlag {
                        operand1: local_n.clone(),
                        operand2: imm.clone(),
                        sub: true,
                        carry: false,
                    },
                    GAOperation::SetVFlag {
                        operand1: local_n.clone(),
                        operand2: imm.clone(),
                        sub: true,
                        carry: false,
                    },
                ]
            }
            Operation::SUBReg { m, n, d } => {
                let m = arm_register_to_ga_operand(m);
                let n = arm_register_to_ga_operand(n);
                let d = arm_register_to_ga_operand(d);
                let local_n = Operand::Local("n".to_owned());

                vec![
                    GAOperation::Move {
                        destination: local_n.clone(),
                        source: n.clone(),
                    },
                    GAOperation::Sub {
                        destination: d.clone(),
                        operand1: n,
                        operand2: m.clone(),
                    },
                    GAOperation::SetNFlag(d.clone()),
                    GAOperation::SetZFlag(d),
                    GAOperation::SetCFlag {
                        operand1: local_n.clone(),
                        operand2: m.clone(),
                        sub: true,
                        carry: false,
                    },
                    GAOperation::SetVFlag {
                        operand1: local_n.clone(),
                        operand2: m.clone(),
                        sub: true,
                        carry: false,
                    },
                ]
            }
            Operation::SUBImmSP { imm } => vec![GAOperation::Sub {
                destination: Operand::Register("SP".to_owned()),
                operand1: Operand::Register("SP".to_owned()),
                operand2: Operand::Immidiate(DataWord::Word32(*imm)),
            }],
            Operation::SVC { imm: _ } => {
                // to be used to call a supervisor in a OS
                // this functionality is not modeled so do nothing
                vec![]
            }
            Operation::SXTB { m, d } => {
                let m = arm_register_to_ga_operand(m);
                let d = arm_register_to_ga_operand(d);

                vec![GAOperation::SignExtend {
                    destination: d,
                    operand: m,
                    bits: 8,
                }]
            }
            Operation::SXTH { m, d } => {
                let m = arm_register_to_ga_operand(m);
                let d = arm_register_to_ga_operand(d);

                vec![GAOperation::SignExtend {
                    destination: d,
                    operand: m,
                    bits: 16,
                }]
            }
            Operation::TSTReg { m, n } => {
                let m = arm_register_to_ga_operand(m);
                let n = arm_register_to_ga_operand(n);
                let result = Operand::Local("result".to_owned());

                vec![
                    GAOperation::And {
                        destination: result.clone(),
                        operand1: n,
                        operand2: m,
                    },
                    GAOperation::SetNFlag(result.clone()),
                    GAOperation::SetZFlag(result),
                ]
            }
            Operation::UDFT1 { imm: _ } => {
                // generates a undefined exeption just panic for now
                unimplemented!()
            }
            Operation::UDFT2 { imm: _ } => {
                // generates a undefined exeption just panic for now
                unimplemented!()
            }
            Operation::UXTB { m, d } => vec![GAOperation::ZeroExtend {
                destination: arm_register_to_ga_operand(d),
                operand: arm_register_to_ga_operand(m),
                bits: 8,
            }],
            Operation::UXTH { m, d } => vec![GAOperation::ZeroExtend {
                destination: arm_register_to_ga_operand(d),
                operand: arm_register_to_ga_operand(m),
                bits: 16,
            }],
            Operation::WFE => todo!(),
            Operation::WFI => todo!(),
            Operation::YIELD => todo!(),
        };

        let instruction_width = match self.width {
            armv6_m_instruction_parser::instructons::InstructionWidth::Bit32 => 32,
            armv6_m_instruction_parser::instructons::InstructionWidth::Bit16 => 16,
        };

        let max_cycle_count = cycle_count_m0plus_core(&self.operation);

        GAInstruction {
            instruction_size: instruction_width,
            operations: operations,
            max_cycle: max_cycle_count,
        }
    }

    fn add_hooks(cfg: &mut RunConfig) {
        let symbolic_sized = |state: &mut GAState| {
            let value_ptr = state.get_register("R0".to_owned())?;
            let size = state
                .get_register("R1".to_owned())?
                .get_constant()
                .unwrap()
                * 8;
            trace!(
                "trying to create symbolic: addr: {:?}, size: {}",
                value_ptr,
                size
            );
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
            trace!("Pc read hook");
            let two = state.ctx.from_u64(1, 32);
            let pc = state.get_register("PC".to_owned()).unwrap();
            Ok(pc.add(&two))
        };

        let write_pc: RegisterWriteHook = |state, value | {
            trace!("Pc write hook");
            state.set_register("PC".to_owned(), value)
        };

        cfg.register_read_hooks.push(("PC+".to_owned(), read_pc));
        cfg.register_write_hooks.push(("PC+".to_owned(), write_pc));
    }
}

fn arm_register_to_ga_operand(reg: &Register) -> Operand {
    Operand::Register(match reg {
        Register::R0 => "R0".to_owned(),
        Register::R1 => "R1".to_owned(),
        Register::R2 => "R2".to_owned(),
        Register::R3 => "R3".to_owned(),
        Register::R4 => "R4".to_owned(),
        Register::R5 => "R5".to_owned(),
        Register::R6 => "R6".to_owned(),
        Register::R7 => "R7".to_owned(),
        Register::R8 => "R8".to_owned(),
        Register::R9 => "R9".to_owned(),
        Register::R10 => "R10".to_owned(),
        Register::R11 => "R11".to_owned(),
        Register::R12 => "R12".to_owned(),
        Register::SP => "SP".to_owned(),
        Register::LR => "LR".to_owned(),
        Register::PC => "PC+".to_owned(),
    })
}

fn arm_special_register_to_operand(reg: &SpecialRegister) -> Operand {
    Operand::Register(match reg {
        SpecialRegister::APSR => "APSR".to_owned(),
        SpecialRegister::IAPSR => "IAPSR".to_owned(),
        SpecialRegister::EAPSR => "EAPSR".to_owned(),
        SpecialRegister::XPSR => "XPSR".to_owned(),
        SpecialRegister::IPSR => "IPSR".to_owned(),
        SpecialRegister::EPSR => "EPSR".to_owned(),
        SpecialRegister::IEPSR => "IEPSR".to_owned(),
        SpecialRegister::MSP => "MSP".to_owned(),
        SpecialRegister::PSP => "PSP".to_owned(),
        SpecialRegister::PRIMASK => "PRIMASK".to_owned(),
        SpecialRegister::CONTROL => "CONTROL".to_owned(),
    })
}

fn arm_cond_to_ga_cond(conditon: &ArmCodition) -> Condition {
    match conditon {
        armv6_m_instruction_parser::conditions::Condition::EQ => Condition::EQ,
        armv6_m_instruction_parser::conditions::Condition::NE => Condition::NE,
        armv6_m_instruction_parser::conditions::Condition::CS => Condition::CS,
        armv6_m_instruction_parser::conditions::Condition::CC => Condition::CC,
        armv6_m_instruction_parser::conditions::Condition::MI => Condition::MI,
        armv6_m_instruction_parser::conditions::Condition::PL => Condition::PL,
        armv6_m_instruction_parser::conditions::Condition::VS => Condition::VS,
        armv6_m_instruction_parser::conditions::Condition::VC => Condition::VC,
        armv6_m_instruction_parser::conditions::Condition::HI => Condition::HI,
        armv6_m_instruction_parser::conditions::Condition::LS => Condition::LS,
        armv6_m_instruction_parser::conditions::Condition::GE => Condition::GE,
        armv6_m_instruction_parser::conditions::Condition::LT => Condition::LT,
        armv6_m_instruction_parser::conditions::Condition::GT => Condition::GT,
        armv6_m_instruction_parser::conditions::Condition::LE => Condition::LE,
        armv6_m_instruction_parser::conditions::Condition::None => Condition::None,
    }
}
