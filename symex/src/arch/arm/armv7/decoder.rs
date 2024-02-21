use std::sync::mpsc::channel;

use crate::general_assembly::{
    instruction::Instruction as GAInstruction,
    instruction::{self, Condition as GACondition, Operand as GAOperand, Operation},
    translator::Translatable,
    DataWord,
};
use dissarmv7::prelude::Thumb;
use dissarmv7::prelude::{Condition, Register, RegisterList, Thumb};
use paste::paste;
impl Into<GAOperand> for dissarmv7::prelude::Register {
    fn local_into(self) -> GAOperand {
        GAOperand::Register(match self {
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
}
impl Into<Option<GAOperand>> for Option<dissarmv7::prelude::Register> {
    fn local_into(self) -> Option<GAOperand> {
        if self.is_none() {
            return None;
        }
        Some(GAOperand::Register(match self.unwrap() {
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
        }))
    }
}

macro_rules! consume {
    (($($id:ident),*) from $name:ident) => {

        let ($($id),*) = {
            paste!(
                let consumer = $name.consumer();
                $(
                    let ($id,consumer) = consumer.[<consume_ $id>]();
                )*
                consumer.consume();
            );
            ($($id),*)
        };
    };
}
macro_rules! backup {
    ($($id:ident),*) => {
        {

            paste!(
                $(
                    let [<backup_ $id>] = GAOperand::Local(format!("backup_{}",stringify!($id)));
                )*
                let ret = vec![
                    $(
                        Operation::Move { destination: [<backup_ $id>], source: $id }
                    ),*
                ];
            );
            paste!(
                (ret,$([<backup_ $id>]),*)
            )
        }
    };
}

impl Into<GAOperand> for u32 {
    fn local_into(self) -> GAOperand {
        GAOperand::Immidiate(DataWord::Word32(self))
    }
}

impl Into<Vec<Operation>> for Thumb {
    fn local_into(self) -> Vec<Operation> {
        match self {
            Thumb::AdcImmediate(adc) => {
                // Ensure that all fields are used
                consume!((s,rd,rn,imm) from adc);
                let (rd, rn, imm) = (rd.local_into(), rn.local_into(), imm.local_into());
                let rd = rd.unwrap_or(rn);
                let (mut ret, backup_rn) = backup!(rn);
                ret.extend([Operation::Adc {
                    destination: rd,
                    operand1: imm,
                    operand2: rn,
                }]);
                if let Some(true) = s {
                    ret.extend([
                        Operation::Add {
                            destination: rd,
                            operand1: rn,
                            operand2: imm,
                        },
                        Operation::SetNFlag(rd),
                        Operation::SetZFlag(rd),
                        Operation::SetCFlag {
                            operand1: backup_rn.clone(),
                            operand2: imm.clone(),
                            sub: false,
                            carry: true,
                        },
                        Operation::SetVFlag {
                            operand1: backup_rn.clone(),
                            operand2: imm.clone(),
                            sub: false,
                            carry: true,
                        },
                    ]);
                }
                ret
            }
            Thumb::AdcRegister(adc) => {
                consume!((s,rd,rn,rm,shift) from adc);
                let (rd, rn, rm) = (rd.local_into(), rn.local_into(), rm.local_into());
                let rd = rd.unwrap_or(rn);
                let (mut ret, local_rn, local_rm) = backup!(rn, rm);
                ret.extend([Operation::Adc {
                    destination: rd,
                    operand1: rm,
                    operand2: rn,
                }]);
                if let Some(true) = s {
                    ret.extend([
                        Operation::Add {
                            destination: rd,
                            operand1: rn,
                            operand2: rm,
                        },
                        Operation::SetNFlag(rd),
                        Operation::SetZFlag(rd),
                        Operation::SetCFlag {
                            operand1: local_rn.clone(),
                            operand2: local_rm.clone(),
                            sub: false,
                            carry: true,
                        },
                        Operation::SetVFlag {
                            operand1: local_rn,
                            operand2: local_rm,
                            sub: false,
                            carry: true,
                        },
                    ]);
                }
                ret
            }
            Thumb::AddImmediate(add) => {
                consume!((s,rd,rn,imm) from add);
                let (rd, rn, imm) = (
                    rd.unwrap_or(rn).local_into(),
                    rn.local_into(),
                    imm.local_into(),
                );
                let (mut ret, local_rn) = backup!(rn);

                ret.push(Operation::Add {
                    destination: rd,
                    operand1: rn,
                    operand2: imm,
                });
                if let Some(true) = s {
                    ret.extend([
                        Operation::Add {
                            destination: rd,
                            operand1: rn,
                            operand2: imm,
                        },
                        Operation::SetNFlag(rd),
                        Operation::SetZFlag(rd),
                        Operation::SetCFlag {
                            operand1: local_rn.clone(),
                            operand2: imm.clone(),
                            sub: false,
                            carry: false,
                        },
                        Operation::SetVFlag {
                            operand1: local_rn,
                            operand2: imm,
                            sub: false,
                            carry: false,
                        },
                    ]);
                }
                ret
            }
            Thumb::AddRegister(add) => {
                consume!((s,rd,rn,rm,shift) from add);
                let (rd, rn, rm) = (rd.local_into(), rn.local_into(), rm.local_into());
                let rd = rd.unwrap_or(rn);
                let (mut ret, local_rn, local_rm) = backup!(rn, rm);
                ret.extend([Operation::Adc {
                    destination: rd,
                    operand1: rm,
                    operand2: rn,
                }]);
                if let Some(true) = s {
                    ret.extend([
                        Operation::Add {
                            destination: rd,
                            operand1: rn,
                            operand2: rm,
                        },
                        Operation::SetNFlag(rd),
                        Operation::SetZFlag(rd),
                        Operation::SetCFlag {
                            operand1: local_rn.clone(),
                            operand2: local_rm.clone(),
                            sub: false,
                            carry: false,
                        },
                        Operation::SetVFlag {
                            operand1: local_rn,
                            operand2: local_rm,
                            sub: false,
                            carry: false,
                        },
                    ]);
                }
                ret
            }
            Thumb::AddSPImmediate(add) => {
                consume!((s,rd,imm) from add);
                let (rd, rn, imm) = (
                    rd.unwrap_or(Register::SP).local_into(),
                    Register::SP.local_into(),
                    imm.local_into(),
                );
                let (mut ret, local_rn) = backup!(rn);

                ret.push(Operation::Add {
                    destination: rd,
                    operand1: rn,
                    operand2: imm,
                });
                if let Some(true) = s {
                    ret.extend([
                        Operation::Add {
                            destination: rd,
                            operand1: rn,
                            operand2: imm,
                        },
                        Operation::SetNFlag(rd),
                        Operation::SetZFlag(rd),
                        Operation::SetCFlag {
                            operand1: local_rn.clone(),
                            operand2: imm.clone(),
                            sub: false,
                            carry: false,
                        },
                        Operation::SetVFlag {
                            operand1: local_rn,
                            operand2: imm,
                            sub: false,
                            carry: false,
                        },
                    ]);
                }
                ret
            }
            Thumb::AddSPRegister(add) => {
                consume!((s,rd,rm,shift) from add);

                let s = match rd {
                    Some(Register::PC) => Some(false),
                    _ => s,
                };

                let (rd, rn, rm) = (
                    rd.unwrap_or(Register::SP).local_into(),
                    Register::SP.local_into(),
                    rm.local_into(),
                );
                let (mut ret, local_rn, local_rm) = backup!(rn, rm);
                ret.extend([Operation::Adc {
                    destination: rd,
                    operand1: rm,
                    operand2: rn,
                }]);
                if let Some(true) = s {
                    ret.extend([
                        Operation::Add {
                            destination: rd,
                            operand1: rn,
                            operand2: rm,
                        },
                        Operation::SetNFlag(rd),
                        Operation::SetZFlag(rd),
                        Operation::SetCFlag {
                            operand1: local_rn.clone(),
                            operand2: local_rm.clone(),
                            sub: false,
                            carry: false,
                        },
                        Operation::SetVFlag {
                            operand1: local_rn,
                            operand2: local_rm,
                            sub: false,
                            carry: false,
                        },
                    ]);
                }
                ret
            }
            Thumb::Adr(adr) => {
                consume!((rd,imm,add) from adr);
                let (rd,imm) = (rd.local_into(),imm.local_into());
                vec![]

            },
            Thumb::AndImmediate(_) => todo!(),
            Thumb::AndRegister(_) => todo!(),
            Thumb::AsrImmediate(_) => todo!(),
            Thumb::AsrRegister(_) => todo!(),
            Thumb::B(_) => todo!(),
            Thumb::Bfc(_) => todo!(),
            Thumb::Bfi(_) => todo!(),
            Thumb::BicImmediate(_) => todo!(),
            Thumb::BicRegister(_) => todo!(),
            Thumb::Bkpt(_) => todo!(),
            Thumb::Bl(_) => todo!(),
            Thumb::Blx(_) => todo!(),
            Thumb::Bx(_) => todo!(),
            Thumb::Cbz(_) => todo!(),
            Thumb::Clrex(_) => todo!(),
            Thumb::Clz(_) => todo!(),
            Thumb::CmnImmediate(_) => todo!(),
            Thumb::CmnRegister(_) => todo!(),
            Thumb::CmpImmediate(_) => todo!(),
            Thumb::CmpRegister(_) => todo!(),
            Thumb::Cps(_) => todo!(),
            Thumb::Dbg(_) => todo!(),
            Thumb::Dmb(_) => todo!(),
            Thumb::Dsb(_) => todo!(),
            Thumb::EorImmediate(_) => todo!(),
            Thumb::EorRegister(_) => todo!(),
            Thumb::Isb(_) => todo!(),
            Thumb::It(_) => todo!(),
            Thumb::Ldm(_) => todo!(),
            Thumb::Ldmdb(_) => todo!(),
            Thumb::LdrImmediate(_) => todo!(),
            Thumb::LdrLiteral(_) => todo!(),
            Thumb::LdrRegister(_) => todo!(),
            Thumb::LdrbImmediate(_) => todo!(),
            Thumb::LdrbLiteral(_) => todo!(),
            Thumb::LdrbRegister(_) => todo!(),
            Thumb::Ldrbt(_) => todo!(),
            Thumb::LdrdImmediate(_) => todo!(),
            Thumb::LdrdLiteral(_) => todo!(),
            Thumb::Ldrex(_) => todo!(),
            Thumb::Ldrexb(_) => todo!(),
            Thumb::Ldrexh(_) => todo!(),
            Thumb::LdrhImmediate(_) => todo!(),
            Thumb::LdrhLiteral(_) => todo!(),
            Thumb::LdrhRegister(_) => todo!(),
            Thumb::Ldrht(_) => todo!(),
            Thumb::LdrsbImmediate(_) => todo!(),
            Thumb::LdrsbLiteral(_) => todo!(),
            Thumb::LdrsbRegister(_) => todo!(),
            Thumb::Ldrsbt(_) => todo!(),
            Thumb::LdrshImmediate(_) => todo!(),
            Thumb::LdrshLiteral(_) => todo!(),
            Thumb::LdrshRegister(_) => todo!(),
            Thumb::Ldrsht(_) => todo!(),
            Thumb::Ldrt(_) => todo!(),
            Thumb::LslImmediate(_) => todo!(),
            Thumb::LslRegister(_) => todo!(),
            Thumb::LsrImmediate(_) => todo!(),
            Thumb::LsrRegister(_) => todo!(),
            Thumb::Mla(_) => todo!(),
            Thumb::Mls(_) => todo!(),
            Thumb::MovImmediate(_) => todo!(),
            Thumb::MovImmediatePlain(_) => todo!(),
            Thumb::MovReg(_) => todo!(),
            Thumb::Movt(_) => todo!(),
            Thumb::Mrs(_) => todo!(),
            Thumb::Msr(_) => todo!(),
            Thumb::Mul(_) => todo!(),
            Thumb::MvnImmediate(_) => todo!(),
            Thumb::MvnRegister(_) => todo!(),
            Thumb::Nop(_) => todo!(),
            Thumb::OrnImmediate(_) => todo!(),
            Thumb::OrnRegister(_) => todo!(),
            Thumb::OrrImmediate(_) => todo!(),
            Thumb::OrrRegister(_) => todo!(),
            Thumb::Pkh(_) => todo!(),
            Thumb::PldImmediate(_) => todo!(),
            Thumb::PldLiteral(_) => todo!(),
            Thumb::PldRegister(_) => todo!(),
            Thumb::PliImmediate(_) => todo!(),
            Thumb::PliRegister(_) => todo!(),
            Thumb::Pop(_) => todo!(),
            Thumb::Push(_) => todo!(),
            Thumb::Qadd(_) => todo!(),
            Thumb::Qadd16(_) => todo!(),
            Thumb::Qadd8(_) => todo!(),
            Thumb::Qasx(_) => todo!(),
            Thumb::Qdadd(_) => todo!(),
            Thumb::Qdsub(_) => todo!(),
            Thumb::Qsax(_) => todo!(),
            Thumb::Qsub(_) => todo!(),
            Thumb::Qsub16(_) => todo!(),
            Thumb::Qsub8(_) => todo!(),
            Thumb::Rbit(_) => todo!(),
            Thumb::Rev(_) => todo!(),
            Thumb::Rev16(_) => todo!(),
            Thumb::Revsh(_) => todo!(),
            Thumb::RorImmediate(_) => todo!(),
            Thumb::RorRegister(_) => todo!(),
            Thumb::Rrx(_) => todo!(),
            Thumb::RsbImmediate(_) => todo!(),
            Thumb::RsbRegister(_) => todo!(),
            Thumb::Sadd16(_) => todo!(),
            Thumb::Sadd8(_) => todo!(),
            Thumb::Sasx(_) => todo!(),
            Thumb::SbcImmediate(_) => todo!(),
            Thumb::SbcRegister(_) => todo!(),
            Thumb::Sbfx(_) => todo!(),
            Thumb::Sdiv(_) => todo!(),
            Thumb::Sel(_) => todo!(),
            Thumb::Sev(_) => todo!(),
            Thumb::Shadd16(_) => todo!(),
            Thumb::Shadd8(_) => todo!(),
            Thumb::Shasx(_) => todo!(),
            Thumb::Shsax(_) => todo!(),
            Thumb::Shsub16(_) => todo!(),
            Thumb::Shsub8(_) => todo!(),
            Thumb::Smla(_) => todo!(),
            Thumb::Smlad(_) => todo!(),
            Thumb::Smlal(_) => todo!(),
            Thumb::SmlalSelective(_) => todo!(),
            Thumb::Smlald(_) => todo!(),
            Thumb::Smlaw(_) => todo!(),
            Thumb::Smlsd(_) => todo!(),
            Thumb::Smlsld(_) => todo!(),
            Thumb::Smmla(_) => todo!(),
            Thumb::Smmls(_) => todo!(),
            Thumb::Smmul(_) => todo!(),
            Thumb::Smuad(_) => todo!(),
            Thumb::Smul(_) => todo!(),
            Thumb::Smull(_) => todo!(),
            Thumb::Smulw(_) => todo!(),
            Thumb::Smusd(_) => todo!(),
            Thumb::Ssat(_) => todo!(),
            Thumb::Ssat16(_) => todo!(),
            Thumb::Ssax(_) => todo!(),
            Thumb::Ssub16(_) => todo!(),
            Thumb::Ssub8(_) => todo!(),
            Thumb::Stm(_) => todo!(),
            Thumb::Stmdb(_) => todo!(),
            Thumb::StrImmediate(_) => todo!(),
            Thumb::StrRegister(_) => todo!(),
            Thumb::StrbImmediate(_) => todo!(),
            Thumb::StrbRegister(_) => todo!(),
            Thumb::Strbt(_) => todo!(),
            Thumb::StrdImmediate(_) => todo!(),
            Thumb::Strex(_) => todo!(),
            Thumb::Strexb(_) => todo!(),
            Thumb::Strexh(_) => todo!(),
            Thumb::StrhImmediate(_) => todo!(),
            Thumb::StrhRegister(_) => todo!(),
            Thumb::Strht(_) => todo!(),
            Thumb::Strt(_) => todo!(),
            Thumb::SubImmediate(_) => todo!(),
            Thumb::SubRegister(_) => todo!(),
            Thumb::SubSpMinusImmediate(_) => todo!(),
            Thumb::SubSpMinusReg(_) => todo!(),
            Thumb::Sxtab(_) => todo!(),
            Thumb::Sxtab16(_) => todo!(),
            Thumb::Sxtah(_) => todo!(),
            Thumb::Sxtb(_) => todo!(),
            Thumb::Sxtb16(_) => todo!(),
            Thumb::Sxth(_) => todo!(),
            Thumb::Tb(_) => todo!(),
            Thumb::TeqImmediate(_) => todo!(),
            Thumb::TeqRegister(_) => todo!(),
            Thumb::TstImmediate(_) => todo!(),
            Thumb::TstRegister(_) => todo!(),
            Thumb::Uadd16(_) => todo!(),
            Thumb::Uadd8(_) => todo!(),
            Thumb::Uasx(_) => todo!(),
            Thumb::Ubfx(_) => todo!(),
            Thumb::Udf(_) => todo!(),
            Thumb::Udiv(_) => todo!(),
            Thumb::Uhadd16(_) => todo!(),
            Thumb::Uhadd8(_) => todo!(),
            Thumb::Uhasx(_) => todo!(),
            Thumb::Uhsax(_) => todo!(),
            Thumb::Uhsub16(_) => todo!(),
            Thumb::Uhsub8(_) => todo!(),
            Thumb::Umaal(_) => todo!(),
            Thumb::Umlal(_) => todo!(),
            Thumb::Umull(_) => todo!(),
            Thumb::Uqadd16(_) => todo!(),
            Thumb::Uqadd8(_) => todo!(),
            Thumb::Uqasx(_) => todo!(),
            Thumb::Uqsax(_) => todo!(),
            Thumb::Uqsub16(_) => todo!(),
            Thumb::Uqsub8(_) => todo!(),
            Thumb::Uqsad8(_) => todo!(),
            Thumb::Usada8(_) => todo!(),
            Thumb::Usad8(_) => todo!(),
            Thumb::Usat(_) => todo!(),
            Thumb::Usat16(_) => todo!(),
            Thumb::Usax(_) => todo!(),
            Thumb::Usub16(_) => todo!(),
            Thumb::Usub8(_) => todo!(),
            Thumb::Uxtab(_) => todo!(),
            Thumb::Uxtab16(_) => todo!(),
            Thumb::Uxtah(_) => todo!(),
            Thumb::Uxtb(_) => todo!(),
            Thumb::Uxtb16(_) => todo!(),
            Thumb::Uxth(_) => todo!(),
            Thumb::Wfe(_) => todo!(),
            Thumb::Wfi(_) => todo!(),
            Thumb::Yield(_) => todo!(),
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
}

impl Translatable for Thumb {
    fn translate(&self) -> GAInstruction {
        // let op: Vec<Operation> = self.local_into();
        GAInstruction {
            instruction_size: 32,
            operations: vec![Operation::Nop],
            max_cycle: instruction::CycleCount::Value(1),
        }
    }
}

mod sealed {
    pub trait Into<T> {
        fn local_into(self) -> T;
    }
}

use sealed::Into;

impl sealed::Into<GACondition> for dissarmv7::prelude::Condition {
    fn local_into(self) -> GACondition {
        match self {
            Self::Eq => GACondition::EQ,
            Self::Ne => GACondition::NE,
            Self::Mi => GACondition::MI,
            Self::Pl => GACondition::PL,
            Self::Vs => GACondition::VS,
            Self::Vc => GACondition::VC,
            Self::Hi => GACondition::HI,
            Self::Ge => GACondition::GE,
            Self::Lt => GACondition::LT,
            Self::Gt => GACondition::GT,
            Self::Ls => GACondition::LS,
            Self::Le => GACondition::LE,
            Self::Cs => GACondition::CS,
            Self::Cc => GACondition::CC,
            Self::None => GACondition::None,
        }
    }
}
/*fn arm_special_register_to_operand(reg: &SpecialRegister) -> Operand {
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
}*/
