use std::sync::mpsc::channel;

use crate::{
    arch::Arch,
    general_assembly::{
        instruction::{
            self, Condition as GACondition, Instruction as GAInstruction, Operand, Operation,
            Shift as GAShift,
        },
        translator::Translatable,
    },
};
use dissarmv7::prelude::Shift;
use dissarmv7::prelude::{Condition, Register, RegisterList, Thumb};
use general_assembly::operand::DataWord;
use paste::paste;
use transpiler::pseudo;

impl sealed::ToString for Register {
    fn to_string(self) -> String {
        match self {
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
        }
    }
}

impl sealed::Into<Operand> for dissarmv7::prelude::Register {
    fn local_into(self) -> Operand {
        Operand::Register(self.to_string())
    }
}
impl sealed::Into<Option<Operand>> for Option<dissarmv7::prelude::Register> {
    fn local_into(self) -> Option<Operand> {
        if self.is_none() {
            return None;
        }
        Some(Operand::Register(self.unwrap().to_string()))
    }
}
impl sealed::Into<GAShift> for Shift {
    fn local_into(self) -> GAShift {
        match self {
            Self::Lsl => GAShift::Lsl,
            Self::Lsr => GAShift::Lsr,
            Self::Asr => GAShift::Asr,
            Self::Rrx => GAShift::Rrx,
            Self::Ror => GAShift::Ror,
        }
    }
}
macro_rules! consume {
    (($($id:ident$($(.$e:expr)+)?),*) from $name:ident) => {
        #[allow(unused_parens)]
        let ($($id),*) = {
            paste!(
                let consumer = $name.consumer();
                $(
                    let ($id,consumer) = consumer.[<consume_ $id>]();
                    let $id = $id$($(.$e)?)*;
                )*
                consumer.consume();
            );
            ($($id),*)
        };
    };
}
macro_rules! shift {
    ($ret:ident.$shift:ident $reg:ident -> $target:ident $(set c for $reg_flag:ident)?) => {
       if let Some(shift) = $shift {
            let (shift_t, shift_n) = (
                    shift.shift_t.local_into(),
                    (shift.shift_n as u32).local_into(),
            );
            $($ret.push( match shift_t{
                GAShift::Lsl => Operation::SetCFlagShiftLeft { operand: $reg_flag.clone(), shift: shift_n.clone() },
                GAShift::Asr => Operation::SetCFlagSra { operand: $reg_flag.clone(), shift: shift_n.clone() },
                GAShift::Lsr => Operation::SetCFlagSrl { operand: $reg_flag.clone(), shift: shift_n.clone() },
                GAShift::Rrx => todo!("This needs some work, https://developer.arm.com/documentation/ddi0406/b/Application-Level-Architecture/Application-Level-Programmers--Model/ARM-core-data-types-and-arithmetic/Integer-arithmetic?lang=en"),
                GAShift::Ror => todo!("This needs to be revisited, seems that the current implementation depends on this being done after the operation is performed")
            });)?
            $ret.push(
                Operation::Shift {
                    destination: $target.clone(),
                    operand: $reg.clone(),
                    shift_n,
                    shift_t,
            })
       }
    };
}
macro_rules! shift_imm {
    ($ret:ident.($shift_t:ident,$($shift_n_const:literal)?$($shift_n:ident)?) $reg:ident -> $target:ident $(set c for $reg_flag:ident)?) => {
        {
            let (shift_t, shift_n) = (
                    $shift_t,
                    $($shift_n)?$($shift_n_const)?,
            );
            $($ret.push( match shift_t{
                GAShift::Lsl => Operation::SetCFlagShiftLeft { operand: $reg_flag.clone(), shift: shift_n.clone() },
                GAShift::Asr => Operation::SetCFlagSra { operand: $reg_flag.clone(), shift: shift_n.clone() },
                GAShift::Lsr => Operation::SetCFlagSrl { operand: $reg_flag.clone(), shift: shift_n.clone() },
                GAShift::Rrx => todo!("This needs some work, https://developer.arm.com/documentation/ddi0406/b/Application-Level-Architecture/Application-Level-Programmers--Model/ARM-core-data-types-and-arithmetic/Integer-arithmetic?lang=en"),
                GAShift::Ror => todo!("This needs to be revisited, seems that the current implementation depends on this being done after the operation is performed")
            });)?
            $ret.push(
                Operation::Shift {
                    destination: $target.clone(),
                    operand: $reg.clone(),
                    shift_n,
                    shift_t,
            })
        }
    };
}
macro_rules! backup {
    ($($id:ident),*) => {
        {

            paste!(
                $(
                    let [<backup_ $id>] = Operand::Local(format!("backup_{}",stringify!($id)));
                )*
                let ret = vec![
                    $(
                        Operation::Move { destination: [<backup_ $id>].clone(), source: $id.clone() }
                    ),*
                ];
            );
            paste!(
                (ret,$([<backup_ $id>]),*)
            )
        }
    };
}
macro_rules! local {
    ($($id:ident),*) => {
        $(
            let $id = Operand::Local(stringify!($id).to_owned());
        )*
    };
}
// These two need to be broken out in to a proc macro to allow any generic expressions and some
// neater syntax
macro_rules! bin_op {
    ($($d:ident = $lhs:ident + $rhs:expr),*) => {
        $(Operation::Add { destination: $d.clone(), operand1: $lhs.clone(), operand2: $rhs.clone()}),*
    };
    // Add carry bit
    ($($d:ident = $lhs:ident + $rhs:ident + c),*) => {
        $(Operation::Adc { destination: $d.clone(), operand1: $lhs.clone(), operand2: $rhs.clone()}),*
    };
    // Add carry bit
    ($($d:ident = $lhs:ident adc $rhs:expr),*) => {
        $(Operation::Adc { destination: $d.clone(), operand1: $lhs.clone(), operand2: $rhs.clone()}),*
    };

    ($($d:ident = $lhs:ident - $rhs:expr),*) => {
        $(Operation::Sub { destination: $d.clone(), operand1: $lhs.clone(), operand2: $rhs.clone()}),*
    };
    ($($d:ident = $lhs:ident * $rhs:expr),*) => {
        $(Operation::Mul { destination: $d.clone(), operand1: $lhs.clone(), operand2: $rhs.clone()}),*
    };
    ($($d:ident = $lhs:ident & $rhs:expr),*) => {
        $(Operation::And { destination: $d.clone(), operand1: $lhs.clone(), operand2: $rhs.clone()}),*
    };
    ($($d:ident = $lhs:ident | $rhs:expr),*) => {
        $(Operation::Or { destination: $d.clone(), operand1: $lhs.clone(), operand2: $rhs.clone()}),*
    };
    ($($d:ident = $lhs:ident ^ $rhs:expr),*) => {
        $(Operation::Xor { destination: $d.clone(), operand1: $lhs.clone(), operand2: $rhs.clone()}),*
    };
    // Default to srl
    ($($d:ident = $lhs:ident >> $rhs:expr),*) => {
        $(Operation::Srl { destination: $d.clone(), operand: $lhs.clone(), shift: $rhs.clone()}),*
    };
    ($($d:ident = $lhs:ident << $rhs:expr),*) => {
        $(Operation::Sl { destination: $d.clone(), operand: $lhs.clone(), shift: $rhs.clone()}),*
    };
    ($($d:ident = $lhs:ident sra $rhs:expr),*) => {
        $(Operation::Sra { destination: $d.clone(), operand: $lhs.clone(), shift: $rhs.clone()}),*
    };
    ($($d:ident = $rhs:ident),*) => {
        $(Operation::Move { destination: $d.clone(), source: $rhs.clone()}),*
    };
    ($($d:ident = ! $rhs:ident),*) => {
        $(Operation::Not { destination: $d.clone(), operand: $rhs.clone()}),*
    };
    // ($d:ident = SInt($op:ident,$n:ident)) => {
    //
    // }
}

impl Into<Operand> for u32 {
    fn local_into(self) -> Operand {
        Operand::Immidiate(DataWord::Word32(self))
    }
}
const fn mask<const START: usize, const END: usize>() -> u32 {
    let mask = ((1 << (END - START + 1) as u32) as u32) - 1 as u32;
    mask
}
fn mask_dyn(start: u32, end: u32) -> u32 {
    let mask = ((1 << (end - start + 1) as u32) as u32) - 1 as u32;
    mask
}
macro_rules! mask {
    ($($id:ident : $start:literal -> $end:literal),*) => {
        {
            let mut ret = vec![]

            paste!(
                $(
                    let [<shifted_ $id>] = Operand::Local(format!("shifted_{}",stringify!($id)));
                )*
                ret.extend([
                    $(
                        Operation::Shift {
                            destination: [<shifted_ $id>],
                            operand: $id,
                            shift_n: $start.local_into(),
                            shift_t: Shift::Lsl,
                        }
                    ),*
                ]);
            );
            paste!(
                (ret,$([<backup_ $id>]),*)
            )
        }

    };
}

impl Into<Vec<Operation>> for Thumb {
    fn local_into(self) -> Vec<Operation> {
        'outer_block: {
            match self {
            Thumb::AdcImmediate(adc) => {
                // Ensure that all fields are used
                consume!((s,rd,rn,imm) from adc);
                let (rd, rn, imm): (Option<Operand>, Operand, Operand) =
                    (rd.local_into(), rn.local_into(), imm.local_into());
                let rd = rd.unwrap_or(rn.clone());
                let (mut ret, backup_rn) = backup!(rn);
                ret.extend([Operation::Adc {
                    destination: rd.clone(),
                    operand1: imm.clone(),
                    operand2: rn.clone(),
                }]);
                if let Some(true) = s {
                    ret.extend([
                        Operation::Add {
                            destination: rd.clone(),
                            operand1: rn.clone(),
                            operand2: imm.clone(),
                        },
                        Operation::SetNFlag(rd.clone()),
                        Operation::SetZFlag(rd.clone()),
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
                let rd = rd.unwrap_or(rn.clone());
                let (mut ret, local_rn, local_rm) = backup!(rn, rm);
                ret.extend([Operation::Adc {
                    destination: rd.clone(),
                    operand1: rm.clone(),
                    operand2: rn.clone(),
                }]);
                if let Some(true) = s {
                    ret.extend([
                        Operation::Add {
                            destination: rd.clone(),
                            operand1: rn.clone(),
                            operand2: rm.clone(),
                        },
                        Operation::SetNFlag(rd.clone()),
                        Operation::SetZFlag(rd.clone()),
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
                    destination: rd.clone(),
                    operand1: rn.clone(),
                    operand2: imm.clone(),
                });
                if let Some(true) = s {
                    ret.extend([
                        Operation::Add {
                            destination: rd.clone(),
                            operand1: rn.clone(),
                            operand2: imm.clone(),
                        },
                        Operation::SetNFlag(rd.clone()),
                        Operation::SetZFlag(rd.clone()),
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
                let rd = rd.unwrap_or(rn.clone());
                let (mut ret, local_rn, local_rm) = backup!(rn, rm);
                ret.extend([Operation::Adc {
                    destination: rd.clone(),
                    operand1: rm.clone(),
                    operand2: rn.clone(),
                }]);
                if let Some(true) = s {
                    ret.extend([
                        Operation::Add {
                            destination: rd.clone(),
                            operand1: rn.clone(),
                            operand2: rm.clone(),
                        },
                        Operation::SetNFlag(rd.clone()),
                        Operation::SetZFlag(rd.clone()),
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
                    destination: rd.clone(),
                    operand1: rn.clone(),
                    operand2: imm.clone(),
                });
                if let Some(true) = s {
                    ret.extend([
                        Operation::Add {
                            destination: rd.clone(),
                            operand1: rn.clone(),
                            operand2: imm.clone(),
                        },
                        Operation::SetNFlag(rd.clone()),
                        Operation::SetZFlag(rd.clone()),
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
                    destination: rd.clone(),
                    operand1: rm.clone(),
                    operand2: rn.clone(),
                }]);
                if let Some(true) = s {
                    ret.extend([
                        Operation::Add {
                            destination: rd.clone(),
                            operand1: rn.clone(),
                            operand2: rm.clone(),
                        },
                        Operation::SetNFlag(rd.clone()),
                        Operation::SetZFlag(rd.clone()),
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
                let (rd, imm) = (rd.local_into(), imm.local_into());

                let mut ret = vec![
                    Operation::Add {
                        destination: Operand::Local("addr".to_owned()),
                        operand1: Operand::Register("PC".to_owned()),
                        operand2: Operand::Immidiate(DataWord::Word32(2)),
                    },
                    Operation::And {
                        destination: Operand::Local("addr".to_owned()),
                        operand1: Operand::Local("addr".to_owned()),
                        operand2: Operand::Immidiate(DataWord::Word32(!0b11)),
                    },
                ];
                ret.push(match add {
                    true => Operation::Add {
                        destination: rd,
                        operand1: Operand::Local("addr".to_owned()),
                        operand2: imm,
                    },
                    false => Operation::Sub {
                        destination: rd,
                        operand1: Operand::Local("addr".to_owned()),
                        operand2: imm,
                    },
                });
                ret
            }
            Thumb::AndImmediate(and) => {
                todo!("Need to figure out how to do thumb_expand_imm with ASPR.C");
                // TODO! Add conditional execution of parts of Operation vector,
                // this would allow meta functions such as ThumbExpandImm_C
                //
                //
                // https://developer.arm.com/documentation/ddi0403/d/Application-Level-Architecture/The-Thumb-Instruction-Set-Encoding/32-bit-Thumb-instruction-encoding/Modified-immediate-constants-in-Thumb-instructions?lang=en
                // consume!((s,rd,rn,imm) from and);
                // let (rd,rn,imm) = (rd.unwrap_or(rn).local_into(),rn.local_into(),imm.in
            }
            Thumb::AndRegister(and) => {
                consume!((s,rd,rn,rm,shift) from and);
                let (rd, rn, rm) = (
                    rd.unwrap_or(rn).local_into(),
                    rn.local_into(),
                    rm.local_into(),
                );
                let mut ret = match shift {
                    Some(shift) => {
                        let (shift_t, shift_n) = (
                            shift.shift_t.local_into(),
                            (shift.shift_n as u32).local_into(),
                        );
                        let flag_setter = match shift_t{
                            GAShift::Lsl => Operation::SetCFlagShiftLeft { operand: rm.clone(), shift: shift_n.clone() },
                            GAShift::Asr => Operation::SetCFlagSra { operand: rm.clone(), shift: shift_n.clone() },
                            GAShift::Lsr => Operation::SetCFlagSrl { operand: rm.clone(), shift: shift_n.clone() },
                            GAShift::Rrx => todo!("This needs some work, https://developer.arm.com/documentation/ddi0406/b/Application-Level-Architecture/Application-Level-Programmers--Model/ARM-core-data-types-and-arithmetic/Integer-arithmetic?lang=en"),
                            GAShift::Ror => todo!("This needs to be revisited, seems that the current implementation depends on this being done after the operation is performed")
                        };
                        vec![
                            flag_setter,
                            Operation::Shift {
                                destination: rm.clone(),
                                operand: rm.clone(),
                                shift_n,
                                shift_t,
                            },
                        ]
                    }
                    None => vec![],
                };
                ret.push(Operation::And {
                    destination: rd.clone(),
                    operand1: rn,
                    operand2: rm,
                });
                if let Some(true) = s {
                    // The shift should already set the shift carry bit
                    ret.extend([Operation::SetNFlag(rd.clone()), Operation::SetZFlag(rd)]);
                }
                ret
            }
            Thumb::AsrImmediate(asr) => {
                consume!((s,rd,rm,imm) from asr);
                let (rd, rm, imm) = (rd.local_into(), rm.local_into(), imm.local_into());
                let mut ret = vec![Operation::Sra {
                    destination: rd.clone(),
                    operand: rm.clone(),
                    shift: imm.clone(),
                }];
                if let Some(true) = s {
                    ret.extend([
                        Operation::SetNFlag(rd.clone()),
                        Operation::SetZFlag(rd.clone()),
                        Operation::SetCFlagSra {
                            operand: rm,
                            shift: imm,
                        },
                    ]);
                }
                ret
            }
            Thumb::AsrRegister(asr) => {
                consume!((s,rd,rm,rn) from asr);
                let (rd, rm, rn) = (rd.local_into(), rm.local_into(), rn.local_into());
                let intermediate = Operand::Local("intermediate".to_owned());
                let mut ret = vec![
                    // Extract 8- least significant bits
                    Operation::And {
                        destination: intermediate.clone(),
                        operand1: rm.clone(),
                        operand2: Operand::Immidiate(DataWord::Word8(u8::MAX)),
                    },
                    Operation::Sra {
                        destination: rd.clone(),
                        operand: rn.clone(),
                        shift: intermediate.clone(),
                    },
                ];
                if let Some(true) = s {
                    ret.extend([
                        Operation::SetNFlag(rd.clone()),
                        Operation::SetZFlag(rd.clone()),
                        Operation::SetCFlagSra {
                            operand: rm,
                            shift: intermediate,
                        },
                    ]);
                }
                ret
            }
            Thumb::B(b) => {
                consume!((condition,imm) from b);
                todo!("This needs to be fixed, imm is signed here");
                let (condition, imm) = (condition.local_into(), imm.local_into());
                let intermediate = Operand::Local("intermediate".to_owned());
                vec![
                    Operation::Move {
                        destination: intermediate.clone(),
                        source: Register::PC.local_into().clone(),
                    },
                    Operation::Add {
                        destination: intermediate.clone(),
                        operand1: intermediate.clone(),
                        operand2: imm.clone(),
                    },
                    // Discard last bit
                    Operation::Srl {
                        destination: intermediate.clone(),
                        operand: intermediate.clone(),
                        shift: Operand::Immidiate(DataWord::Word8(0b1)),
                    },
                    Operation::Sl {
                        destination: intermediate.clone(),
                        operand: intermediate.clone(),
                        shift: Operand::Immidiate(DataWord::Word8(0b1)),
                    },
                    Operation::ConditionalJump {
                        destination: intermediate,
                        condition,
                    },
                ]
            }
            Thumb::Bfc(bfc) => {
                consume!((rd,lsb,msb) from bfc);
                let rd = rd.local_into();
                let mask = !mask_dyn(lsb, msb);
                vec![Operation::And {
                    destination: rd.clone(),
                    operand1: rd,
                    operand2: Operand::Immidiate(DataWord::Word32(mask)),
                }]
            }
            Thumb::Bfi(bfi) => {
                consume!((rd,rn,lsb,msb) from bfi);
                let (rd, rn) = (rd.local_into(), rn.local_into());
                let p_mask = mask_dyn(0, msb - lsb);
                let n_mask = !mask_dyn(lsb, msb);
                let intermediate = Operand::Local("intermediate".to_owned());
                vec![
                    // Clear out the field
                    Operation::And {
                        destination: rd.clone(),
                        operand1: rd.clone(),
                        operand2: Operand::Immidiate(DataWord::Word32(n_mask)),
                    },
                    // Clear out the field
                    Operation::And {
                        destination: intermediate.clone(),
                        operand1: rn,
                        operand2: Operand::Immidiate(DataWord::Word32(p_mask)),
                    },
                    // Align the fields
                    Operation::Sl {
                        destination: intermediate.clone(),
                        operand: intermediate.clone(),
                        shift: Operand::Immidiate(DataWord::Word32(lsb)),
                    },
                    // Replace the field
                    Operation::Or {
                        destination: rd.clone(),
                        operand1: rd,
                        operand2: intermediate,
                    },
                ]
            }
            Thumb::BicImmediate(bic) => {
                consume!((s,rd,rn,imm) from bic);
                let (rd, rn, imm) = (
                    rd.unwrap_or(rn.clone()).local_into(),
                    rn.local_into(),
                    imm.local_into(),
                );
                let intermediate = Operand::Local("intermediate".to_owned());
                let mut ret = vec![
                    Operation::Not {
                        destination: intermediate.clone(),
                        operand: imm,
                    },
                    Operation::And {
                        destination: rd.clone(),
                        operand1: rn.clone(),
                        operand2: intermediate.clone(),
                    },
                ];
                if let Some(true) = s {
                    ret.extend([
                        Operation::SetNFlag(rd.clone()),
                        Operation::SetZFlag(rd.clone()),
                    ]);
                }
                ret
            }
            Thumb::BicRegister(bic) => {
                consume!((s,rd,rn,rm,shift) from bic);

                let (rd, rn, rm) = (
                    rd.unwrap_or(rn).local_into(),
                    rn.local_into(),
                    rm.local_into(),
                );
                let mut ret = match shift {
                    Some(shift) => {
                        let (shift_t, shift_n) = (
                            shift.shift_t.local_into(),
                            (shift.shift_n as u32).local_into(),
                        );
                        let flag_setter = match shift_t{
                            GAShift::Lsl => Operation::SetCFlagShiftLeft { operand: rm.clone(), shift: shift_n.clone() },
                            GAShift::Asr => Operation::SetCFlagSra { operand: rm.clone(), shift: shift_n.clone() },
                            GAShift::Lsr => Operation::SetCFlagSrl { operand: rm.clone(), shift: shift_n.clone() },
                            GAShift::Rrx => todo!("This needs some work, https://developer.arm.com/documentation/ddi0406/b/Application-Level-Architecture/Application-Level-Programmers--Model/ARM-core-data-types-and-arithmetic/Integer-arithmetic?lang=en"),
                            GAShift::Ror => todo!("This needs to be revisited, seems that the current implementation depends on this being done after the operation is performed")
                        };
                        vec![
                            flag_setter,
                            Operation::Shift {
                                destination: rm.clone(),
                                operand: rm.clone(),
                                shift_n,
                                shift_t,
                            },
                        ]
                    }
                    None => vec![],
                };
                let intermediate = Operand::Local("intermediate".to_owned());
                ret.extend([
                    Operation::Not {
                        destination: intermediate.clone(),
                        operand: rm.clone(),
                    },
                    Operation::And {
                        destination: rd.clone(),
                        operand1: rn.clone(),
                        operand2: intermediate.clone(),
                    },
                ]);
                if let Some(true) = s {
                    ret.extend([
                        Operation::SetNFlag(rd.clone()),
                        Operation::SetZFlag(rd.clone()),
                    ]);
                }
                ret
            }
            Thumb::Bkpt(_) => vec![Operation::Nop],
            Thumb::Bl(bl) => {
                let imm = bl.imm;
                let add = imm >= 0;
                let target = Operand::Local("target".to_owned());
                // 1. Set intermediate
                let mut ret = match add {
                    true => {
                        let imm = imm as u32;
                        vec![Operation::Add {
                            destination: target.clone(),
                            operand1: Register::PC.local_into(),
                            operand2: imm.local_into(),
                        }]
                    }
                    false => {
                        let imm = (-imm) as u32;
                        vec![Operation::Sub {
                            destination: target.clone(),
                            operand1: Register::PC.local_into(),
                            operand2: imm.local_into(),
                        }]
                    }
                };
                // 1.5. discard last bit
                ret.extend([
                    Operation::Srl {
                        destination: target.clone(),
                        operand: target.clone(),
                        shift: Operand::Immidiate(DataWord::Word8(0b1)),
                    },
                    Operation::Sl {
                        destination: target.clone(),
                        operand: target.clone(),
                        shift: Operand::Immidiate(DataWord::Word8(0b1)),
                    },
                ]);
                // 2. Copy pc in to LR
                ret.extend([
                    Operation::Move {
                        destination: Register::LR.local_into(),
                        source: Register::PC.local_into(),
                    },
                    // Set LSB to 1, i.e. next instrution
                    Operation::Or {
                        destination: Register::LR.local_into(),
                        operand1: Register::LR.local_into(),
                        operand2: Operand::Immidiate(DataWord::Word8(0b1)),
                    },
                ]);
                // 3. Branch to the new target
                ret.push(Operation::ConditionalJump {
                    destination: target,
                    condition: Condition::None.local_into(),
                });
                ret
            }
            Thumb::Blx(blx) => {
                consume!((rm) from blx);
                let rm = rm.local_into();
                let target = Operand::Local("target".to_owned());
                let ret_addr = Operand::Local("ret_addr".to_owned());
                let intermediate = Operand::Local("intermediate".to_owned());
                let mask_intermediate = Operand::Local("mask".to_owned());
                let mut ret = vec![
                    Operation::Move {
                        destination: target.clone(),
                        source: rm.clone(),
                    },
                    Operation::Sub {
                        destination: ret_addr.clone(),
                        operand1: Register::PC.local_into(),
                        operand2: Operand::Immidiate(DataWord::Word8(0b10)),
                    },
                    // Set last bit to 1
                    Operation::Or {
                        destination: Register::LR.local_into(),
                        operand1: ret_addr.clone(),
                        operand2: Operand::Immidiate(DataWord::Word8(0b1)),
                    },
                    // BLXWritePc https://developer.arm.com/documentation/ddi0419/c/Application-Level-Architecture/Application-Level-Programmers--Model/Registers-and-execution-state/ARM-core-registers

                    // 1. Set the EPSR.T reg to the LSB in the target
                    Operation::And {
                        destination: intermediate.clone(),
                        operand1: target.clone(),
                        operand2: Operand::Immidiate(DataWord::Word32(1)),
                    },
                    Operation::Sl {
                        destination: intermediate.clone(),
                        operand: intermediate.clone(),
                        shift: 23.local_into(),
                    },
                    // TODO! Clear the bit here first
                    Operation::Sl {
                        destination: mask_intermediate.clone(),
                        operand: 0b1.local_into(),
                        shift: 23.local_into(),
                    },
                    Operation::Not {
                        destination: mask_intermediate.clone(),
                        operand: mask_intermediate.clone(),
                    },
                    Operation::And {
                        destination: SpecialRegister::EPSR.local_into(),
                        operand1: SpecialRegister::EPSR.local_into(),
                        operand2: mask_intermediate,
                    },
                    Operation::Or {
                        destination: SpecialRegister::EPSR.local_into(),
                        operand1: SpecialRegister::EPSR.local_into(),
                        // Set bit nr 24 to 1
                        operand2: intermediate,
                    },
                    // 2. Force final bit to 0
                    Operation::And {
                        destination: target.clone(),
                        operand1: target.clone(),
                        operand2: { !(0b1 as u32) }.local_into(),
                    },
                    Operation::ConditionalJump {
                        destination: target,
                        condition: Condition::None.local_into(),
                    },
                ];
                ret
            }

            Thumb::Bx(bx) => {
                // TODO! Add edge case here for panics
                let rm = bx.rm.local_into();
                // Simply implements https://developer.arm.com/documentation/ddi0419/c/Application-Level-Architecture/Application-Level-Programmers--Model/Registers-and-execution-state/ARM-core-registers
                let intermediate = Operand::Local("intermediate".to_owned());
                let mask_intermediate = Operand::Local("mask".to_owned());

                vec![
                    // BXWritePc https://developer.arm.com/documentation/ddi0419/c/Application-Level-Architecture/Application-Level-Programmers--Model/Registers-and-execution-state/ARM-core-registers

                    // 1. Set the EPSR.T reg to the LSB in the target
                    Operation::And {
                        destination: intermediate.clone(),
                        operand1: rm.clone(),
                        operand2: Operand::Immidiate(DataWord::Word32(1)),
                    },
                    Operation::Sl {
                        destination: intermediate.clone(),
                        operand: intermediate.clone(),
                        shift: 23.local_into(),
                    },
                    // TODO! Clear the bit here first
                    Operation::Sl {
                        destination: mask_intermediate.clone(),
                        operand: 0b1.local_into(),
                        shift: 23.local_into(),
                    },
                    Operation::Not {
                        destination: mask_intermediate.clone(),
                        operand: mask_intermediate.clone(),
                    },
                    Operation::And {
                        destination: SpecialRegister::EPSR.local_into(),
                        operand1: SpecialRegister::EPSR.local_into(),
                        operand2: mask_intermediate,
                    },
                    Operation::Or {
                        destination: SpecialRegister::EPSR.local_into(),
                        operand1: SpecialRegister::EPSR.local_into(),
                        // Set bit nr 24 to 1
                        operand2: intermediate,
                    },
                    // 2. Force final bit to 0
                    Operation::And {
                        destination: rm.clone(),
                        operand1: rm.clone(),
                        operand2: { !(0b1 as u32) }.local_into(),
                    },
                    Operation::ConditionalJump {
                        destination: rm,
                        condition: Condition::None.local_into(),
                    },
                ]
            }
            Thumb::Cbz(cbz) => {
                consume!((non, rn,imm) from cbz);
                let (rn, imm) = (rn.local_into(), imm.local_into());
                let flag = Operand::Flag("z".to_owned());
                let intermediate = Operand::Local("z_intermediate".to_owned());

                let condition = match non {
                    Some(true) => Condition::Ne,
                    _ => Condition::Eq,
                }
                .local_into();
                let target = Operand::Local("target".to_owned());
                vec![
                    Operation::Move {
                        destination: intermediate.clone(),
                        source: flag.clone(),
                    },
                    Operation::SetZFlag(rn.clone()),
                    Operation::And {
                        destination: target.clone(),
                        operand1: imm,
                        operand2: 0b1.local_into(),
                    },
                    Operation::ConditionalJump {
                        destination: target,
                        condition,
                    },
                    Operation::Move {
                        destination: flag,
                        source: intermediate,
                    },
                ]
            }
            Thumb::Clrex(_) => todo!("This should not be needed for now"),
            Thumb::Clz(clz) => {
                // TODO! Fix this,
                //
                // This instruction should produce the actual amount of leading zeros,
                // at the time of writing it simply produces a new symbol that is unconstrained
                // and limits it to 32
                //
                //
                //
                // TODO! Change this to use a register read hook to generate symbolic values
                let rd_old = clz.rd.clone();
                let rd = clz.rd.local_into();
                vec![
                    Operation::Symbolic {
                        destination: rd.clone(),
                        name: rd_old.to_string(),
                    },
                    // No value larger than 2^5 is valid
                    Operation::And {
                        destination: rd.clone(),
                        operand1: rd,
                        operand2: 32.local_into(),
                    },
                ]
            } //todo!("This is quite resonable but it needs to set the destination register to random symbolic"),
            Thumb::CmnImmediate(cmn) => {
                consume!((rn,imm) from cmn);
                let (rn, imm) = (rn.local_into(), imm.local_into());
                let flag = Operand::Flag("v".to_owned());
                let intermediate = Operand::Local("v_intermediate".to_owned());
                let result = Operand::Local("result".to_owned());

                vec![
                    Operation::Move {
                        destination: intermediate.clone(),
                        source: flag.clone(),
                    },
                    Operation::Move {
                        destination: flag.clone(),
                        source: 0.local_into(),
                    },
                    Operation::Adc {
                        destination: result.clone(),
                        operand1: rn.clone(),
                        operand2: imm.clone(),
                    },
                    Operation::SetNFlag(result.clone()),
                    Operation::SetZFlag(result.clone()),
                    Operation::SetCFlag {
                        operand1: rn.clone(),
                        operand2: imm.clone(),
                        sub: false,
                        carry: true,
                    },
                    Operation::SetVFlag {
                        operand1: rn,
                        operand2: imm,
                        sub: false,
                        carry: true,
                    },
                ]
            }
            Thumb::CmnRegister(cmn) => {
                consume!((rn,rm,shift) from cmn);
                let (rn, rm) = (rn.local_into(), rm.local_into());
                let shifted = Operand::Local("destination".to_owned());
                let mut ret = match shift {
                    Some(shift) => {
                        let (shift_t, shift_n) = (
                            shift.shift_t.local_into(),
                            (shift.shift_n as u32).local_into(),
                        );
                        vec![Operation::Shift {
                            destination: shifted.clone(),
                            operand: rm.clone(),
                            shift_n: shift_n,
                            shift_t: shift_t,
                        }]
                    }
                    // If no shift is applied just move the value in to the register
                    None => vec![Operation::Move {
                        destination: shifted.clone(),
                        source: rm.clone(),
                    }],
                };
                let result = Operand::Local("result".to_owned());
                ret.extend([
                    Operation::Move {
                        destination: Operand::Flag("v".to_owned()),
                        source: 0.local_into(),
                    },
                    Operation::Adc {
                        destination: result.clone(),
                        operand1: rn.clone(),
                        operand2: shifted.clone(),
                    },
                    Operation::SetNFlag(result.clone()),
                    Operation::SetZFlag(result.clone()),
                    Operation::SetCFlag {
                        operand1: rn.clone(),
                        operand2: shifted.clone(),
                        sub: false,
                        carry: true,
                    },
                    Operation::SetVFlag {
                        operand1: rn,
                        operand2: shifted,
                        sub: false,
                        carry: true,
                    },
                ]);
                ret
            }
            Thumb::CmpImmediate(cmp) => {
                consume!((rn,imm) from cmp);
                let (rn, imm) = (rn.local_into(), imm.local_into());
                let flag = Operand::Flag("v".to_owned());
                let intermediate = Operand::Local("v_intermediate".to_owned());
                let result = Operand::Local("result".to_owned());
                let imm_intermediate = Operand::Local("imm_intermediate".to_owned());
                vec![
                    Operation::Not {
                        destination: imm_intermediate.clone(),
                        operand: imm,
                    },
                    Operation::Move {
                        destination: intermediate.clone(),
                        source: flag.clone(),
                    },
                    Operation::Move {
                        destination: flag.clone(),
                        source: 1.local_into(),
                    },
                    Operation::Adc {
                        destination: result.clone(),
                        operand1: rn.clone(),
                        operand2: imm_intermediate.clone(),
                    },
                    Operation::SetNFlag(result.clone()),
                    Operation::SetZFlag(result.clone()),
                    Operation::SetCFlag {
                        operand1: rn.clone(),
                        operand2: imm_intermediate.clone(),
                        sub: false,
                        carry: true,
                    },
                    Operation::SetVFlag {
                        operand1: rn,
                        operand2: imm_intermediate,
                        sub: false,
                        carry: true,
                    },
                ]
            }
            Thumb::CmpRegister(cmp) => {
                consume!((rn,rm,shift) from cmp);
                let (rn, rm) = (rn.local_into(), rm.local_into());
                let shifted = Operand::Local("destination".to_owned());
                let mut ret = match shift {
                    Some(shift) => {
                        let (shift_t, shift_n) = (
                            shift.shift_t.local_into(),
                            (shift.shift_n as u32).local_into(),
                        );
                        vec![Operation::Shift {
                            destination: shifted.clone(),
                            operand: rm.clone(),
                            shift_n: shift_n,
                            shift_t: shift_t,
                        }]
                    }
                    // If no shift is applied just move the value in to the register
                    None => vec![Operation::Move {
                        destination: shifted.clone(),
                        source: rm.clone(),
                    }],
                };
                let result = Operand::Local("result".to_owned());
                ret.extend([
                    Operation::Not {
                        destination: shifted.clone(),
                        operand: shifted.clone(),
                    },
                    Operation::Move {
                        destination: Operand::Flag("v".to_owned()),
                        source: 1.local_into(),
                    },
                    Operation::Adc {
                        destination: result.clone(),
                        operand1: rn.clone(),
                        operand2: shifted.clone(),
                    },
                    Operation::SetNFlag(result.clone()),
                    Operation::SetZFlag(result.clone()),
                    Operation::SetCFlag {
                        operand1: rn.clone(),
                        operand2: shifted.clone(),
                        sub: false,
                        carry: true,
                    },
                    Operation::SetVFlag {
                        operand1: rn,
                        operand2: shifted,
                        sub: false,
                        carry: true,
                    },
                ]);
                ret
            }
            Thumb::Cps(cps) => {
                consume!((enable,disable,affect_pri,affect_fault) from cps);
                assert!(enable != disable);
                let mut ret = Vec::with_capacity(2);
                if enable {
                    if affect_pri {
                        // force lsb to 0
                        ret.push(Operation::And {
                            destination: SpecialRegister::PRIMASK.local_into(),
                            operand1: SpecialRegister::PRIMASK.local_into(),
                            operand2: ((!(0b1u32)).local_into()),
                        })
                    }
                    if affect_fault {
                        // force lsb to 0
                        ret.push(Operation::And {
                            destination: SpecialRegister::FAULTMASK.local_into(),
                            operand1: SpecialRegister::FAULTMASK.local_into(),
                            operand2: ((!(0b1u32)).local_into()),
                        })
                    }
                } else {
                    if affect_pri {
                        // force lsb to 1
                        ret.push(Operation::And {
                            destination: SpecialRegister::PRIMASK.local_into(),
                            operand1: SpecialRegister::PRIMASK.local_into(),
                            operand2: ((0b1u32).local_into()),
                        })
                    }
                    if affect_fault {
                        // force lsb to 1
                        ret.push(Operation::And {
                            destination: SpecialRegister::FAULTMASK.local_into(),
                            operand1: SpecialRegister::FAULTMASK.local_into(),
                            operand2: ((0b1u32).local_into()),
                        })
                    }
                }
                ret
            }
            Thumb::Dbg(_) => vec![Operation::Nop],
            Thumb::Dmb(_) => {
                todo!("This requires an exhaustive rewrite of the system to allow memory barriers")
            }
            Thumb::Dsb(_) => {
                todo!("This requires an exhaustive rewrite of the system to allow memory barriers")
            }
            Thumb::EorImmediate(eor) => {
                todo!("Depends on more complex system, we need ThumbExpandImm_C");
                consume!((s,rd,rn,imm) from eor);
                // let mut ret = vec![
                //
                // ];
                // todo!()
            }
            Thumb::EorRegister(eor) => {
                consume!((s,rd,rn,rm,shift) from eor);
                let (rd, rn, rm) = (
                    rd.unwrap_or(rn.clone()).local_into(),
                    rn.local_into(),
                    rm.local_into(),
                );
                let mut ret = Vec::with_capacity(10);
                let shifted = Operand::Local("destination".to_owned());
                ret.extend(match shift {
                    Some(shift) => {
                        let (shift_t, shift_n) = (
                            shift.shift_t.local_into(),
                            (shift.shift_n as u32).local_into(),
                        );

                        let mut flag_setter = match shift_t{
                            GAShift::Lsl => Operation::SetCFlagShiftLeft { operand: rm.clone(), shift: shift_n.clone() },
                            GAShift::Asr => Operation::SetCFlagSra { operand: rm.clone(), shift: shift_n.clone() },
                            GAShift::Lsr => Operation::SetCFlagSrl { operand: rm.clone(), shift: shift_n.clone() },
                            GAShift::Rrx => todo!("This needs some work, https://developer.arm.com/documentation/ddi0406/b/Application-Level-Architecture/Application-Level-Programmers--Model/ARM-core-data-types-and-arithmetic/Integer-arithmetic?lang=en"),
                            GAShift::Ror => todo!("This needs to be revisited, seems that the current implementation depends on this being done after the operation is performed")
                        };
                        if let Some(true) = s {
                            flag_setter = Operation::Nop;
                        }
                        vec![Operation::Shift {
                            destination: shifted.clone(),
                            operand: rm.clone(),
                            shift_n: shift_n,
                            shift_t: shift_t,
                        },
                        flag_setter
                        ]
                    }
                    // If no shift is applied just move the value in to the register
                    None =>{
                        let mut flag_setter = Operation::Move {
                            destination: Operand::Flag("c".to_owned()),
                            source: 0.local_into()
                        };
                        if let Some(true) = s {
                            flag_setter = Operation::Nop;
                        }
                        vec![Operation::Move {
                        destination: shifted.clone(),
                        source: rm.clone(),
                        },
                        flag_setter

                    ]
                    }
                });

                ret.push(Operation::Xor { destination: rd.clone(), operand1: rn.clone(), operand2: shifted.clone() });
                if let Some(true) = s {
                    ret.extend([
                        Operation::SetNFlag(rd.clone()),
                        Operation::SetZFlag(rd)
                    ]);
                }
                ret
            }
            Thumb::Isb(_) => todo!("This needs to be revisited when the executor can handle it"),
            Thumb::It(_) => todo!("Leaving this for when https://github.com/s7rul/symex/tree/conditional-execution is completed"),
            Thumb::Ldm(ldm) => {
                consume!((rn,w,registers) from ldm);
                let rn_old = rn.clone();
                let rn = rn.local_into();
                let mut ret = Vec::with_capacity(15);
                let address_setter = Operand::Local("address".to_owned());
                let address = Operand::AddressInLocal("address".to_owned(), 32);
                ret.push(Operation::Move { destination: address_setter.clone(), source: rn.clone() });
                let mut write_back = w.unwrap_or(false);
                for register in &registers.regs {
                    if *register == rn_old{
                        write_back = false;
                    }
                    ret.extend([
                        Operation::Move { destination: register.clone().local_into(), source: address.clone() },
                        Operation::Add { destination: address_setter.clone(), operand1: address.clone(), operand2: 4.local_into() }
                    ]
                    );
                }
                // TODO! Add LoadWritePC
                if write_back{
                    ret.push(
                        Operation::Add { destination: rn.clone(), operand1: rn.clone(), operand2: (4*(registers.regs.len() as u32)).local_into() }
                        );
                }
                ret

            },
            Thumb::Ldmdb(ldmdb) =>{
                consume!((rn,w,registers) from ldmdb);
                let rn_old = rn.clone();
                let rn = rn.local_into();
                let mut ret = Vec::with_capacity(15);
                let address_setter = Operand::Local("address".to_owned());
                let address = Operand::AddressInLocal("address".to_owned(), 32);
                ret.push(Operation::Sub { destination: address_setter.clone(), operand1: rn.clone(), operand2: (4*(registers.regs.len() as u32)).local_into() });
                let mut write_back = w.unwrap_or(false);
                for register in &registers.regs {
                    if *register == rn_old{
                        write_back = false;
                    }
                    ret.extend([
                        Operation::Move { destination: register.clone().local_into(), source: address.clone() },
                        Operation::Add { destination: address_setter.clone(), operand1: address_setter.clone(), operand2: 4.local_into() }
                    ]
                    );
                }
                // TODO! Add LoadWritePC
                if write_back{
                    ret.push(
                        Operation::Sub { destination: rn.clone(), operand1: rn.clone(), operand2: (4*(registers.regs.len() as u32)).local_into() }
                        );
                }
                ret
            },
            Thumb::LdrImmediate(ldr) => {
                consume!((index,add,w,rt,rn,imm) from ldr);
                let old_rt = rt.clone();
                let (rt,rn,imm) = (rt.local_into(),rn.local_into(),imm.local_into());
                let mut ret = Vec::with_capacity(5);
                local!(offset_addr,data);
                let address_setter = Operand::Local("address".to_owned());
                let address = Operand::AddressInLocal("address".to_owned(), 32);
                match add {
                    true => ret.push(Operation::Add { destination: offset_addr.clone(), operand1: rn.clone(), operand2: imm.clone() }),
                    _ => ret.push(Operation::Sub { destination: offset_addr.clone(), operand1: rn.clone(), operand2: imm.clone() }),
                }
                match index {
                    true => ret.push(Operation::Move { destination: address.clone(), source: offset_addr.clone()}),
                    _ => ret.push(Operation::Move { destination: address_setter.clone(), source: rn.clone()}),
                }
                ret.push(Operation::Move { destination: data.clone(), source: address });
                match old_rt {
                    Register::PC => todo!("We need to check the 2 lsbs of address here which is un achievable at this time"),
                    _ => ret.push(Operation::Move { destination: rt, source: data })
                }
                ret
            },
            Thumb::LdrLiteral(ldr) => {
                todo!("Align PC 4")
            },
            Thumb::LdrRegister(ldr) => {
                // TODO! Validate that the documentation is correct
                // It seems that index is allways true
                // add is allways true and w is allways false.
                consume!((w,rt,rn,rm,shift) from ldr);
                let rt_old = rt.clone();
                let (rt,rn,rm) = (rt.local_into(), rn.local_into(),rm.local_into());
                let mut ret = Vec::with_capacity(10);
                local!(offset,offset_addr,data);
                let address_setter = Operand::Local("address".to_owned());
                let address = Operand::AddressInLocal("address".to_owned(),32);
                ret.push(Operation::Move { destination: offset.clone(), source: rm.clone() });
                if let Some(shift) = shift{
                    ret.push(Operation::Sl { destination: offset.clone(), operand: offset.clone(), shift: (shift.shift_n as u32).local_into() });
                }
                // Continuing with the assumptions mentioned above
                ret.extend([
                    Operation::Add { destination: offset_addr.clone(), operand1: rn.clone(), operand2: offset.clone() },
                    Operation::Move { destination: address_setter.clone(), source: offset_addr },
                    Operation::Move { destination: data.clone(), source: address }
                ]);
                match rt_old {
                        Register::PC => todo!("LoadWritePc"),
                        _ => {ret.push(Operation::Move { destination: rt, source: data });}
                }
                ret
            },
            Thumb::LdrbImmediate(ldrb) => {
                consume!((index,add,w,rt,rn,imm) from ldrb);
                let imm = imm.unwrap_or(0);
                let (rt,rn,imm) = (rt.local_into(),rn.local_into(),imm.local_into());
                let mut ret = Vec::with_capacity(5);
                local!(offset_addr,data);
                let address_setter = Operand::Local("address".to_owned());
                let address = Operand::AddressInLocal("address".to_owned(), 8);
                match add {
                    Some(true) => ret.push(Operation::Add { destination: offset_addr.clone(), operand1: rn.clone(), operand2: imm.clone() }),
                    _ => ret.push(Operation::Sub { destination: offset_addr.clone(), operand1: rn.clone(), operand2: imm.clone() }),
                }
                match index {
                    true => ret.push(Operation::Move { destination: address.clone(), source: offset_addr.clone()}),
                    _ => ret.push(Operation::Move { destination: address_setter.clone(), source: rn.clone()}),
                }
                ret.push(Operation::Move { destination: data.clone(), source: address });
                ret.push(Operation::Move { destination: rt.clone(), source: data });
                ret.push(Operation::ZeroExtend { destination: rt.clone(), operand: rt, bits: 32 });
                if let Some(true) = w {
                    ret.push(Operation::Move { destination: rn, source: offset_addr })
                }
                ret
            },
            Thumb::LdrbLiteral(_) => todo!("Align PC 4"),
            Thumb::LdrbRegister(ldrb) =>{
                // TODO! Validate that the documentation is correct
                // It seems that index is allways true
                // add is allways true and w is allways false.
                consume!((rt,rn,rm,shift,add) from ldrb);
                let (rt,rn,rm) = (rt.local_into(), rn.local_into(),rm.local_into());
                let mut ret = Vec::with_capacity(10);
                local!(offset,offset_addr,data);
                let address_setter = Operand::Local("address".to_owned());
                let address = Operand::AddressInLocal("address".to_owned(),8);
                ret.push(Operation::Move { destination: offset.clone(), source: rm.clone() });
                if let Some(shift) = shift{
                    ret.push(Operation::Sl { destination: offset.clone(), operand: offset.clone(), shift: (shift.shift_n as u32).local_into() });
                }
                // Continuing with the assumptions mentioned above
                ret.extend([
                    Operation::Add { destination: offset_addr.clone(), operand1: rn.clone(), operand2: offset.clone() },
                    Operation::Move { destination: address_setter.clone(), source: offset_addr },
                    Operation::Move { destination: data.clone(), source: address }
                ]);
                ret.extend([Operation::Move { destination: rt.clone(), source: data },
                    Operation::ZeroExtend { destination: rt.clone(), operand: rt, bits: 32 },]);
                ret
            },
            Thumb::Ldrbt(ldrbt) => {
                consume!((rt,rn,imm) from ldrbt);
                let (rt,rn,imm) = (rt.local_into(),rn.local_into(),imm.unwrap_or(0).local_into());
                let address_setter = Operand::Local("address".to_owned());
                let address = Operand::AddressInLocal("address".to_owned(),8);
                vec![
                    Operation::Add { destination: address_setter.clone(), operand1: rn, operand2: imm },
                    Operation::Move { destination: rt.clone(), source: address },
                    Operation::ZeroExtend { destination: rt.clone(), operand: rt, bits: 32 },
                ]
            },
            Thumb::LdrdImmediate(ldrd) => {
                consume!((rt.local_into(),rt2.local_into(),rn.local_into(),imm.local_into(),add,index,w) from ldrd);
                let address_setter = Operand::Local("address".to_owned());
                let offset_address = Operand::Local("offset_address".to_owned());
                let address = Operand::AddressInLocal("address".to_owned(), 32);
                let mut ret = Vec::with_capacity(23);
                
                match add{
                    Some(true) => ret.push(Operation::Add { destination: offset_address.clone(), operand1: rn.clone(), operand2: imm }),
                    _ => ret.push(Operation::Sub { destination: offset_address.clone(), operand1: rn.clone(), operand2: imm }),
                }
                match index{
                    Some(true) => ret.push(Operation::Move { destination: address_setter.clone(), source: offset_address.clone() }),
                    _ => ret.push(Operation::Move { destination: address_setter.clone(), source: rn.clone() })
                }

                ret.extend([
                           Operation::Move { destination: rt.clone(), source: address.clone()},
                           Operation::Add { destination: address_setter.clone(), operand1: address_setter.clone(), operand2: 4.local_into() },
                           Operation::Move { destination: rt2.clone(), source: address.clone()},
                           Operation::ZeroExtend { destination: rt.clone(), operand: rt, bits: 32 },
                           Operation::ZeroExtend { destination: rt2.clone(), operand: rt2, bits: 32 }
                ]);
                if let Some(true) = w {
                    ret.push(Operation::Move { destination: rn, source: offset_address })
                }
                ret
            },
            Thumb::LdrdLiteral(ldrd) => {
                consume!((rt.local_into(),rt2.local_into(),imm.local_into(),add,w,index) from ldrd);
                let address_setter = Operand::Local("address".to_owned());
                let address = Operand::AddressInLocal("address".to_owned(), 32);
                let mut ret = Vec::with_capacity(10);

                match add {
                    Some(true) => ret.push(Operation::Add { destination: address_setter.clone(), operand1: Register::PC.local_into(), operand2: imm }),
                    _ => ret.push(Operation::Sub { destination: address_setter.clone(), operand1: Register::PC.local_into(), operand2: imm }),
                }

                ret.extend([
                    Operation::Move { destination: rt.clone(), source: address.clone() },
                    Operation::Add { destination: address_setter.clone(), operand1: address_setter, operand2: 4.local_into() },
                    Operation::Move { destination: rt2.clone(), source: address.clone() },
                    Operation::ZeroExtend { destination: rt.clone(), operand: rt, bits: 32 },
                    Operation::ZeroExtend { destination: rt2.clone(), operand: rt2, bits: 32 }
                ]);
                
                ret
            },
            Thumb::Ldrex(_) => todo!("This is probably not needed"),
            Thumb::Ldrexb(_) => todo!("This is probably not needed"),
            Thumb::Ldrexh(_) => todo!("This is probably not needed"),
            Thumb::LdrhImmediate(ldrh) => {
                consume!((rt.local_into(),rn.local_into(),imm.local_into(),add,w,index) from ldrh);
                let address_setter = Operand::Local("address".to_owned());
                let offset_address = Operand::Local("offset_address".to_owned());
                let address = Operand::AddressInLocal("address".to_owned(), 16);
                let mut ret = Vec::with_capacity(4);
                
                match add{
                    Some(true) => ret.push(Operation::Add { destination: offset_address.clone(), operand1: rn.clone(), operand2: imm }),
                    _ => ret.push(Operation::Sub { destination: offset_address.clone(), operand1: rn.clone(), operand2: imm }),
                }
                match index{
                    Some(true) => ret.push(Operation::Move { destination: address_setter.clone(), source: offset_address.clone() }),
                    _ => ret.push(Operation::Move { destination: address_setter.clone(), source: rn.clone() })
                }

                ret.extend([
                           Operation::Move { destination: rt.clone(), source: address.clone()},
                        Operation::ZeroExtend { destination: rt.clone(), operand: rt, bits: 32 }
                ]);
                if let Some(true) = w {
                    ret.push(Operation::Move { destination: rn, source: offset_address })
                }
                ret
            }
            Thumb::LdrhLiteral(ldrh) => {
                consume!((rt.local_into(),imm.local_into(),add) from ldrh);
                let address_setter = Operand::Local("address".to_owned());
                let address = Operand::AddressInLocal("address".to_owned(), 16);
                let mut ret = Vec::with_capacity(23);
                // TODO! Add register hooks for PCA which simply aligns the value
                ret.push(Operation::Move { destination: address_setter.clone(), source: Operand::Register("PCA".to_owned()) });
                
                ret.push(match add {
                    Some(true) => Operation::Add { destination: address_setter.clone(), operand1: address_setter, operand2: imm },
                    _ => Operation::Sub { destination: address_setter.clone(), operand1: address_setter, operand2: imm },
                });

                ret.extend([
                    Operation::Move { destination: rt.clone(), source: address },

                    Operation::ZeroExtend { destination: rt.clone(), operand: rt, bits: 32 }
                ]);
                ret
            },
            Thumb::LdrhRegister(ldrh) => {
                consume!((rt.local_into(),rn.local_into(),rm.local_into(),shift) from ldrh);
                let mut ret = Vec::with_capacity(10);
                let offset = Operand::Local("offset".to_owned());
                let address_setter = Operand::Local("address".to_owned());
                let offset_address = Operand::Local("offset_address".to_owned());
                let address = Operand::AddressInLocal("address".to_owned(), 16);
                
                shift!(ret.shift rm -> offset);
                
                // This is correct for the ARMV7 probably not for future extensions
                ret.extend([
                    Operation::Add { destination: offset_address.clone(), operand1: rn, operand2:  offset},
                    Operation::Move { destination: address_setter.clone(), source: offset_address },
                    Operation::Move { destination: rt.clone(), source: address },
                    Operation::ZeroExtend { destination: rt.clone(), operand: rt, bits: 32 }
                ]);
                
                ret
            },
            Thumb::Ldrht(ldrht) => {
                consume!((rt.local_into(),rn.local_into(),imm.unwrap_or(0).local_into()) from ldrht);
                let address_setter = Operand::Local("address".to_owned());
                let address = Operand::AddressInLocal("address".to_owned(), 16);
                vec![
                    Operation::Add { destination: address_setter.clone(), operand1: rn, operand2: imm },
                    Operation::Move { destination: rt, source: address }
                ]
            },
            Thumb::LdrsbImmediate(ldrsb) => {
                consume!((rt.local_into(), rn.local_into(), imm.unwrap_or(0).local_into(), add, index, wback ) from ldrsb);
                let mut ret = Vec::with_capacity(10);
                let address_setter = Operand::Local("address".to_owned());
                let offset_address = Operand::Local("offset_address".to_owned());
                let address = Operand::AddressInLocal("address".to_owned(), 8);
                
                ret.push(match add {
                    true => Operation::Add { destination: offset_address.clone(), operand1: rn.clone(), operand2: imm },
                    _ => Operation::Sub { destination: offset_address.clone(), operand1: rn.clone(), operand2: imm },
                });

                ret.push(match index{
                    true => Operation::Move { destination: address_setter.clone(), source: offset_address.clone() },
                    _ => Operation::Move { destination: address_setter.clone(), source: rn.clone() },
                });

                ret.extend(
                    [
                    Operation::SignExtend { destination: rt, operand: address, bits: 32 }
                    ]
                    );

                if wback{
                    ret.push(Operation::Move { destination: rn, source: offset_address })
                }

                ret
            },
            Thumb::LdrsbLiteral(ldrsb) => {
                consume!((rt.local_into(),imm.local_into(),add) from ldrsb);
                let address_setter = Operand::Local("address".to_owned());
                let offset_address = Operand::Local("offset_address".to_owned());
                let address = Operand::AddressInLocal("address".to_owned(), 8);
                let mut ret = Vec::with_capacity(23);
                // TODO! Add register hooks for PCA which simply aligns the value
                ret.push(Operation::Move { destination: offset_address.clone(), source: Operand::Register("PCA".to_owned()) });

                           
                ret.push(match add {
                    true => Operation::Add { destination: address_setter.clone(), operand1: offset_address, operand2: imm }, 
                    false => Operation::Sub { destination: address_setter.clone(), operand1: offset_address, operand2: imm }, 
                });

                ret.push(Operation::SignExtend { destination: rt, operand: address, bits: 32 });
                ret
            },
            Thumb::LdrsbRegister(ldrsb) => {
                consume!((rt.local_into(),rn.local_into(),rm.local_into(),shift) from ldrsb);
                let mut ret = Vec::with_capacity(10);
                let offset = Operand::Local("offset".to_owned());
                let address_setter = Operand::Local("address".to_owned());
                let offset_address = Operand::Local("offset_address".to_owned());
                let address = Operand::AddressInLocal("address".to_owned(), 8);
                
                shift!(ret.shift rm -> offset);
                
                // This is correct for the ARMV7 probably not for future extensions
                ret.extend([
                    Operation::Add { destination: offset_address.clone(), operand1: rn, operand2:  offset},
                    Operation::Move { destination: address_setter.clone(), source: offset_address },
                    Operation::Move { destination: rt.clone(), source: address },
                    Operation::SignExtend { destination: rt.clone(), operand: rt, bits: 32 }
                ]);
                
                ret
            },
            Thumb::Ldrsbt(ldrsbt) => {
                consume!((rt.local_into(), rn.local_into(), imm.local_into()) from ldrsbt);
                let address_setter = Operand::Local("address".to_owned());
                let address = Operand::AddressInLocal("address".to_owned(), 8);
                vec![
                    Operation::Add { destination: address_setter, operand1: rn, operand2: imm },
                    Operation::SignExtend { destination: rt, operand: address, bits: 32 }
                ]
            },
            Thumb::LdrshImmediate(ldrsh) => {
                consume!((rt.local_into(), rn.local_into(), imm.unwrap_or(0).local_into(), add, index, wback ) from ldrsh);
                let mut ret = Vec::with_capacity(10);
                let address_setter = Operand::Local("address".to_owned());
                let offset_address = Operand::Local("offset_address".to_owned());
                let address = Operand::AddressInLocal("address".to_owned(), 16);
                
                ret.push(match add {
                    true => Operation::Add { destination: offset_address.clone(), operand1: rn.clone(), operand2: imm },
                    _ => Operation::Sub { destination: offset_address.clone(), operand1: rn.clone(), operand2: imm },
                });

                ret.push(match index{
                    true => Operation::Move { destination: address_setter.clone(), source: offset_address.clone() },
                    _ => Operation::Move { destination: address_setter.clone(), source: rn.clone() },
                });

                ret.extend(
                    [
                    Operation::SignExtend { destination: rt, operand: address, bits: 32 }
                    ]
                    );

                if wback{
                    ret.push(Operation::Move { destination: rn, source: offset_address })
                }

                ret
            },
            Thumb::LdrshLiteral(ldrsh) => {
                consume!((rt.local_into(),imm.local_into(),add) from ldrsh);
                let address_setter = Operand::Local("address".to_owned());
                let offset_address = Operand::Local("offset_address".to_owned());
                let address = Operand::AddressInLocal("address".to_owned(), 16);
                let mut ret = Vec::with_capacity(23);
                // TODO! Add register hooks for PCA which simply aligns the value
                ret.push(Operation::Move { destination: offset_address.clone(), source: Operand::Register("PCA".to_owned()) });

                           
                ret.push(match add {
                    true => Operation::Add { destination: address_setter.clone(), operand1: offset_address, operand2: imm }, 
                    false => Operation::Sub { destination: address_setter.clone(), operand1: offset_address, operand2: imm }, 
                });

                ret.push(Operation::SignExtend { destination: rt, operand: address, bits: 32 });
                ret
            },
            Thumb::LdrshRegister(ldrsh) => {
                consume!((rt.local_into(),rn.local_into(),rm.local_into(),shift) from ldrsh);
                let mut ret = Vec::with_capacity(10);
                let offset = Operand::Local("offset".to_owned());
                let address_setter = Operand::Local("address".to_owned());
                let offset_address = Operand::Local("offset_address".to_owned());
                let address = Operand::AddressInLocal("address".to_owned(), 16);
                
                shift!(ret.shift rm -> offset);
                
                // This is correct for the ARMV7 probably not for future extensions
                ret.extend([
                    Operation::Add { destination: offset_address.clone(), operand1: rn, operand2:  offset},
                    Operation::Move { destination: address_setter.clone(), source: offset_address },
                    Operation::Move { destination: rt.clone(), source: address },
                    Operation::SignExtend { destination: rt.clone(), operand: rt, bits: 32 }
                ]);
                
                ret
            },
            Thumb::Ldrsht(ldrsht) => {
                consume!((rt.local_into(), rn.local_into(), imm.unwrap_or(0).local_into()) from ldrsht);
                let address_setter = Operand::Local("address".to_owned());
                let address = Operand::AddressInLocal("address".to_owned(), 16);
                vec![
                    Operation::Add { destination: address_setter, operand1: rn, operand2: imm },
                    Operation::SignExtend { destination: rt, operand: address, bits: 32 }
                ]
            },
            Thumb::Ldrt(ldrt) => {
                consume!((rt.local_into(), rn.local_into(), imm.unwrap_or(0).local_into()) from ldrt);
                let address_setter = Operand::Local("address".to_owned());
                let address = Operand::AddressInLocal("address".to_owned(), 32);
                vec![
                    Operation::Add { destination: address_setter, operand1: rn, operand2: imm },
                    Operation::Move { destination: rt, source: address }
                ]
            },
            Thumb::LslImmediate(lsl) => {
                consume!((s.unwrap_or(false),rd.local_into(),rm.local_into(), imm) from lsl);
                let shift:Option<dissarmv7::prelude::ImmShift> = Some((Shift::Lsl,imm).try_into().unwrap());
                let mut ret = vec![];
                match s {
                    true => shift!(ret.shift rm -> rd set c for rm),
                    false => shift!(ret.shift rm -> rd),
                };
                ret
            },
            Thumb::LslRegister(lsl) => {
                consume!((s.unwrap_or(false),rd.local_into(),rn.local_into(),rm.local_into()) from lsl);
                local!(shift_n,result);
                let mut ret = vec![
                    Operation::And { destination: shift_n.clone(), operand1: rm, operand2: 0xff.local_into() }
                ];
                let shift_t = Shift::Lsl.local_into();
                match s {
                    true => shift_imm!(ret.(shift_t,shift_n) rn -> rd set c for rn),
                    false => shift_imm!(ret.(shift_t,shift_n) rn -> rd)
                };
                ret

            },
            Thumb::LsrImmediate(lsr) => {
                consume!((s.unwrap_or(false),rd.local_into(),rm.local_into(), imm) from lsr);
                let shift:Option<dissarmv7::prelude::ImmShift> = Some((Shift::Lsr,imm).try_into().unwrap());
                let mut ret = vec![];
                match s {
                    true => shift!(ret.shift rm -> rd set c for rm),
                    false => shift!(ret.shift rm -> rd),
                };
                ret
            },
            Thumb::LsrRegister(lsr) => {
                consume!((s.unwrap_or(false),rd.local_into(),rn.local_into(),rm.local_into()) from lsr);
                local!(shift_n,result);
                let mut ret = vec![
                    Operation::And { destination: shift_n.clone(), operand1: rm, operand2: 0xff.local_into() }
                ];
                let shift_t = Shift::Lsr.local_into();
                match s {
                    true => shift_imm!(ret.(shift_t,shift_n) rn -> rd set c for rn),
                false => shift_imm!(ret.(shift_t,shift_n) rn -> rd)
                };
                ret
            },
            Thumb::Mla(mla) => {
                // S is allway false for >= V7
                //
                // TODO! s in to the thumb instruction definition and make dissarmv7 set it to
                // false
                consume!((rn.local_into(),ra.local_into(), rd.local_into(), rm.local_into()) from mla);
                // TODO! Validate that the usage of SInt or UInt is irrelevant
                let mut ret = Vec::with_capacity(3);
                pseudo!(
                    ret.extend[
                       rd = rn*rm;
                       rd = rd+ra;
                    ]
                );
                ret
            },
            Thumb::Mls(mls) => {
                consume!((rn.local_into(),ra.local_into(), rd.local_into(), rm.local_into()) from mls);
                let mut ret = Vec::with_capacity(3);
                pseudo!(
                    ret.extend[
                        rd = rn*rm;
                        rd = ra-rd;
                    ]
                );
                ret
            },
            // One single encoding, this needs to be revisited once it is needed
            Thumb::MovImmediate(mov) => {todo!("This requires ThumbExpandImm_C")},
            Thumb::MovImmediatePlain(mov) =>{
                consume!((s,rd.local_into(),imm.local_into()) from mov);
                let mut ret = Vec::with_capacity(4);
                // Carry is unchanged here, the only time that carry changes is in
                // [`Thumb::MovImmediatePlain`]
                ret.push(bin_op!(rd = imm));
                if let Some(true) = s{
                    ret.extend(
                        [
                            Operation::SetNFlag(rd.clone()),
                            Operation::SetZFlag(rd)
                        ]
                    );
                }
                ret
            }
            Thumb::MovReg(mov) => {
                // This might cause some issues, we will disregard BX cases here as we have no way
                // of changing the instruciton set
                consume!((s,rd, rm.local_into()) from mov);
                if rd == Register::PC {
                    break 'outer_block vec![
                        Operation::ConditionalJump { destination: rm, condition: Condition::None.local_into() }
                    ];
                }
                let rd = rd.local_into();
                let mut ret = vec![Operation::Move { destination: rd.clone(), source: rm }];
                if let Some(true) = s{
                    ret.extend([
                        Operation::SetNFlag(rd.clone()),
                        Operation::SetZFlag(rd)
                    ]);
                }
                ret
            },
            Thumb::Movt(movt) => {
                
                    consume!((rd.local_into(),imm) from movt);
                    let imm = (imm as u32).local_into();
                    let mut ret = Vec::with_capacity(4);
                    let mask = (u16::MAX as u32).local_into();
                    let shift = 16.local_into();
                    local!(intermediate);
                    pseudo!(
                        ret.extend[
                            intermediate = imm << shift;
                            // Perserve the lower half word
                            rd = mask & rd;
                            rd = intermediate | rd;
                        ]
                    );
                    ret
            },
            Thumb::Mrs(_) => todo!("TODO! need to revisit and check if there is any strange behaviour"),
            Thumb::Msr(_) => todo!("TODO! need to revisit and check if there is any strange behaviour"),
            Thumb::Mul(mul) => {
                consume!((s,rn, rd.unwrap_or(rn.clone()).local_into(),rm.local_into()) from mul);
                let rn = rn.local_into();
                let mut ret = vec![bin_op!(rd = rn*rm)];
                if let Some(true) = s { 
                    ret.extend([
                        Operation::SetZFlag(rd.clone()),
                        Operation::SetNFlag(rd)
                    ]);
                }
                ret
            },
            Thumb::MvnImmediate(mvn) => {
                todo!("Needs ThumbExpandImm_C also needs to fix the conversion in dissarmv7");
            },
            Thumb::MvnRegister(mvn) => {
                consume!((s,rd.local_into(), rm.local_into(),shift) from mvn);
                let mut ret =  Vec::with_capacity(5);
                local!(shifted);
                shift!(ret.shift rm -> shifted set c for rm);
                ret.push(bin_op!(rd = ! shifted));
                if let Some(true) = s {
                    ret.extend([
                        Operation::SetNFlag(rd.clone()),
                        Operation::SetZFlag(rd)
                    ]);
                }
                ret
            },
            Thumb::Nop(_) => vec![Operation::Nop],
            Thumb::OrnImmediate(_) => todo!("Needs ThumbExpandImm_C"),
            Thumb::OrnRegister(orn) => {
                consume!((s,rd, rm.local_into(),rn,shift) from orn);
                let (rd,rn) = (rd.unwrap_or(rn.clone()).local_into(),rn.local_into());
                let mut ret =  Vec::with_capacity(5);
                local!(shifted);
                shift!(ret.shift rm -> shifted set c for rm);
                ret.extend([
                           bin_op!(shifted = !shifted),
                           bin_op!(rd = rn | shifted)
                ]);
                if let Some(true) = s {
                    ret.extend([
                        Operation::SetNFlag(rd.clone()),
                        Operation::SetZFlag(rd)
                    ]);
                }
                ret
            },
            Thumb::OrrImmediate(orr) => todo!("Needs ThumbExpandImm_c"),
            Thumb::OrrRegister(orr) => {
                consume!((s,rd, rm.local_into(),rn,shift) from orr);
                let (rd,rn) = (rd.unwrap_or(rn.clone()).local_into(),rn.local_into());
                let mut ret =  Vec::with_capacity(5);
                local!(shifted);
                shift!(ret.shift rm -> shifted set c for rm);
                ret.extend([
                           bin_op!(rd = rn | shifted)
                ]);
                if let Some(true) = s {
                    ret.extend([
                        Operation::SetNFlag(rd.clone()),
                        Operation::SetZFlag(rd)
                    ]);
                }
                ret
            },
            Thumb::Pkh(pkh) => {
                consume!((rd,shift,rn,rm.local_into(),tb) from pkh);
                let mut ret = Vec::with_capacity(5);
                let (rd,rn) = (rd.unwrap_or(rn.clone()).local_into(),rn.local_into());
                local!(shifted);
                shift!(ret.shift rm -> shifted);
                let (msh,lsh) = match tb {
                    true => (rn,shifted),
                    _ => (shifted,rn)
                };
                pseudo!(
                    ret.extend[
                        lsh = lsh & (u16::MAX as u32).local_into();
                        msh = msh & (!(u16::MAX as u32)).local_into();
                        rd = msh | lsh;
                    ]
                );
                ret
            },
            Thumb::PldImmediate(pld) => {
                todo!(" We need some speciality pre load instruction here")
            },
            Thumb::PldLiteral(_) => todo!(" We need some speciality pre load instruction here"),
            Thumb::PldRegister(_) => todo!(" We need some speciality pre load instruction here"),
            Thumb::PliImmediate(_) => todo!(" We need some speciality pre load instruction here"),
            Thumb::PliRegister(_) => todo!(" We need some speciality pre load instruction here"),
            Thumb::Pop(pop) => {
                consume!((registers) from pop);

                let address_setter = Operand::Local("address".to_owned());
                let address = Operand::AddressInLocal("address".to_owned(), 32);
                let sp = Register::SP.local_into();
                
                let mut ret = vec![];
                pseudo!(

                    ret.extend[
                        address_setter = sp;
                        sp = sp + (4*registers.regs.len() as u32).local_into();
                    ]
                );
                let mut pc = false;
                let regs = registers.regs.iter().map(|reg| {if *reg == Register::PC{pc =true};reg.local_into()}).collect::<Vec<Operand>>();
                for reg in regs {
                    pseudo!(
                        ret.extend[
                            // Read the current address in to the specified reg
                            reg = address;
                            address_setter = address_setter + 4.local_into();
                        ]
                    );
                }
                if pc {
                    todo!("LoadWritePc")
                }
                ret
            },
            Thumb::Push(push) => {
                consume!((registers) from push);
                let address_setter = Operand::Local("address".to_owned());
                let address = Operand::AddressInLocal("address".to_owned(), 32);
                let sp = Register::SP.local_into();
                
                let mut ret = vec![];
                pseudo!(
                    ret.extend[
                        address_setter = sp - (4*registers.regs.len() as u32).local_into();
                        sp = sp - (4*registers.regs.len() as u32).local_into();
                    ]
                );
                let regs = registers.regs.iter().map(|reg| reg.local_into()).collect::<Vec<Operand>>();
                for reg in regs {
                    pseudo!(
                        ret.extend[
                            // Read the current address in to the specified reg
                            reg = address;
                            address_setter = address_setter + 4.local_into();

                        ]
                    );
                }
                ret
            },
            Thumb::Qadd(_) => todo!("Need to figure out how to do saturating operations"),
            Thumb::Qadd16(_) => todo!("Need to figure out how to do saturating operations"),
            Thumb::Qadd8(_) => todo!("Need to figure out how to do saturating operations"),
            Thumb::Qasx(_) => todo!("Need to figure out how to do saturating operations"),
            Thumb::Qdadd(_) => todo!("Need to figure out how to do saturating operations"),
            Thumb::Qdsub(_) => todo!("Need to figure out how to do saturating operations"),
            Thumb::Qsax(_) => todo!("Need to figure out how to do saturating operations"),
            Thumb::Qsub(_) => todo!("Need to figure out how to do saturating operations"),
            Thumb::Qsub16(_) => todo!("Need to figure out how to do saturating operations"),
            Thumb::Qsub8(_) => todo!("Need to figure out how to do saturating operations"),
            Thumb::Rbit(rbit) => {
                consume!((rd.local_into(),rm.local_into()) from rbit);
                let mut ret =  vec![];
                local!(intermediate);
                let zero = 0.local_into();
                for i in 0..31u32 {
                    let mask = ( 1 << i ).local_into();
                    let shift = 31 - (i as i32)*2i32;
                    match shift >  0{
                        true => {
                            let shift = (shift as u32).local_into();
                            pseudo!(
                                ret.extend[
                                    intermediate = zero;
                                    intermediate = rm & mask;
                                    intermediate =  intermediate << shift;
                                    rd = rd|intermediate;
                                ]
                            );
                        }
                        false => {
                            let shift = (-shift as u32).local_into();
                            pseudo!(
                                ret.extend[
                                    intermediate = zero;
                                    intermediate = rm & mask;
                                    intermediate =  intermediate >> shift;
                                    rd = rd|intermediate;
                                ]
                            );
                        }
                    }
                }
                ret
            },
            Thumb::Rev(rev) => {
                consume!((rd.local_into(),rm.local_into()) from rev);
                local!(int1,int2,int3,int4);
                let mut ret = vec![];
                let m = u8::MAX as u32;
                let mask = |m:u32,n:u32| {(m << n*8).local_into()};
                let zero = 0.local_into();
                pseudo!(
                    ret.extend[
                        int1 = rm & mask(m,0);
                        int2 = rm & mask(m,1);
                        int3 = rm & mask(m,2);
                        int4 = rm & mask(m,3);
                        int1 = int1 << (24).local_into();
                        int2 = int2 << (8).local_into();
                        int3 = int3 >> (8).local_into();
                        int4 = int4 >> (24).local_into();
                        rd = zero;
                        rd = rd | int1;
                        rd = rd | int2;
                        rd = rd | int3;
                        rd = rd | int4;
                    ]
                );

                
                ret
            },
            Thumb::Rev16(rev) => {
                consume!((rd.local_into(),rm.local_into()) from rev);
                local!(int1,int2,int3,int4);
                let mut ret = vec![];
                let m = u8::MAX as u32;
                let mask = |m:u32,n:u32| {(m << n*8).local_into()};
                let zero = 0.local_into();
                pseudo!(
                    ret.extend[
                        int1 = rm & mask(m,0);
                        int2 = rm & mask(m,1);
                        int3 = rm & mask(m,2);
                        int4 = rm & mask(m,3);
                        int1 = int1 << (8).local_into();
                        int2 = int2 >> (8).local_into();
                        int3 = int3 << (8).local_into();
                        int4 = int4 >> (8).local_into();
                        rd = zero;
                        rd = rd | int1;
                        rd = rd | int2;
                        rd = rd | int3;
                        rd = rd | int4;
                    ]
                );
                ret
            },
            Thumb::Revsh(revsh) => {
                consume!((rd.local_into(),rm.local_into()) from revsh);
                local!(int1,int2);
                let mut ret = vec![];
                let m = u8::MAX as u32;
                let mask = |m:u32,n:u32| {(m << n*8).local_into()};
                let zero = 0.local_into();
                pseudo!(
                    ret.extend[
                        int1 = rm & mask(m,0);
                        int2 = rm & mask(m,1);
                        int1 = int1 << (8).local_into();
                        int2 = int2 >> (8).local_into();
                        rd = zero;
                    ]
                );
                ret.push(
                    // This should be correct as the value has already been shifted over by
                    // 9
                    Operation::SignExtend { destination: rd.clone(), operand: int1, bits: 16 }
                );
                pseudo!(
                    ret.extend[
                        rd = rd | int2;
                    ]
                );
                ret
            },
            Thumb::RorImmediate(ror) => {
                consume!((s,rd.local_into(), rm.local_into(),imm) from ror);
                let imm: u32 = imm.into();
                let shift_n = imm.local_into();
                let mut ret = vec![
                    Operation::Sror { destination: rd.clone(), operand: rm.clone(), shift: shift_n.clone() },
                ];
                if let Some(true) = s{
                    ret.extend([
                        Operation::SetZFlag(rd.clone()),
                        Operation::SetNFlag(rd.clone()),
                        Operation::SetCFlagRor(rd.clone())
                    ]);
                }
                ret
            },
            Thumb::RorRegister(ror) => {
                consume!((s,rd.local_into(), rm.local_into(),rn.local_into()) from ror);
                local!(shift_n);
                let mask = (u8::MAX as u32).local_into();

                let mut ret = vec![
                    Operation::And { destination: shift_n.clone(), operand1: rm.clone(), operand2: mask },
                    Operation::Sror { destination: rd.clone(), operand: rn.clone(), shift: shift_n.clone() },
                ];
                if let Some(true) = s{
                    ret.extend([
                        Operation::SetZFlag(rd.clone()),
                        Operation::SetNFlag(rd.clone()),
                        Operation::SetCFlagRor(rd.clone())
                    ]);
                }
                ret
            },
            Thumb::Rrx(rrx) => {
                consume!((s,rd.local_into(), rm.local_into()) from rrx);
                // Let's fulhacka
                let mask = (u32::MAX >> 1).local_into();
                let lsb_mask = (1).local_into();
                local!(lsb,result,msb);
                let carry = Operand::Flag("c".to_owned());
                let mut ret = Vec::with_capacity(10);
                pseudo!(
                    ret.extend[
                        lsb = rm & lsb_mask;
                        result = rm >> 1.local_into();
                        msb = carry << 31.local_into();
                        // Clear the bit first
                        result = result & mask;
                        result = result | msb;
                        rd = result;
                    ]
                );

                if let Some(true) = s {
                    ret.extend([
                        Operation::SetNFlag(result.clone()),
                        Operation::SetZFlag(result.clone()),
                        Operation::Move { destination: carry, source: lsb }
                    ]);
                }
                ret
            },
            Thumb::RsbImmediate(rsb) => {
                consume!((s,rd,rn,imm.local_into()) from rsb);
                let (rd,rn) = (rd.unwrap_or(rn.clone()).local_into(),rn.local_into());
                let carry = Operand::Flag("c".to_owned());
                local!(intermediate,old_carry);
                let one = 1.local_into();

                let mut ret = Vec::with_capacity(10);

                pseudo!(
                    ret.extend[
                        // Backup carry bit
                        old_carry = carry;
                        // Set carry  bit to 1 
                        carry = one;

                        intermediate = !rn;
                        // add with carry
                        rd = intermediate adc imm;
                    ]
                );
                ret.extend(
                    match s {
                        Some(true)  => {
                            vec![
                                Operation::SetZFlag(rd.clone()),
                                Operation::SetNFlag(rd.clone()),
                                Operation::SetCFlag { operand1: intermediate, operand2: imm, sub: false, carry: true }
                            ]
                        }
                        _ => vec![bin_op!(carry = old_carry)]
                    }
                );
                ret
            },
            Thumb::RsbRegister(rsb) => {
                consume!((s,rd,rn,rm.local_into(), shift) from rsb);
                let (rd,rn) = (rd.unwrap_or(rn.clone()).local_into(),rn.local_into());
                let mut ret = Vec::with_capacity(10);
                let carry = Operand::Flag("c".to_owned());
                let one = 1.local_into();

                local!(shifted,intermediate,old_carry);
                shift!(ret.shift rm -> shifted);

                pseudo!(
                    ret.extend[
                        // Backup carry bit
                        old_carry = carry;
                        // Set carry  bit to 1 
                        carry = one;

                        intermediate = !rn;

                        // add with carry
                        rd = intermediate adc shifted;
                    ]
                );
                ret.extend(
                    match s {
                        Some(true)  => {
                            vec![
                                Operation::SetZFlag(rd.clone()),
                                Operation::SetNFlag(rd.clone()),
                                Operation::SetCFlag { operand1: intermediate, operand2: shifted, sub: false, carry: true }
                            ]
                        }
                        _ => vec![bin_op!(carry = old_carry)]
                    }
                );


                ret
            },
            Thumb::Sadd16(sadd) => {
                consume!((
                    rn.local_into(),
                    rd.local_into().unwrap_or(rn.clone()),
                    rm.local_into()
                ) from sadd); 

                let mut ret = Vec::with_capacity(20);
                local!(rn_lsh,rn_msh,rm_lsh,rm_msh,sum1,sum2,ge_lsh,ge_msh);
                let msh_mask = (!(u16::MAX as u32)).local_into();
                let lsh_mask = (u16::MAX as u32).local_into();
                let msh_shift = 16.local_into();

                pseudo!(
                    ret.extend[
                        // Field extraction
                        rn_lsh = rn & lsh_mask;
                        rn_msh = rn & msh_mask;
                        rn_msh = rn_msh >> msh_shift;

                        rm_lsh = rm & lsh_mask;
                        rm_msh = rm & msh_mask;
                        rm_msh = rm_msh >> msh_shift;
                        // [rn_lsh = SInt(rn_lsh,16)];
                    ]
                );
                // This is likely incorrect, we should implement some version of SInt 
                ret.extend([
                           Operation::SignExtend { destination: rn_lsh.clone(), operand: rn_lsh.clone(), bits: 16  },
                           Operation::SignExtend { destination: rn_msh.clone(), operand: rn_msh.clone(), bits: 16 },
                           Operation::SignExtend { destination: rm_lsh.clone(), operand: rm_lsh.clone(), bits: 16 },
                           Operation::SignExtend { destination: rm_msh.clone(), operand: rm_msh.clone(), bits: 16 },
                ]);
                pseudo!(
                    ret.extend[
                        sum1 = rn_lsh + rm_lsh;
                        sum2 = rn_msh + rm_msh;

                        // Assign 
                        rd = sum2 << msh_shift;
                        sum1 = sum1 & lsh_mask;
                        rd = rd | sum1;
                    ]
                );
                todo!("look in to SInt")
            },
            Thumb::Sadd8(_) => todo!(),
            Thumb::Sasx(_) => todo!(),
            Thumb::SbcImmediate(sbc) => {
                consume!((
                    s.unwrap_or(false), 
                    rn.local_into(), 
                    rd.local_into().unwrap_or(rn.clone()),
                    imm.local_into()
                ) from sbc);
                let mut ret = Vec::with_capacity(7);
                pseudo!(
                    ret.extend[
                        let intermediate = ! imm;
                        let result = rn adc imm;
                        rd = result;
                    ]
                );
                if s {
                    ret.extend([
                        Operation::SetZFlag(result.clone()),
                        Operation::SetNFlag(result.clone()),
                        Operation::SetCFlag { operand1: rn.clone(), operand2: imm.clone(), sub: false, carry: true },
                        Operation::SetVFlag { operand1: rn.clone(), operand2: imm.clone(), sub: false, carry: true }

                    ]);
                }
                ret
            },
            Thumb::SbcRegister(sbc) => {
                consume!((
                    s.unwrap_or(false),
                    rn.local_into(),
                    rd.local_into().unwrap_or(rn.clone()), 
                    rm.local_into(),
                    shift
                ) from sbc);
                let mut ret = Vec::with_capacity(10);
                local!(shifted);
                shift!(ret.shift rm -> shifted);
                pseudo!(
                    ret.extend[
                        let intermediate = !shifted;
                        let result = rn adc intermediate;
                        rd = result;
                    ]
                );
                if s{
                    ret.extend([
                        Operation::SetZFlag(result.clone()),
                        Operation::SetNFlag(result.clone()),
                        Operation::SetCFlag { operand1: rn.clone(), operand2: intermediate.clone(), sub: false, carry: true },
                        Operation::SetVFlag { operand1: rn.clone(), operand2: intermediate.clone(), sub: false, carry: true }
                    ]);
                }
                ret
            },
            Thumb::Sbfx(sbfx) => {
                consume!((rd.local_into(), rn.local_into(), lsb, width) from sbfx);
                let mut ret = vec![];
                
                let msb = lsb + (width-1);
                let mask = ((1<<(msb-lsb))-1) << lsb;
                
                pseudo!(
                    ret.extend[
                        let intermediate = rn & mask.local_into();
                        intermediate = intermediate >> lsb.local_into();
                        rd = SignExtend(intermediate,width);
                    ]
                );
                ret
            },
            Thumb::Sdiv(_) => todo!("Need to revisit both SInt and div"),
            Thumb::Sel(_) => todo!("This will likely need a big rewrite as it changes behaviour based on value of flags"),
            Thumb::Sev(_) => todo!("This is likely not needed"),
            Thumb::Shadd16(_) => todo!("Need to revisit SInt"),
            Thumb::Shadd8(_) => todo!("Need to revisit SInt"),
            Thumb::Shasx(_) => todo!("Need to revisit SInt"),
            Thumb::Shsax(_) => todo!("Need to reivist SInt"),
            Thumb::Shsub16(_) => todo!("Need to revisit SInt"),
            Thumb::Shsub8(_) => todo!("Need to revisit SInt"),
            Thumb::Smla(_) => todo!("Need to revisit SInt"),
            Thumb::Smlad(_) => todo!("Need to revisit SInt"),
            Thumb::Smlal(_) => todo!("Need to revisit SInt"),
            Thumb::SmlalSelective(_) => todo!("Need to revisit SInt"),
            Thumb::Smlald(_) => todo!("Need to revisit SInt"),
            Thumb::Smlaw(_) => todo!("Need to revisit SInt"),
            Thumb::Smlsd(_) => todo!("Need to revisit SInt"),
            Thumb::Smlsld(_) => todo!("Need to revisit SInt"),
            Thumb::Smmla(_) => todo!("Need to revisit SInt"),
            Thumb::Smmls(_) => todo!("Need to revisit SInt"),
            Thumb::Smmul(_) => todo!("Need to revisit SInt"),
            Thumb::Smuad(_) => todo!("Need to revisit SInt"),
            Thumb::Smul(_) => todo!("Need to revisit SInt"),
            Thumb::Smull(_) => todo!("Need to revisit SInt"),
            Thumb::Smulw(_) => todo!("Need to revisit SInt"),
            Thumb::Smusd(_) => todo!("Need to revisit SInt"),
            Thumb::Ssat(_) => todo!("Need to revisit SInt"),
            Thumb::Ssat16(_) => todo!("Need to revisit SInt"),
            Thumb::Ssax(_) => todo!("Need to revisit SInt"),
            Thumb::Ssub16(_) => todo!("Need to revisit SInt"),
            Thumb::Ssub8(_) => todo!("Need to revisit SInt"),
            Thumb::Stm(stm) => {
                consume!((rn.local_into(), registers, w.unwrap_or(false)) from stm);
                let mut ret = vec![];
                pseudo!(
                    ret.extend[
                        let address = rn;
                    ]
                );
                let n = registers.regs.len() as u32;
                for register in registers.regs{
                    pseudo!(
                        ret.extend[
                            LocalAddress("address",32) = register.local_into();
                            address = address + 4.local_into();
                        ]
                    );
                }
                if w {
                    pseudo!(
                        ret.extend[
                            rn = rn + n.local_into();
                        ]
                    );
                }
                ret
            },
            Thumb::Stmdb(stmdb) => {
                consume!((
                    w.unwrap_or(false), 
                    rn.local_into(), 
                    registers
                ) from stmdb);
                let mut ret = vec![];
                let n = registers.regs.len() as u32;
                pseudo!(ret.extend[
                    let address = rn - (4*n).local_into();
                    for reg in registers.regs{
                            LocalAddress("address",32) = reg.local_into();
                            address = address + 4.local_into();
                    }
                    if w {
                        rn = rn - (4u32* n).local_into();
                    }
                ]);
                ret

            },
            Thumb::StrImmediate(str) => {
                consume!((
                    w.unwrap_or(false),
                    add,
                    index.unwrap_or(false), 
                    rt.local_into(),
                    rn.local_into(), 
                    imm.local_into()
                ) from str);
                let mut ret = Vec::new();
                pseudo!(
                    ret.extend[

                        let offset_addr = 0.local_into();
                        if add {
                            offset_addr = rn + imm;
                        } else {
                            offset_addr = rn - imm;
                        }

                        let address = 0.local_into();
                        if index {
                            address = offset_addr;
                        } else {
                            address = rn;
                        }
                        
                        LocalAddress("address",32) = rt;
                        
                        if w {
                            rn = offset_addr;
                        }
                    ]
                );
                ret
            },
            Thumb::StrRegister(str) => {
                consume!((
                    rt.local_into(),
                    rn.local_into(),
                    rm.local_into(),
                    shift) from str);
                let shift_n = (shift.unwrap_or((Shift::Lsl,0).try_into().unwrap()).shift_n as u32 ).local_into();
                let mut ret = vec![];
                pseudo!(ret.extend[
                    // Shift will allways be LSL on the v7
                    let offset = rm << shift_n;
                    let address = rn + offset;
                    LocalAddress("address", 32) = rt;
                ]);
                ret
            },
            Thumb::StrbImmediate(strb) => {
                consume!((w.unwrap_or(false),add,index.unwrap_or(false), rt.local_into(),rn.local_into(), imm.local_into()) from strb);
                let mut ret = Vec::new();
                pseudo!(
                    ret.extend[

                        let offset_addr = 0.local_into();
                        if add {
                            offset_addr = rn + imm;
                        } else {
                            offset_addr = rn - imm;
                        } f

                        let address = 0.local_into();
                        if index {
                            address = offset_addr;
                        } else {
                            address = rn;
                        }
                        
                        // TODO! Ensure that this is correct for writing only LS byte
                        LocalAddress("address",8) = rt;
                        
                        if w {
                            rn = offset_addr;
                        }
                    ]
                );
                ret
            },
            Thumb::StrbRegister(strb) => {
                consume!((
                    rt.local_into(),
                    rn.local_into(),
                    rm.local_into(),
                    shift
                ) from strb);
                let shift_n = (shift.unwrap_or((Shift::Lsl,0).try_into().unwrap()).shift_n as u32 ).local_into();
                let mut ret = vec![];
                pseudo!(ret.extend[
                    // Shift will allways be LSL on the v7
                    let offset = rm << shift_n;
                    let address = rn + offset;
                    LocalAddress("address", 8) = rt;
                ]);
                ret
            },
            Thumb::Strbt(strbt) => {
                consume!((
                    rt.local_into(),
                    rn.local_into(),
                    imm.unwrap_or(0).local_into()
                ) from strbt);
                let mut ret =  vec![];
                pseudo!(
                    ret.extend[
                        let address = rn + imm;
                        LocalAddress("address", 8) = rt;
                ]);

                ret
            },
            Thumb::StrdImmediate(strd) => {
                // consume!((
                //     rt.local_into(), 
                //     rt2.local_into(), 
                //     rn.local_into(),
                //     add.unwrap_or(true),
                //     index.unwrap_or(true),
                //     imm.unwrap_or(0).local_into()
                // ) from strd);
                // let mut ret = vec![];
                // pseudo!(ret.extend[
                //     let offset_addr = rn - imm;
                //     if add {
                //         offset_addr = rn + imm;
                //     }
                //
                //     let address = rn;
                //     if index {
                //         address = offset_addr;                    
                //     }
                //     LocalAddress("address",32) = rt;
                //     address = address + 4.local_into();
                //     LocalAddress("address",32) = rt2;
                //
                //     if w {
                //         rn = offset_addr;
                //     }
                // ]);
                // ret
            },
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
    pub trait ToString {
        fn to_string(self) -> String;
    }
    pub trait ToInt {
        fn to_u32(self) -> u32;
    }
}

use sealed::Into;

use self::sealed::ToString;

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
pub enum SpecialRegister {
    APSR,
    IAPSR,
    EAPSR,
    XPSR,
    IPSR,
    EPSR,
    IEPSR,
    MSP,
    PSP,
    PRIMASK,
    CONTROL,
    FAULTMASK,
}
impl Into<Operand> for SpecialRegister {
    fn local_into(self) -> Operand {
        Operand::Register(match self {
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
            SpecialRegister::FAULTMASK => "FAULTMASK".to_owned(),
        })
    }
}
