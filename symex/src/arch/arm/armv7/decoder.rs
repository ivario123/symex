use crate::general_assembly::{
    instruction::Instruction as GAInstruction,
    instruction::{self, Condition as GACondition, Operand as GAOperand, Operation},
    translator::Translatable,
    DataWord,
};
use yaxpeax_arm::armv7::{ConditionCode, Instruction, Opcode, Operand};

impl Translatable for Instruction {
    fn translate(&self) -> GAInstruction {
        // let op: Vec<Operation> = self.local_into();
        GAInstruction{
            instruction_size:32,operations:vec![Operation::Nop],max_cycle:instruction::CycleCount::Value(1)
        }
    }
}

impl Into<Vec<Operation>> for &Instruction {
    fn local_into(self) -> Vec<Operation> {
        match self.opcode {
            // From alphabetic listing

            // Add with carry
            //
            // TODO! Validate that shift is allready accounted for
            Opcode::ADC => {
                // 2 cases
                //
                // 1. d = rn + imm8

                let (n, m) = match self.operands[2] {
                    // d == n
                    Operand::Nothing => (0, 1),
                    _ => (1, 2),
                };
                let (n, m) = (self.operands[n].local_into(), self.operands[m].local_into());

                let mut operations = if self.s {
                    vec![
                        Operation::SetCFlag {
                            operand1: n.clone(),
                            operand2: m.clone(),
                            sub: false,
                            carry: true,
                        },
                        Operation::SetVFlag {
                            operand1: n.clone(),
                            operand2: m.clone(),
                            sub: false,
                            carry: true,
                        },
                    ]
                } else {
                    vec![]
                };
                operations.extend([
                    Operation::Adc {
                        destination: self.operands[0].local_into(),
                        operand1: n,
                        operand2: m,
                    },
                    Operation::SetNFlag(self.operands[0].local_into()),
                    Operation::SetZFlag(self.operands[0].local_into()),
                ]);
                operations
            }
            Opcode::ADD => {
                // 2 cases
                //
                // 1. d = rn + imm8
                //
                // TODO! Validate that this works for ADD sp (A7.7.5) and (A7.7.6)

                let (n, m) = match self.operands[2] {
                    // d == n
                    Operand::Nothing => (0, 1),
                    _ => (1, 2),
                };
                let (n, m) = (self.operands[n].local_into(), self.operands[m].local_into());

                let mut operations = if self.s {
                    vec![
                        Operation::SetCFlag {
                            operand1: n.clone(),
                            operand2: m.clone(),
                            sub: false,
                            carry: false,
                        },
                        Operation::SetVFlag {
                            operand1: n.clone(),
                            operand2: m.clone(),
                            sub: false,
                            carry: false,
                        },
                    ]
                } else {
                    vec![]
                };
                operations.extend([
                    Operation::Add {
                        destination: self.operands[0].local_into(),
                        operand1: n,
                        operand2: m,
                    },
                    Operation::SetNFlag(self.operands[0].local_into()),
                    Operation::SetZFlag(self.operands[0].local_into()),
                ]);

                operations
            }

            Opcode::ADR => {
                let (n, m) = match self.operands[2] {
                    // d == n
                    Operand::Nothing => (0, 1),
                    _ => (1, 2),
                };
                let (n, m) = (self.operands[n].local_into(), self.operands[m].local_into());
                // Might need to rework this when I know how the arguments are parsed
                let operations = vec![Operation::Add {
                    destination: self.operands[0].local_into(),
                    operand1: n,
                    operand2: m,
                }];
                operations
            }

            Opcode::AND => {
                let (n, m) = match self.operands[2] {
                    // d == n
                    Operand::Nothing => (0, 1),
                    _ => (1, 2),
                };
                let (n, m) = (self.operands[n].local_into(), self.operands[m].local_into());

                let mut operations = if self.s {
                    vec![Operation::SetCFlag {
                        operand1: n.clone(),
                        operand2: m.clone(),
                        sub: false,
                        carry: false,
                    }]
                } else {
                    vec![]
                };
                operations.extend([
                    Operation::And {
                        destination: self.operands[0].local_into(),
                        operand1: n,
                        operand2: m,
                    },
                    Operation::SetNFlag(self.operands[0].local_into()),
                    Operation::SetZFlag(self.operands[0].local_into()),
                ]);
                operations
            }
            Opcode::ASR => {
               todo!(""); 
            }

            //
            Opcode::NOP => vec![Operation::Nop],

            // Branching instructions
            Opcode::B => {
                vec![Operation::ConditionalJump {
                    destination: self.operands[0].local_into(),
                    condition: self.condition.local_into(),
                }]
            }

            Opcode::CBZ => todo!("Conditional branch on zero"),
            Opcode::CBNZ => todo!("Conditional branch on non zero"),
            Opcode::BL => todo!("Branch and link"),
            Opcode::BLX => todo!("Branch link and exchange"),
            Opcode::BX => todo!("Branch and exchange"),

            // Unique branching
            Opcode::TBB => todo!(),
            Opcode::TBH => todo!(),

            // ==================== Arithmetic Operations (A4.4.1) ====================
            // Shifting
            Opcode::LSL => todo!("logical left shift A7.7.67"),
            Opcode::LSR => todo!("logical right shift"),
            Opcode::ASR => todo!("arithmetic right shift"),
            Opcode::ROR => todo!("rotate right"),
            Opcode::RRX => todo!("rotate right with sign extend"),
            // --------------- Table A4-2 ---------------
            // Addition
            Opcode::ADC => todo!("Add with carry"),
            Opcode::ADD => todo!("Add"),
            Opcode::ADR => todo!("Add from PC"),

            // Bit ops
            Opcode::BIC => todo!("Bitwise clear"),
            Opcode::CMN => todo!("Compare negative"),
            Opcode::CMP => todo!("Compare"),
            Opcode::EOR => todo!("Bitwise xor"),
            Opcode::MOV => todo!("Coppy operand to destination, has one and only one operand"),
            Opcode::MVN => todo!("Bitwise not, has one and only one operand"),
            Opcode::ORN => todo!("Bitwise or not"),
            Opcode::ORR => todo!("Bitwise or"),

            // Subtraction
            Opcode::RSB => todo!("Reverse subtract, subtracts $1 from $2"),
            Opcode::SBC => todo!("Subtract with carry"),
            Opcode::SUB => todo!("Subtract"),

            // Testing
            Opcode::TEQ => todo!("Set flags if operands are equal"),
            Opcode::TST => todo!("Not clear at all, refference sets test  but test what"),

            // ==================== Multiply instructions (A4.4.3) ====================

            //  -------- Without signed unsigned distinction (A4-4)
            Opcode::MLA => todo!("Multiply and accumulate d = a + n * m"),
            Opcode::MLS => todo!("Multiply and sub d = a - n*m"),
            Opcode::MUL => todo!("Multiply d = n*m || d = d*n"),

            //  -------- Signed multiply instruction (A4-5)
            Opcode::SMLAL => {
                todo!("Signed multiply and accumulate long  d = a + n * m  but a and d are 64 bit")
            }
            Opcode::SMULL => todo!("Signed multiply long d = n*m but d is 64 bit"),

            // -------- Signed multiply DSP needs modification I belive (A4-6)
            Opcode::SMLA(false, false) => todo!("Signed Multiply Accumulate (halfwords) lsb,lsb"),
            Opcode::SMLA(false, true) => todo!("Signed Multiply Accumulate (halfwords) lsb,msb"),
            Opcode::SMLA(true, false) => todo!("Signed Multiply Accumulate (halfwords) msb,lsb"),
            Opcode::SMLA(true, true) => todo!("Signed Multiply Accumulate (halfwords) msb,msb"),
            // This might be incorrect I am not quite sure what else it would be though
            Opcode::SMLAD => todo!("Signed multiplication d = a + n*m + n*m"),
            Opcode::SMLAL_halfword(false, false) => {
                todo!("Long Signed Multiply Accumulate (halfwords), lsb,lsb")
            }
            Opcode::SMLAL_halfword(false, true) => {
                todo!("Long Signed Multiply Accumulate (halfwords), lsb,msb")
            }
            Opcode::SMLAL_halfword(true, false) => {
                todo!("Long Signed Multiply Accumulate (halfwords), msb,lsb")
            }
            Opcode::SMLAL_halfword(true, true) => {
                todo!("Long Signed Multiply Accumulate (halfwords), msb,msb")
            }

            _ => todo!(),
        }
    }
}

mod sealed {
    pub trait Into<T> {
        fn local_into(self) -> T;
    }
}

use sealed::Into;

impl sealed::Into<GACondition> for ConditionCode {
    fn local_into(self) -> GACondition {
        match self {
            Self::EQ => GACondition::EQ,
            Self::NE => GACondition::NE,
            Self::MI => GACondition::MI,
            Self::PL => GACondition::PL,
            Self::VS => GACondition::VS,
            Self::VC => GACondition::VC,
            Self::HI => GACondition::HI,
            Self::GE => GACondition::GE,
            Self::LT => GACondition::LT,
            Self::GT => GACondition::GT,
            Self::LS => GACondition::LS,
            Self::LE => GACondition::LE,
            Self::HS => todo!(),
            Self::LO => todo!(),
            Self::AL => todo!(),
        }
    }
}

impl sealed::Into<GAOperand> for yaxpeax_arm::armv7::Operand {
    fn local_into(self) -> GAOperand {
        match self {
            // This might be horribly wrong
            // TODO! Look in to difference between RegWBack and Reg
            // TODO! Find out what dereff means here
            Self::Reg(r) | Self::RegWBack(r, _) | Self::RegDeref(r) => {
                GAOperand::Register(format!("r{}", r.number()))
            }
            Self::Imm12(i) => GAOperand::Immidiate(DataWord::Word16(i)),
            Self::Imm32(i) => GAOperand::Immidiate(DataWord::Word32(i)),

            // The following two casts might be incorrect
            Self::BranchOffset(i) => GAOperand::Immidiate(DataWord::Word32(i as u32)),
            // This might be incorrect as the i might be left shifted by 1
            Self::BranchThumbOffset(i) => GAOperand::Immidiate(DataWord::Word32(i as u32)),

            Self::StatusRegMask(mask) => GAOperand::Immidiate(DataWord::Word8(mask as u8)),

            // Controll registers might need some specific management
            // TODO! Validate if it needs any other management
            Self::CReg(r) => GAOperand::Register(format!("r{}", r.number())),

            // This might need extension of the GA since this would be used in batch numerical
            // operations
            Self::RegList(i) => GAOperand::Immidiate(DataWord::Word16(i)),

            // Maybe not needed
            Self::BankedSPSR(_b) => todo!("Implement banked status regs"),
            Self::BankedReg(_bank, _reg) => todo!("Implement banked regs"),
            Self::CoprocOption(_o) => todo!("This might not be needed at all, Co Processor Option"),
            Self::Coprocessor(_i) => todo!("This might not be needed at all, Co Processor Call"),
            Self::RegShift(_r) => todo!("No support for RegShit"),
            Self::RegDerefPreindexReg(_, _, _, _) => todo!(),
            Self::RegDerefPreindexOffset(_, _, _, _) => todo!(),
            Self::RegDerefPreindexRegShift(_, _, _, _) => todo!(),
            Self::RegDerefPostindexReg(_, _, _, _) => todo!(),
            Self::RegDerefPostindexOffset(_, _, _, _) => todo!(),
            Self::RegDerefPostindexRegShift(_, _, _, _) => todo!(),

            Self::APSR => todo!("Add status registers if needed B1.4.2"),
            Self::SPSR => todo!("What is this?"),
            Self::CPSR => todo!("What is this?"),

            Self::Nothing => panic!("This should be unreachable"),
        }
    }
}
