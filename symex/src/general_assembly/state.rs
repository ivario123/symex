//! Holds the state in general assembly execution.

use std::collections::HashMap;

use tracing::{debug, trace};

use crate::{
    elf_util::Variable,
    general_assembly::{
        project::{PCHook, ProjectError},
        GAError, Result,
    },
    memory::ArrayMemory,
    smt::{DContext, DExpr, DSolver},
};

use super::{
    instruction::{Condition, Instruction},
    project::Project,
};

pub enum HookOrInstruction {
    PcHook(PCHook),
    Instruction(Instruction),
}

#[derive(Clone, Debug)]
pub struct GAState {
    pub project: &'static Project,
    pub ctx: &'static DContext,
    pub constraints: DSolver,
    pub marked_symbolic: Vec<Variable>,
    pub memory: ArrayMemory,
    pub cycle_count: u128,
    pub registers: HashMap<String, DExpr>,
    pc_register: u64, // this register is special
    flags: HashMap<String, DExpr>,
    instruction_counter: usize,
}

impl GAState {
    pub fn new(
        ctx: &'static DContext,
        project: &'static Project,
        constraints: DSolver,
        function: &str,
        end_address: u64,
    ) -> Result<Self> {
        let pc_reg = match project.get_symbol_address(function) {
            Some(a) => a,
            None => return Err(GAError::EntryFunctionNotFound(function.to_owned())),
        };
        debug!("Found function at addr: {:#X}.", pc_reg);
        let ptr_size = project.get_ptr_size();

        let sp_reg = match project.get_symbol_address("_stack_start") {
            Some(a) => Ok(a),
            None => Err(ProjectError::UnableToParseElf(
                "start of stack not found".to_owned(),
            )),
        }?;
        debug!("Found stack start at addr: {:#X}.", sp_reg);

        let memory = ArrayMemory::new(ctx, ptr_size);
        let mut registers = HashMap::new();
        let pc_expr = ctx.from_u64(pc_reg, ptr_size);
        registers.insert("PC".to_owned(), pc_expr);

        let sp_expr = ctx.from_u64(sp_reg, ptr_size);
        registers.insert("SP".to_owned(), sp_expr);

        // set the link register to max value to detect when returning from a function
        let end_pc_expr = ctx.from_u64(end_address, ptr_size);
        registers.insert("LR".to_owned(), end_pc_expr);

        let mut flags = HashMap::new();
        flags.insert("N".to_owned(), ctx.unconstrained(1, "flags.N"));
        flags.insert("Z".to_owned(), ctx.unconstrained(1, "flags.Z"));
        flags.insert("C".to_owned(), ctx.unconstrained(1, "flags.C"));
        flags.insert("V".to_owned(), ctx.unconstrained(1, "flags.V"));

        Ok(GAState {
            project,
            ctx,
            constraints,
            marked_symbolic: Vec::new(),
            memory,
            cycle_count: 0,
            registers,
            pc_register: pc_reg,
            flags,
            instruction_counter: 0,
        })
    }

    /// Increments the instruction counter by one.
    pub fn increment_instruction_count(&mut self) {
        self.instruction_counter += 1;
    }
    
    /// Gets the current instruction count
    pub fn get_instruction_count(&self) -> usize {
        self.instruction_counter
    }

    pub fn create_test_state(project: &'static Project, ctx: &'static DContext, constraints: DSolver, start_pc: u64, start_stack: u64) -> Self {
        let pc_reg = start_pc;
        let ptr_size = project.get_ptr_size();

        let sp_reg = start_stack;
        debug!("Found stack start at addr: {:#X}.", sp_reg);

        let memory = ArrayMemory::new(ctx, ptr_size);
        let mut registers = HashMap::new();
        let pc_expr = ctx.from_u64(pc_reg, ptr_size);
        registers.insert("PC".to_owned(), pc_expr);

        let sp_expr = ctx.from_u64(sp_reg, ptr_size);
        registers.insert("SP".to_owned(), sp_expr);

        let mut flags = HashMap::new();
        flags.insert("N".to_owned(), ctx.unconstrained(1, "flags.N"));
        flags.insert("Z".to_owned(), ctx.unconstrained(1, "flags.Z"));
        flags.insert("C".to_owned(), ctx.unconstrained(1, "flags.C"));
        flags.insert("V".to_owned(), ctx.unconstrained(1, "flags.V"));

        GAState {
            project,
            ctx,
            constraints,
            marked_symbolic: Vec::new(),
            memory,
            cycle_count: 0,
            registers,
            pc_register: pc_reg,
            flags,
            instruction_counter: 0,
        }
    }

    pub fn set_register(&mut self, register: String, expr: DExpr) {
        // crude solution should prbobly change
        if register == "PC" {
            let value = match expr.get_constant() {
                Some(v) => v,
                None => todo!("handle branch to symbolic address"),
            };
            self.pc_register = value;
        }
        self.registers.insert(register, expr);
    }

    pub fn set_pc(&mut self, value: u64) {
        self.pc_register = value
    }

    pub fn get_pc(&self) -> u64 {
        self.pc_register
    }

    pub fn get_register(&self, register: String) -> Option<DExpr> {
        match self.registers.get(&register) {
            Some(v) => Some(v.to_owned()),
            None => None,
        }
    }

    pub fn set_flag(&mut self, flag: String, expr: DExpr) {
        trace!("flag {} set to {:?}", flag, expr);
        self.flags.insert(flag, expr);
    }

    pub fn get_flag(&mut self, flag: String) -> Option<DExpr> {
        match self.flags.get(&flag) {
            Some(v) => Some(v.to_owned()),
            None => todo!(),
        }
    }

    pub fn get_expr(&mut self, condition: &Condition) -> Result<DExpr> {
        Ok(match condition {
            Condition::EQ => self.get_flag("Z".to_owned()).unwrap(),
            Condition::NE => self.get_flag("Z".to_owned()).unwrap().not(),
            Condition::CS => self.get_flag("C".to_owned()).unwrap(),
            Condition::CC => self.get_flag("C".to_owned()).unwrap().not(),
            Condition::MI => self.get_flag("N".to_owned()).unwrap(),
            Condition::PL => self.get_flag("N".to_owned()).unwrap().not(),
            Condition::VS => self.get_flag("V".to_owned()).unwrap(),
            Condition::VC => self.get_flag("V".to_owned()).unwrap().not(),
            Condition::HI => {
                let c = self.get_flag("C".to_owned()).unwrap();
                let z = self.get_flag("Z".to_owned()).unwrap().not();
                c.and(&z)
            }
            Condition::LS => {
                let c = self.get_flag("C".to_owned()).unwrap().not();
                let z = self.get_flag("Z".to_owned()).unwrap();
                c.or(&z)
            }
            Condition::GE => {
                let n = self.get_flag("N".to_owned()).unwrap();
                let v = self.get_flag("V".to_owned()).unwrap();
                n._eq(&v)
            }
            Condition::LT => {
                let n = self.get_flag("N".to_owned()).unwrap();
                let v = self.get_flag("V".to_owned()).unwrap();
                n._ne(&v)
            }
            Condition::GT => {
                let z = self.get_flag("Z".to_owned()).unwrap();
                let n = self.get_flag("N".to_owned()).unwrap();
                let v = self.get_flag("V".to_owned()).unwrap();
                z.not().and(&n._eq(&v))
            }
            Condition::LE => {
                let z = self.get_flag("Z".to_owned()).unwrap();
                let n = self.get_flag("N".to_owned()).unwrap();
                let v = self.get_flag("V".to_owned()).unwrap();
                z.and(&n._ne(&v))
            }
            Condition::None => self.ctx.from_bool(true),
        })
    }

    pub fn get_next_instruction(&self) -> Result<HookOrInstruction> {
        let pc = self.pc_register & !(0b1); // Not applicable for all architectures TODO: Fix this.;
        match self.project.get_pc_hook(pc) {
            Some(hook) => Ok(HookOrInstruction::PcHook(hook)),
            None => Ok(HookOrInstruction::Instruction(
                self.project.get_instruction(pc)?,
            )),
        }
    }

    fn read_word_from_memory_no_static(&self, address: &DExpr) -> Result<DExpr> {
        Ok(self.memory.read(address, self.project.get_word_size())?)
    }

    fn write_word_from_memory_no_static(&mut self, address: &DExpr, value: DExpr) -> Result<()> {
        Ok(self.memory.write(address, value)?)
    }

    pub fn read_word_from_memory(&self, address: &DExpr) -> Result<DExpr> {
        match address.get_constant() {
            Some(address_const) => {
                if self.project.address_in_range(address_const) {
                    // read from static memmory in project
                    let value = match self.project.get_word(address_const)? {
                        crate::general_assembly::DataWord::Word64(data) => {
                            self.ctx.from_u64(data, 64)
                        }
                        crate::general_assembly::DataWord::Word32(data) => {
                            self.ctx.from_u64(data as u64, 32)
                        }
                        crate::general_assembly::DataWord::Word16(data) => {
                            self.ctx.from_u64(data as u64, 16)
                        }
                        crate::general_assembly::DataWord::Word8(data) => {
                            self.ctx.from_u64(data as u64, 8)
                        }
                    };
                    Ok(value)
                } else {
                    self.read_word_from_memory_no_static(address)
                }
            }

            // For non constant addresses always read non_static memmory
            None => self.read_word_from_memory_no_static(address),
        }
    }

    pub fn write_word_to_memory(&mut self, address: &DExpr, value: DExpr) -> Result<()> {
        match address.get_constant() {
            Some(address_const) => {
                if self.project.address_in_range(address_const) {
                    Err(GAError::WritingToStaticMemoryProhibited)
                } else {
                    self.write_word_from_memory_no_static(address, value)
                }
            }

            // For non constant addresses always read non_static memmory
            None => self.write_word_from_memory_no_static(address, value),
        }
    }
}
