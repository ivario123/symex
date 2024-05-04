use std::{collections::HashMap, fmt::Debug, fs};

use armv6_m_instruction_parser::parse;
use gimli::{DebugAbbrev, DebugInfo, DebugStr};
use object::{Architecture, File, Object, ObjectSection, ObjectSymbol};
use tracing::debug;

use crate::{general_assembly::translator::Translatable, memory::MemoryError, smt::DExpr};

use self::segments::{Segment, Segments};

use super::{
    instruction::Instruction,
    state::{self, GAState},
    DataHalfWord, DataWord, Endianness, RawDataWord, Result as SuperResult, RunConfig, WordSize,
};

mod dwarf_helper;
use dwarf_helper::*;

mod segments;

type Result<T> = std::result::Result<T, ProjectError>;

#[derive(Clone, Debug, Eq, PartialEq, thiserror::Error)]
pub enum ProjectError {
    #[error("Unable to parse elf file: {0}")]
    UnableToParseElf(String),

    #[error("Program memmory error")]
    ProgramMemmoryError(#[from] MemoryError),

    #[error("Unavalable operation")]
    UnabvalableOperation,
}

#[derive(Debug, Clone, Copy)]
pub enum PCHook {
    Continue,
    EndSuccess,
    EndFaliure(&'static str),
    Intrinsic(fn(state: &mut GAState) -> SuperResult<()>),
    Suppress,
}

pub type PCHooks = HashMap<u64, PCHook>;

/// Hook for a register read.
pub type RegisterReadHook = fn(state: &mut GAState) -> SuperResult<DExpr>;
pub type RegisterReadHooks = HashMap<String, RegisterReadHook>;

/// Hook for a register write.
pub type RegisterWriteHook = fn(state: &mut GAState, value: DExpr) -> SuperResult<()>;
pub type RegisterWriteHooks = HashMap<String, RegisterWriteHook>;

#[derive(Debug, Clone)]
pub enum MemoryHookAddress {
    Single(u64),
    Range(u64, u64),
}

/// Hook for a memory write.
pub type MemoryWriteHook =
    fn(state: &mut GAState, address: u64, value: DExpr, bits: u32) -> SuperResult<()>;
pub type SingleMemoryWriteHooks = HashMap<u64, MemoryWriteHook>;
pub type RangeMemoryWriteHooks = Vec<((u64, u64), MemoryWriteHook)>;

/// Hook for a memory read.
pub type MemoryReadHook = fn(state: &mut GAState, address: u64) -> SuperResult<DExpr>;
pub type SingleMemoryReadHooks = HashMap<u64, MemoryReadHook>;
pub type RangeMemoryReadHooks = Vec<((u64, u64), MemoryReadHook)>;

/// Holds all data read from the ELF file.
// Add all read only memmory here later to handle global constants.
pub struct Project {
    segments: Segments,
    word_size: WordSize,
    endianness: Endianness,
    architecture: object::Architecture,
    symtab: HashMap<String, u64>,
    pc_hooks: PCHooks,
    reg_read_hooks: RegisterReadHooks,
    reg_write_hooks: RegisterWriteHooks,
    single_memory_read_hooks: SingleMemoryReadHooks,
    range_memory_read_hooks: RangeMemoryReadHooks,
    single_memory_write_hooks: SingleMemoryWriteHooks,
    range_memory_write_hooks: RangeMemoryWriteHooks,
}

fn construct_register_read_hooks(hooks: Vec<(String, RegisterReadHook)>) -> RegisterReadHooks {
    let mut ret = HashMap::new();
    for (register, hook) in hooks {
        ret.insert(register, hook);
    }
    ret
}

fn construct_register_write_hooks(hooks: Vec<(String, RegisterWriteHook)>) -> RegisterWriteHooks {
    let mut ret = HashMap::new();

    for (register, hook) in hooks {
        ret.insert(register, hook);
    }

    ret
}

fn construct_memory_write(
    hooks: Vec<(MemoryHookAddress, MemoryWriteHook)>,
) -> (SingleMemoryWriteHooks, RangeMemoryWriteHooks) {
    let mut single_hooks = HashMap::new();
    let mut range_hooks = vec![];

    for (address, hook) in hooks {
        match address {
            MemoryHookAddress::Single(addr) => {
                single_hooks.insert(addr, hook);
            }
            MemoryHookAddress::Range(start, end) => {
                range_hooks.push(((start, end), hook));
            }
        }
    }

    (single_hooks, range_hooks)
}

fn construct_memory_read_hooks(
    hooks: Vec<(MemoryHookAddress, MemoryReadHook)>,
) -> (SingleMemoryReadHooks, RangeMemoryReadHooks) {
    let mut single_hooks = HashMap::new();
    let mut range_hooks = vec![];

    for (address, hook) in hooks {
        match address {
            MemoryHookAddress::Single(addr) => {
                single_hooks.insert(addr, hook);
            }
            MemoryHookAddress::Range(start, end) => {
                range_hooks.push(((start, end), hook));
            }
        }
    }

    (single_hooks, range_hooks)
}

impl Project {
    pub fn manual_project(
        program_memory: Vec<u8>,
        start_addr: u64,
        end_addr: u64,
        word_size: WordSize,
        endianness: Endianness,
        architecture: object::Architecture,
        symtab: HashMap<String, u64>,
        pc_hooks: PCHooks,
        reg_read_hooks: RegisterReadHooks,
        reg_write_hooks: RegisterWriteHooks,
        single_memory_read_hooks: SingleMemoryReadHooks,
        range_memory_read_hooks: RangeMemoryReadHooks,
        single_memory_write_hooks: SingleMemoryWriteHooks,
        range_memory_write_hooks: RangeMemoryWriteHooks,
    ) -> Project {
        Project {
            segments: Segments::from_single_segment(program_memory, start_addr, end_addr),
            word_size,
            endianness,
            architecture,
            symtab,
            pc_hooks,
            reg_read_hooks,
            reg_write_hooks,
            single_memory_read_hooks,
            range_memory_read_hooks,
            single_memory_write_hooks,
            range_memory_write_hooks,
        }
    }

    pub fn from_path(path: &str, cfg: &mut RunConfig) -> Result<Self> {
        debug!("Parsing elf file: {}", path);
        let file = fs::read(path).expect("Unable to open file.");
        let obj_file = match object::File::parse(&*file) {
            Ok(x) => x,
            Err(e) => {
                debug!("Error: {}", e);
                return Err(ProjectError::UnableToParseElf(path.to_owned()));
            }
        };

        let segments = Segments::from_file(&obj_file);

        let endianness = if obj_file.is_little_endian() {
            Endianness::Little
        } else {
            Endianness::Big
        };

        let architecture = obj_file.architecture();

        // Do not catch 16 or 8 bit architectures but will do for now.
        let word_size = if obj_file.is_64() {
            WordSize::Bit64
        } else {
            WordSize::Bit32
        };

        let mut symtab = HashMap::new();
        for symbol in obj_file.symbols() {
            symtab.insert(
                match symbol.name() {
                    Ok(name) => name.to_owned(),
                    Err(_) => continue, // ignore entry if name can not be read
                },
                symbol.address(),
            );
        }

        let gimli_endian = match endianness {
            Endianness::Little => gimli::RunTimeEndian::Little,
            Endianness::Big => gimli::RunTimeEndian::Big,
        };

        let debug_info = obj_file.section_by_name(".debug_info").unwrap();
        let debug_info = DebugInfo::new(debug_info.data().unwrap(), gimli_endian);

        let debug_abbrev = obj_file.section_by_name(".debug_abbrev").unwrap();
        let debug_abbrev = DebugAbbrev::new(debug_abbrev.data().unwrap(), gimli_endian);

        let debug_str = obj_file.section_by_name(".debug_str").unwrap();
        let debug_str = DebugStr::new(debug_str.data().unwrap(), gimli_endian);


        match architecture {
            Architecture::Arm => {
                armv6_m_instruction_parser::instructons::Instruction::add_hooks(cfg)
            }
            _ => todo!(),
        }
        let pc_hooks = cfg.pc_hooks.clone();

        let pc_hooks =
            construct_pc_hooks_no_index(pc_hooks, &debug_info, &debug_abbrev, &debug_str);

        debug!("Created pc hooks: {:?}", pc_hooks);

        let reg_read_hooks = construct_register_read_hooks(cfg.register_read_hooks.clone());
        let reg_write_hooks = construct_register_write_hooks(cfg.register_write_hooks.clone());

        let (single_memory_write_hooks, range_memory_write_hooks) =
            construct_memory_write(cfg.memory_write_hooks.clone());
        let (single_memory_read_hooks, range_memory_read_hooks) =
            construct_memory_read_hooks(cfg.memory_read_hooks.clone());

        Ok(Project {
            segments,
            word_size,
            endianness,
            architecture,
            symtab,
            pc_hooks,
            reg_read_hooks,
            reg_write_hooks,
            single_memory_read_hooks,
            range_memory_read_hooks,
            single_memory_write_hooks,
            range_memory_write_hooks,
        })
    }

    pub fn get_pc_hook(&self, pc: u64) -> Option<PCHook> {
        self.pc_hooks.get(&pc).copied()
    }

    pub fn add_pc_hook(&mut self, pc: u64, hook: PCHook) {
        self.pc_hooks.insert(pc, hook);
    }

    pub fn get_register_read_hook(&self, register: &str) -> Option<RegisterReadHook> {
        self.reg_read_hooks.get(register).copied()
    }

    pub fn get_register_write_hook(&self, register: &str) -> Option<RegisterWriteHook> {
        self.reg_write_hooks.get(register).copied()
    }

    pub fn get_memory_write_hook(&self, address: u64) -> Option<MemoryWriteHook> {
        match self.single_memory_write_hooks.get(&address) {
            Some(hook) => Some(hook.clone()),
            None => {
                for ((start, end), hook) in &self.range_memory_write_hooks {
                    if address >= *start && address < *end {
                        return Some(hook.to_owned());
                    }
                }
                None
            }
        }
    }

    pub fn get_memory_read_hook(&self, address: u64) -> Option<MemoryReadHook> {
        match self.single_memory_read_hooks.get(&address) {
            Some(hook) => Some(hook.clone()),
            None => {
                for ((start, end), hook) in &self.range_memory_read_hooks {
                    if address >= *start && address < *end {
                        return Some(hook.to_owned());
                    }
                }
                None
            }
        }
    }

    pub fn address_in_range(&self, address: u64) -> bool {
        if let Some(_) = self.segments.read_raw_bytes(address, 1) {
            true
        } else {
            false
        }
    }

    pub fn get_word_size(&self) -> u32 {
        self.get_ptr_size() // same for now
    }

    pub fn get_endianness(&self) -> Endianness {
        self.endianness.clone()
    }

    pub fn get_ptr_size(&self) -> u32 {
        // This is an oversimplification and not true for some architectures
        // But will do and should map to the addresses in the elf
        match self.word_size {
            WordSize::Bit64 => 64,
            WordSize::Bit32 => 32,
            WordSize::Bit16 => 16,
            WordSize::Bit8 => 8,
        }
    }

    /// Get the address of a symbol from the ELF symbol table
    pub fn get_symbol_address(&self, symbol: &str) -> Option<u64> {
        self.symtab.get(symbol).copied()
    }

    /// Get the instruction att a address
    pub fn get_instruction(&self, address: u64) -> Result<Instruction> {
        debug!("Reading instruction from address: {:#010X}", address);
        match self.get_raw_word(address)? {
            RawDataWord::Word64(d) => self.instruction_from_array_ptr(&d),
            RawDataWord::Word32(d) => self.instruction_from_array_ptr(&d),
            RawDataWord::Word16(d) => self.instruction_from_array_ptr(&d),
            RawDataWord::Word8(_) => todo!(),
        }
    }

    fn instruction_from_array_ptr(&self, data: &[u8]) -> Result<Instruction> {
        match self.architecture {
            object::Architecture::Arm => {
                // probobly right add more cheks later or custom enum etc.
                let arm_instruction = parse(data).unwrap();
                debug!("instruction read: {:?}", arm_instruction);
                Ok(arm_instruction.translate())
            }
            _ => todo!(),
        }
    }

    /// Get a byte of data from program memory.
    pub fn get_byte(&self, address: u64) -> Result<u8> {
        match self.segments.read_raw_bytes(address, 1) {
            Some(v) => Ok(v[0]),
            None => Err(MemoryError::OutOfBounds.into()),
        }
    }

    fn get_word_internal(&self, address: u64, width: WordSize) -> Result<DataWord> {
        Ok(match width {
            WordSize::Bit64 => match self.segments.read_raw_bytes(address, 8) {
                Some(v) => {
                    let mut data = [0; 8];
                    data.copy_from_slice(v);
                    DataWord::Word64(match self.endianness {
                        Endianness::Little => u64::from_le_bytes(data),
                        Endianness::Big => u64::from_be_bytes(data),
                    })
                }
                None => {
                    return Err(MemoryError::OutOfBounds.into());
                }
            },
            WordSize::Bit32 => match self.segments.read_raw_bytes(address, 4) {
                Some(v) => {
                    let mut data = [0; 4];
                    data.copy_from_slice(v);
                    DataWord::Word32(match self.endianness {
                        Endianness::Little => u32::from_le_bytes(data),
                        Endianness::Big => u32::from_be_bytes(data),
                    })
                }
                None => {
                    return Err(MemoryError::OutOfBounds.into());
                }
            },
            WordSize::Bit16 => match self.segments.read_raw_bytes(address, 2) {
                Some(v) => {
                    let mut data = [0; 2];
                    data.copy_from_slice(v);
                    DataWord::Word16(match self.endianness {
                        Endianness::Little => u16::from_le_bytes(data),
                        Endianness::Big => u16::from_be_bytes(data),
                    })
                }
                None => {
                    return Err(MemoryError::OutOfBounds.into());
                }
            },
            WordSize::Bit8 => DataWord::Word8(self.get_byte(address)?),
        })
    }

    /// Get a word from data memory
    pub fn get_word(&self, address: u64) -> Result<DataWord> {
        self.get_word_internal(address, self.word_size)
    }

    pub fn get_half_word(&self, address: u64) -> Result<DataHalfWord> {
        Ok(match self.word_size {
            WordSize::Bit64 => match self.get_word_internal(address, WordSize::Bit32)? {
                DataWord::Word32(d) => DataHalfWord::HalfWord64(d),
                _ => panic!("Should never reach this part."),
            },
            WordSize::Bit32 => match self.get_word_internal(address, WordSize::Bit16)? {
                DataWord::Word16(d) => DataHalfWord::HalfWord32(d),
                _ => panic!("Should never reach this part."),
            },
            WordSize::Bit16 => match self.get_word_internal(address, WordSize::Bit8)? {
                DataWord::Word8(d) => DataHalfWord::HalfWord16(d),
                _ => panic!("Should never reach this part."),
            },
            WordSize::Bit8 => return Err(ProjectError::UnabvalableOperation),
        })
    }

    pub fn get_raw_word(&self, address: u64) -> Result<RawDataWord> {
        Ok(match self.word_size {
            WordSize::Bit64 => match self.segments.read_raw_bytes(address, 8) {
                Some(v) => {
                    let mut data = [0; 8];
                    data.copy_from_slice(v);
                    RawDataWord::Word64(data)
                }
                None => {
                    return Err(MemoryError::OutOfBounds.into());
                }
            },
            WordSize::Bit32 => match self.segments.read_raw_bytes(address, 4) {
                Some(v) => {
                    let mut data = [0; 4];
                    data.copy_from_slice(v);
                    RawDataWord::Word32(data)
                }
                None => {
                    return Err(MemoryError::OutOfBounds.into());
                }
            },
            WordSize::Bit16 => match self.segments.read_raw_bytes(address, 2) {
                Some(v) => {
                    let mut data = [0; 2];
                    data.copy_from_slice(v);
                    RawDataWord::Word16(data)
                }
                None => {
                    return Err(MemoryError::OutOfBounds.into());
                }
            },
            WordSize::Bit8 => RawDataWord::Word8([self.get_byte(address)?]),
        })
    }
}

impl Debug for Project {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Project")
            .field("word_size", &self.word_size)
            .field("endianness", &self.endianness)
            .field("architecture", &self.architecture)
            .finish()
    }
}
