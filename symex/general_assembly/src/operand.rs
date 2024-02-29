//! Defines all types of operands that are valid in [`GeneralAssembly`]



#[derive(Debug, Clone, Copy)]
pub enum DataWord {
    Word64(u64),
    Word32(u32),
    Word16(u16),
    Word8(u8),
}

impl Into<u64> for DataWord {
    fn into(self) -> u64 {
        match self {
            DataWord::Word64(v) => v as u64,
            DataWord::Word32(v) => v as u64,
            DataWord::Word16(v) => v as u64,
            DataWord::Word8(v) => v as u64,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum RawDataWord {
    Word64([u8; 8]),
    Word32([u8; 4]),
    Word16([u8; 2]),
    Word8([u8; 1]),
}

#[derive(Debug, Clone, Copy)]
pub enum DataHalfWord {
    HalfWord64(u32),
    HalfWord32(u16),
    HalfWord16(u8),
}

impl Into<DataWord> for DataHalfWord {
    fn into(self) -> DataWord {
        match self {
            DataHalfWord::HalfWord64(v) => DataWord::Word64(v as u64),
            DataHalfWord::HalfWord32(v) => DataWord::Word32(v as u32),
            DataHalfWord::HalfWord16(v) => DataWord::Word16(v as u16),
        }
    }
}

/// A operand representing some value.
#[derive(Debug, Clone)]
pub enum Operand {
    /// Representing a value in a register.
    Register(String),

    /// Representing an immediate value.
    Immidiate(DataWord),

    /// Representing the value stored in memory
    /// at the address stored in a local.
    ///
    /// Reads the specified number of bits from memory.
    AddressInLocal(String, u32),

    /// Representing the value stored in memory
    /// at the constant address.
    Address(DataWord, u32),

    /// Representing the value stored in memory
    /// at the address stored in a register offset
    /// by an constant value.
    AddressWithOffset {
        address: DataWord,
        offset_reg: String,
        width: u32,
    },

    /// Represent the value that is local to the instruction.
    Local(String),

    /// Represents a flag in the core.
    Flag(String),
}
