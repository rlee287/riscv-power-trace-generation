use std::str::FromStr;

#[cfg(feature = "mem_track")]
use std::collections::BTreeMap;

use crate::cpu::parsing::CPUStateStrParseErr;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StoreVal {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64)
}
impl StoreVal {
    pub fn get_strobe(store: &Self, addr: u64) -> u8{
        match store {
            StoreVal::U8(_)  => 0x01 << (addr % 8),
            StoreVal::U16(_) => 0x03 << (addr % 8),
            StoreVal::U32(_) => 0x0f << (addr % 8),
            StoreVal::U64(_) => 0xff
        }
    }
}
#[cfg(feature = "mem_track")]
impl StoreVal {
    pub fn byte_len(&self) -> usize {
        match self {
            StoreVal::U8(_) => 1,
            StoreVal::U16(_) => 2,
            StoreVal::U32(_) => 4,
            StoreVal::U64(_) => 8
        }
    }
    pub fn is_zero(&self) -> bool {
        match self {
            StoreVal::U8(v) => *v==0,
            StoreVal::U16(v) => *v==0,
            StoreVal::U32(v) => *v==0,
            StoreVal::U64(v) => *v==0
        }
    }
}
impl FromStr for StoreVal {
    type Err = CPUStateStrParseErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // We expect from_str_radix to always suceed due to expected input
        // Failure has the same semantic cause as regex failure, so report that
        match s.len() {
            1 | 2 => {
                match u8::from_str_radix(s, 16) {
                    Ok(val) => Ok(StoreVal::U8(val)),
                    Err(_) => Err(CPUStateStrParseErr::RegexFail)
                }
            },
            4 => {
                match u16::from_str_radix(s, 16) {
                    Ok(val) => Ok(StoreVal::U16(val)),
                    Err(_) => Err(CPUStateStrParseErr::RegexFail)
                }
            },
            8 => {
                match u32::from_str_radix(s, 16) {
                    Ok(val) => Ok(StoreVal::U32(val)),
                    Err(_) => Err(CPUStateStrParseErr::RegexFail)
                }
            },
            16 => {
                match u64::from_str_radix(s, 16) {
                    Ok(val) => Ok(StoreVal::U64(val)),
                    Err(_) => Err(CPUStateStrParseErr::RegexFail)
                }
            },
            _ => Err(CPUStateStrParseErr::InvalidStoreWidth(s.len()))
        }
    }
}
impl From<StoreVal> for u64 {
    fn from(store_val: StoreVal) -> Self {
        match store_val {
            StoreVal::U8(val) => u64::from(val),
            StoreVal::U16(val) => u64::from(val),
            StoreVal::U32(val) => u64::from(val),
            StoreVal::U64(val) => val,
        }
    }
}
impl Default for StoreVal {
    fn default() -> Self {
        // Assume that a full read happened before we started logging
        Self::U64(0)
    }
}
