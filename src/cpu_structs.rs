use std::collections::{BTreeSet, BTreeMap};
use std::ops::BitXor;

use lazy_static::lazy_static;
use regex::Regex;
use std::str::FromStr;
use std::fmt;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct CPUState {
    privilege_state: u8,
    pc: u64,
    instr: u32,
    xregs: [u64; 31], // + zero-reg
    memaddr: u64,
    memory: BTreeMap<u64, u8>
}
impl BitXor for &CPUState {
    type Output = CPUState;
    fn bitxor(self, rhs: Self) -> Self::Output {
        let mut new_xregs = self.xregs;
        for i in 0..31 {
            new_xregs[i] ^= rhs.xregs[i];
        }

        // Copy over distinct values for each, and then XOR shared ones
        let self_mem_keys: BTreeSet<_> = self.memory.keys().collect();
        let rhs_mem_keys: BTreeSet<_> = self.memory.keys().collect();
        let mut new_memory = BTreeMap::new();
        for self_key in self_mem_keys.difference(&rhs_mem_keys) {
            new_memory.insert(**self_key, *self.memory.get(self_key).unwrap()).ok_or(()).unwrap_err();
        }
        for rhs_key in rhs_mem_keys.difference(&self_mem_keys) {
            new_memory.insert(**rhs_key, *rhs.memory.get(rhs_key).unwrap()).ok_or(()).unwrap_err();
        }
        for shared_key in self_mem_keys.intersection(&rhs_mem_keys) {
            let mem_val = *self.memory.get(shared_key).unwrap() ^ *rhs.memory.get(shared_key).unwrap();
            new_memory.insert(**shared_key, mem_val).ok_or(()).unwrap_err();
        }
        CPUState {
            privilege_state: self.privilege_state ^ rhs.privilege_state,
            pc: self.pc ^ rhs.pc,
            instr: self.instr ^ rhs.instr,
            xregs: new_xregs,
            memaddr: self.memaddr ^ rhs.memaddr,
            memory: new_memory
        }
    }
}
impl CPUState {
    pub fn write_store(&mut self, addr: u64, store: StoreVal) {
        let mut byte_arr: [u8; 8] = [0x00; 8];

        match store {
            StoreVal::U8(val) => byte_arr[..1].copy_from_slice(&val.to_le_bytes()),
            StoreVal::U16(val) => byte_arr[..2].copy_from_slice(&val.to_le_bytes()),
            StoreVal::U32(val) => byte_arr[..4].copy_from_slice(&val.to_le_bytes()),
            StoreVal::U64(val) => byte_arr.copy_from_slice(&val.to_le_bytes())
        };
        for (rel_addr, byte) in byte_arr[..store.byte_len()].iter().enumerate() {
            let abs_addr = addr.wrapping_add(rel_addr.try_into().unwrap());
            if *byte == 0 {
                self.memory.remove(&abs_addr);
            } else {
                self.memory.insert(abs_addr, *byte);
            }
        }
    }
    pub fn apply(&self, delta: CPUStateDelta) -> CPUState {
        let mut return_val = self.clone();
        match delta {
            CPUStateDelta::RegisterOnly { register: reg, value: val } => {
                return_val.xregs[(reg-1) as usize] = val;
            },
            CPUStateDelta::MemoryLoad { addr, register: reg, value: val } => {
                return_val.memaddr = addr;
                return_val.xregs[(reg-1) as usize] = val;
            },
            CPUStateDelta::MemoryStore { addr, value: store_val } => {
                return_val.memaddr = addr;
                return_val.write_store(addr, store_val);
            }
        };
        return_val
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct PowerSettings {
    pub hamming_weight_multiplier: f64,
    pub hamming_delta_multiplier: f64
}
#[derive(Default, Debug, Clone, Copy)]
pub struct CPUPowerSettings {
    pub clock_frequency: f64,
    pub priv_weighting: PowerSettings,
    pub pc_weighting: PowerSettings,
    pub instr_weighting: PowerSettings,
    pub xregs_weighting: PowerSettings,
    pub memaddr_weighting: PowerSettings,
    pub memory_weighting: PowerSettings
}

/* Example lines
core   0: 0 0x0000000000010348 (0x7125) x 2 0x0000003ffffff870
core   0: 0 0x000000000001034a (0xe34a) mem 0x0000003ffffff9f0 0x0000000000000000
core   0: 0 0x000000000001034c (0x00064917) x18 0x000000000007434c
core   0: 0 0x0000000000010350 (0x4bc93903) x18 0x0000000000072b10 mem 0x0000000000074808
core   0: 0 0x0000000000010354 (0x00093783) x15 0x69625f7365610000 mem 0x0000000000072b10
core   0: 0 0x0000000000010358 (0xef06) mem 0x0000003ffffffa08 0x0000000000014288
 */
lazy_static! {
    /* Capturing groups:
     * 0: The entire thing
     * 1: Privilege level: u8
     * 2: Program Counter: u64
     * 3: Instruction bits
     * 4: Everything else
     */
    static ref TRACE_REGEX: Regex = Regex::new(concat!("^",
        "core *[[:digit:]]+: ",
        "([[:digit:]]+) ",
        "0x([[:xdigit:]]{16})",
        r"\(0x([[:xdigit:]]{4,8})\)",
        "( .+)?",
        "$")
    ).unwrap();
    /* Capturing groups:
     * 0: The entire thing
     * 1: Register number, if present
     * 2: Register value, if present
     * 3: Memory address, if present
     * 4: Value stored into memory, if present
     */
    static ref VALUE_CHANGE_REGEX: Regex = Regex::new(concat!("^",
        "(?: x ?(?:([[:digit:]]+) 0x([[:xdigit:]]{16})))?",
        "(?: mem 0x([[:xdigit:]]{16})(?: 0x([[:xdigit:]]+))?)?",
        "$")
    ).unwrap();
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StoreVal {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64)
}
impl StoreVal {
    pub fn byte_len(&self) -> usize {
        match self {
            StoreVal::U8(_) => 1,
            StoreVal::U16(_) => 2,
            StoreVal::U32(_) => 4,
            StoreVal::U64(_) => 8
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CPUStateDelta {
    RegisterOnly {register: u8, value: u64},
    // Loads are always zero-extended or sign-extended
    MemoryLoad {addr: u64, register: u8, value: u64},
    MemoryStore {addr: u64, value: StoreVal}
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CPUStateDeltaFromStrErr {
    InvalidRegister(u8),
    InvalidStoreWidth(usize),
    RegexFail(String)
}
impl fmt::Display for CPUStateDeltaFromStrErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidRegister(reg) => write!(f, "Invalid register {}", reg),
            Self::InvalidStoreWidth(w) => write!(f, "Invalid store width {}", w),
            Self::RegexFail(s) => write!(f, "Parsing failed for {}", s)
        }
    }
}
impl FromStr for CPUStateDelta {
    type Err = CPUStateDeltaFromStrErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let captures_option = VALUE_CHANGE_REGEX.captures(s);
        match captures_option {
            Some(captures) => {
                let reg = captures.get(1)
                    .map(|s| u8::from_str_radix(s.as_str(), 16).unwrap());
                let reg_value = match reg {
                    Some(_) => Some(u64::from_str_radix(captures.get(2).unwrap().as_str(), 16).unwrap()),
                    None => None
                };
                let mem_address = captures.get(3)
                    .map(|s| u64::from_str_radix(s.as_str(), 16).unwrap());
                let mem_value = match mem_address {
                    Some(_) => captures.get(4),
                    None => None
                };
                match (reg, reg_value, mem_address, mem_value) {
                    (Some(reg), Some(reg_val), None, None) => {
                        // Register only change
                        if !(1..=32).contains(&reg) {
                            Err(CPUStateDeltaFromStrErr::InvalidRegister(reg))
                        } else {
                            Ok(CPUStateDelta::RegisterOnly {
                                register: reg,
                                value: reg_val
                            })
                        }
                    },
                    (Some(reg), Some(reg_val), Some(mem_addr), None) => {
                        // Value load into register
                        if !(1..=32).contains(&reg) {
                            Err(CPUStateDeltaFromStrErr::InvalidRegister(reg))
                        } else {
                            Ok(CPUStateDelta::MemoryLoad {
                                addr: mem_addr,
                                register: reg,
                                value: reg_val
                            })
                        }
                    },
                    (None, None, Some(mem_addr), Some(mem_val)) => {
                        let val_str = mem_val.as_str();
                        // Length is in hexadecimal digits
                        let val = match val_str.len() {
                            2 => StoreVal::U8(u8::from_str_radix(val_str, 16).unwrap()),
                            4 => StoreVal::U16(u16::from_str_radix(val_str, 16).unwrap()),
                            8 => StoreVal::U32(u32::from_str_radix(val_str, 16).unwrap()),
                            16 => StoreVal::U64(u64::from_str_radix(val_str, 16).unwrap()),
                            val => return Err(CPUStateDeltaFromStrErr::InvalidStoreWidth(val))
                        };
                        Ok(CPUStateDelta::MemoryStore {
                            addr: mem_addr,
                            value: val
                        })
                    },
                    _ => Err(CPUStateDeltaFromStrErr::RegexFail(s.to_string()))
                }
            },
            None => Err(CPUStateDeltaFromStrErr::RegexFail(s.to_string()))
        }
    }
}
