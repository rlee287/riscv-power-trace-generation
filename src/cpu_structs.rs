use std::collections::{HashSet, HashMap};
#[cfg(feature = "mem_track")]
use std::collections::{BTreeSet, BTreeMap};
use std::ops::BitXor;

use lazy_static::lazy_static;
use regex::Regex;
use std::str::FromStr;
use std::fmt;

use crate::power_config::CPUPowerSettings;
use crate::arithmetic_utils;
use crate::memory::StoreVal;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct ParsedCPUState {
    privilege_state: u8,
    pc: u64,
    instr: u32
}
impl ParsedCPUState {
    #[inline(always)]
    pub fn pc(&self) -> u64 {
        self.pc
    }
    pub fn apply_persistent_state(self, old_state: &CPUState) -> CPUState {
        CPUState {
            privilege_state: self.privilege_state,
            pc: self.pc,
            instr: self.instr,
            xregs: old_state.xregs,
            membus_addr: old_state.membus_addr,
            membus_read: old_state.membus_read,
            membus_write: old_state.membus_write,
            membus_strobe: old_state.membus_strobe,
            csr: old_state.csr.clone(),
            #[cfg(feature = "mem_track")]
            memory: old_state.memory.clone()
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct CPUState {
    privilege_state: u8,
    pc: u64,
    instr: u32,
    xregs: [u64; 31], // + zero-reg
    // Memory bus values
    membus_addr: u64,
    membus_read: u64,
    membus_write: u64,
    membus_strobe: u8,
    // Use BTreeMap and its arrays for locality
    #[cfg(feature = "mem_track")]
    memory: BTreeMap<u64, u8>,
    /*
     * CSRs not expected to have similar locality
     * Make CSRs a sparse array
     */
    csr: HashMap<u16, u64>
}
impl BitXor for &CPUState {
    type Output = CPUState;
    fn bitxor(self, rhs: Self) -> Self::Output {
        let mut new_xregs = self.xregs;
        for i in 0..31 {
            new_xregs[i] ^= rhs.xregs[i];
        }

        #[cfg(feature = "mem_track")]
        let mut new_memory: BTreeMap<u64, u8>;
        #[cfg(feature = "mem_track")]
        {
            // Copy over distinct values for each, and then XOR shared ones
            let self_mem_keys: BTreeSet<_> = self.memory.keys().collect();
            let rhs_mem_keys: BTreeSet<_> = rhs.memory.keys().collect();
            new_memory = BTreeMap::new();
            for self_key in self_mem_keys.difference(&rhs_mem_keys) {
                new_memory.insert(**self_key, *self.memory.get(self_key).unwrap()).ok_or(()).unwrap_err();
            }
            for rhs_key in rhs_mem_keys.difference(&self_mem_keys) {
                new_memory.insert(**rhs_key, *rhs.memory.get(rhs_key).unwrap()).ok_or(()).unwrap_err();
            }
            for shared_key in self_mem_keys.intersection(&rhs_mem_keys) {
                let mem_val = *self.memory.get(shared_key).unwrap() ^ *rhs.memory.get(shared_key).unwrap();
                if mem_val != 0 {
                    new_memory.insert(**shared_key, mem_val).ok_or(()).unwrap_err();
                }
            }
        }

        let self_csr_keys: HashSet<_> = self.csr.keys().collect();
        let rhs_csr_keys: HashSet<_> = rhs.csr.keys().collect();
        let mut new_csr = HashMap::new();
        for self_key in self_csr_keys.difference(&rhs_csr_keys) {
            new_csr.insert(**self_key, *self.csr.get(self_key).unwrap()).ok_or(()).unwrap_err();
        }
        for rhs_key in rhs_csr_keys.difference(&self_csr_keys) {
            new_csr.insert(**rhs_key, *rhs.csr.get(rhs_key).unwrap()).ok_or(()).unwrap_err();
        }
        for shared_key in self_csr_keys.intersection(&rhs_csr_keys) {
            let mem_val = *self.csr.get(shared_key).unwrap() ^ *rhs.csr.get(shared_key).unwrap();
            if mem_val != 0 {
                new_csr.insert(**shared_key, mem_val).ok_or(()).unwrap_err();
            }
        }

        CPUState {
            privilege_state: self.privilege_state ^ rhs.privilege_state,
            pc: self.pc ^ rhs.pc,
            instr: self.instr ^ rhs.instr,
            xregs: new_xregs,
            membus_addr: self.membus_addr ^ rhs.membus_addr,
            membus_read: self.membus_read ^ rhs.membus_read,
            membus_write: self.membus_write ^ rhs.membus_write,
            membus_strobe: self.membus_strobe ^ rhs.membus_strobe,
            #[cfg(feature = "mem_track")]
            memory: new_memory,
            csr: new_csr
        }
    }
}
impl CPUState {
    #[inline(always)]
    pub fn pc(&self) -> u64 {
        self.pc
    }
    #[cfg(feature = "mem_track")]
    fn write_store(&mut self, addr: u64, store: StoreVal) {
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
    pub fn apply(&mut self, delta: ParsedCPUStateDelta) {
        if let Some((reg, val)) = delta.x_register {
            self.xregs[(reg-1) as usize] = val;
        }
        if let Some((reg, val)) = delta.csr_registers[0] {
            if val==0 {
                self.csr.remove(&reg);
            } else {
                self.csr.insert(reg, val);
            }
        }
        if let Some((reg, val)) = delta.csr_registers[1] {
            if val==0 {
                self.csr.remove(&reg);
            } else {
                self.csr.insert(reg, val);
            }
        }
        match delta.memory_op {
            Some(MemoryOperation::MemoryLoad { addr }) => {
                self.membus_addr = addr;
                if let Some((_reg, val)) = delta.x_register {
                    self.membus_read = val;
                }
            }
            Some(MemoryOperation::MemoryStore { addr, value }) => {
                self.membus_addr = addr;
                #[cfg(feature = "mem_track")]
                self.write_store(addr, value);

                self.membus_write = value.into();
                self.membus_strobe = match value {
                    StoreVal::U8(_)  => 0x01 << (addr % 8),
                    StoreVal::U16(_) => 0x03 << (addr % 8),
                    StoreVal::U32(_) => 0x0f << (addr % 8),
                    StoreVal::U64(_) => 0xff
                }
            },
            Some(MemoryOperation::MemoryLoadStore { addr , value}) => {
                self.membus_addr = addr;
                #[cfg(feature = "mem_track")]
                self.write_store(addr, value);

                self.membus_read = value.into();
                self.membus_write = value.into();
                self.membus_strobe = match value {
                    StoreVal::U8(_)  => 0x01 << (addr % 8),
                    StoreVal::U16(_) => 0x03 << (addr % 8),
                    StoreVal::U32(_) => 0x0f << (addr % 8),
                    StoreVal::U64(_) => 0xff
                }
            },
            None => {}
        }
    }
    pub fn compute_power(&self, other: Option<&Self>, power: &CPUPowerSettings) -> f64 {
        let priv_power = power.r#priv.weight_multiplier *
            f64::from(self.privilege_state.count_ones());
        let pc_power = power.pc.weight_multiplier *
            f64::from(self.pc.count_ones());
        let instr_power = power.instr.weight_multiplier *
            f64::from(self.instr.count_ones());
        let xregs_power = self.xregs.map(|regval| {
            power.xregs.weight_multiplier * f64::from(regval.count_ones())
        });
        let membus_addr_power = power.membus.weight_multiplier *
            f64::from(self.membus_addr.count_ones());
        let membus_read_power = power.membus.weight_multiplier *
            f64::from(self.membus_read.count_ones());
        let membus_write_power = power.membus.weight_multiplier *
            f64::from(self.membus_write.count_ones());
        let membus_strobe_power = power.membus.weight_multiplier *
            f64::from(self.membus_strobe.count_ones());
        #[cfg(feature = "mem_track")]
        let memory_power = self.memory.values().map(|byte| {
            power.memory.weight_multiplier * f64::from(byte.count_ones())
        });
        #[cfg(not(feature = "mem_track"))]
        let memory_power = [0.0];
        let csr_power = self.csr.values().map(|csr_reg| {
            power.csr.weight_multiplier * f64::from(csr_reg.count_ones())
        });
        let weight_iter = [priv_power].into_iter()
            .chain([pc_power, instr_power])
            .chain([membus_addr_power, membus_read_power])
            .chain([membus_write_power, membus_strobe_power])
            .chain(xregs_power)
            .chain(memory_power)
            .chain(csr_power);

        match other {
            Some(other_state) => {
                let delta_state = self ^ other_state;

                let priv_power = power.r#priv.delta_multiplier *
                    f64::from(delta_state.privilege_state.count_ones());
                let pc_power = power.pc.delta_multiplier *
                    f64::from(delta_state.pc.count_ones());
                let instr_power = power.instr.delta_multiplier *
                    f64::from(delta_state.instr.count_ones());
                let xregs_power = delta_state.xregs.map(|regval| {
                    power.xregs.delta_multiplier * f64::from(regval.count_ones())
                });
                let membus_addr_power = power.membus.delta_multiplier *
                    f64::from(delta_state.membus_addr.count_ones());
                let membus_read_power = power.membus.weight_multiplier *
                    f64::from(delta_state.membus_read.count_ones());
                let membus_write_power = power.membus.weight_multiplier *
                    f64::from(delta_state.membus_write.count_ones());
                let membus_strobe_power = power.membus.weight_multiplier *
                    f64::from(delta_state.membus_strobe.count_ones());
                #[cfg(feature = "mem_track")]
                let memory_power = delta_state.memory.values().map(|byte| {
                    power.memory.delta_multiplier * f64::from(byte.count_ones())
                });
                #[cfg(not(feature = "mem_track"))]
                let memory_power = [0.0];
                let csr_power = delta_state.csr.values().map(|csr_reg| {
                    power.csr.delta_multiplier * f64::from(csr_reg.count_ones())
                });
                let delta_iter = [priv_power].into_iter()
                    .chain([pc_power, instr_power])
                    .chain([membus_addr_power, membus_read_power])
                    .chain([membus_write_power, membus_strobe_power])
                    .chain(xregs_power)
                    .chain(memory_power)
                    .chain(csr_power);
                arithmetic_utils::sum(weight_iter.chain(delta_iter))
            },
            None => arithmetic_utils::sum(weight_iter)
        }
    }
}

/* Example lines
core   0: 0 0x0000000000010348 (0x7125) x 2 0x0000003ffffff870
core   0: 0 0x000000000001034a (0xe34a) mem 0x0000003ffffff9f0 0x0000000000000000
core   0: 0 0x000000000001034c (0x00064917) x18 0x000000000007434c
core   0: 0 0x0000000000010350 (0x4bc93903) x18 0x0000000000072b10 mem 0x0000000000074808
core   0: 0 0x0000000000010354 (0x00093783) x15 0x69625f7365610000 mem 0x0000000000072b10
core   0: 0 0x0000000000010358 (0xef06) mem 0x0000003ffffffa08 0x0000000000014288
 */
/* Line format:
 * `core <number>: <priv> 0x<pc> (0x<instr>) <commit>* <mem>`
 * Where <commit> is one of
 * ` x ?<xreg_num> <value>`
 * ` c<num>_<csr_name> <addr> <value>?
 * And <mem> is
 * ` mem <addr> <value>?`
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
        "([[:digit:]]) ",
        "0x([[:xdigit:]]{16}) ",
        r"\(0x([[:xdigit:]]{4,8})\)",
        "( .+)?",
        "$")
    ).unwrap();
    /* Capturing groups:
     * 0: The entire thing
     * 1: Program Counter: u64
     * 2: Everything else
     */
    static ref TRACE_PC_REGEX: Regex = Regex::new(concat!("^",
        "core *[[:digit:]]+: ",
        "[[:digit:]] ",
        "0x([[:xdigit:]]{16})",
        ".+",
        "$")
    ).unwrap();
    /* Capturing groups:
     * 0: The entire thing
     * 1: Register number, if present
     * 2: Register value, if present
     * 3: Memory address, if present
     * 4: Value stored into memory, if present
     */
    /*static ref VALUE_CHANGE_REGEX: Regex = Regex::new(concat!("^",
        "(?: x ?(?:([[:digit:]]+) 0x([[:xdigit:]]{16})))?",
        "(?: mem 0x([[:xdigit:]]{16})(?: 0x([[:xdigit:]]+))?)?",
        "$")
    ).unwrap();*/
    /* Capturing groups:
     * 0: The entire thing
     * 1: Register number
     * 2: Register value
     */
    static ref XREG_CHANGE: Regex = Regex::new(
        " x ?([[:digit:]]+) 0x([[:xdigit:]]{16})"
    ).unwrap();
    /* Capturing groups:
     * 0: The entire thing
     * 1: CSR register number (we discard the name)
     * 2: CSR register value
     */
    static ref CSR_CHANGE: Regex = Regex::new(
        " c([[:digit:]]+)_[^ ]+ 0x([[:xdigit:]]{16})"
    ).unwrap();
    /* Capturing groups:
     * 0: The entire thing
     * 1: Memory address
     * 2: Memory value, if present
     */
    static ref MEM_CHANGE: Regex = Regex::new(
        " mem 0x([[:xdigit:]]{16})(?: 0x((?:[[:xdigit:]])+))?"
    ).unwrap();
}

// Keep even without mem_track for tracking the bus
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MemoryOperation {
    // Loads are always zero-extended or sign-extended
    MemoryLoad {addr: u64},
    // Stores have a size to account for
    MemoryStore {
        addr: u64,
        value: StoreVal
    },
    // XXX: for atomic instructions, load and store value may differ, but we don't track this
    MemoryLoadStore {
        addr: u64,
        value: StoreVal
    }
}
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct ParsedCPUStateDelta {
    x_register: Option<(u8, u64)>,
    csr_registers: [Option<(u16, u64)>; 2],
    memory_op: Option<MemoryOperation>
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CPUStateStrParseErr {
    InvalidPrivilege(u8),
    InvalidRegister(u8),
    InvalidStoreWidth(usize),
    RegexFail
}
impl fmt::Display for CPUStateStrParseErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidPrivilege(r#priv) => write!(f, "Invalid privilege {} ", r#priv),
            Self::InvalidRegister(reg) => write!(f, "Invalid register {}", reg),
            Self::InvalidStoreWidth(width) => write!(f, "Invalid store width {}", width),
            Self::RegexFail => f.write_str("Regex did not match")
        }
    }
}
impl FromStr for ParsedCPUStateDelta {
    type Err = CPUStateStrParseErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut ret_delta = ParsedCPUStateDelta::default();
        let xreg_captures_opt = XREG_CHANGE.captures(s);
        let csr_captures: Vec<_> = CSR_CHANGE.captures_iter(s).collect();
        let mem_captures: Vec<_> = MEM_CHANGE.captures_iter(s).collect();

        if csr_captures.len()>2 {
            // TODO: better error, if this actually gets hit
            return Err(CPUStateStrParseErr::RegexFail);
        }
        if mem_captures.len()>2 {
            return Err(CPUStateStrParseErr::RegexFail);
        }
        /*
         * Possible state transitions
         * - Only one x register can be written to
         * - Up to two CSR registers can be written to
         * - Up to two memory operations occurs per instruction
         * (Even though RISC-V is load-store atomic fetch-and-[action] instructions can affect more than one location)
         */
        if let Some(xreg_captures) = xreg_captures_opt {
            let reg = u8::from_str(&xreg_captures[1]).map_err(|_| CPUStateStrParseErr::RegexFail)?;
            if !(1..32).contains(&reg) {
                return Err(CPUStateStrParseErr::InvalidRegister(reg));
            }
            let reg_val = u64::from_str_radix(&xreg_captures[2], 16).map_err(|_| CPUStateStrParseErr::RegexFail)?;
            ret_delta.x_register = Some((reg, reg_val));
        }
        for (i,csr_capture) in csr_captures.into_iter().enumerate() {
            let csr_addr = u16::from_str(&csr_capture[1]).map_err(|_| CPUStateStrParseErr::RegexFail)?;
            let csr_val = u64::from_str_radix(&csr_capture[2], 16).map_err(|_| CPUStateStrParseErr::RegexFail)?;
            ret_delta.csr_registers[i] = Some((csr_addr, csr_val));
        }
        let temp_capture_iter: Vec<_> = mem_captures.into_iter().map(|mem_capture| {
            let mem_addr = u64::from_str_radix(&mem_capture[1], 16).map_err(|_| CPUStateStrParseErr::RegexFail)?;
            match mem_capture.get(2) {
                Some(val_match) => {
                    Result::<MemoryOperation, CPUStateStrParseErr>::Ok(MemoryOperation::MemoryStore {
                        addr: mem_addr,
                        value: StoreVal::from_str(val_match.as_str())?
                    })
                },
                None => {
                    Ok(MemoryOperation::MemoryLoad {
                        addr: mem_addr
                    })
                }
            }
        }).collect();
        match &temp_capture_iter[..] {
            [] => {}, // Do nothing
            [mem_op_result] => {
                match mem_op_result {
                    Ok (mem_op) => {
                        ret_delta.memory_op = Some(*mem_op);
                    },
                    Err(e) => {
                        return Err(*e)
                    }
                }
            },
            [mem_op_result_1, mem_op_result_2] => {
                if mem_op_result_1.is_err() || mem_op_result_2.is_err() {
                    return Err(CPUStateStrParseErr::RegexFail)
                }
                let mut load_addr: Option<u64> = None;
                match mem_op_result_1 {
                    Ok(MemoryOperation::MemoryStore { addr, value }) => {
                        ret_delta.memory_op = Some(
                            MemoryOperation::MemoryLoadStore{
                                addr: *addr, value: *value
                            }
                        );
                    },
                    Ok(MemoryOperation::MemoryLoad { addr }) => {
                        load_addr = Some(*addr)
                    }
                    _ => {
                        return Err(CPUStateStrParseErr::RegexFail)
                    }
                }
                match mem_op_result_2 {
                    Ok(MemoryOperation::MemoryStore { addr, value }) => {
                        if ret_delta.memory_op.is_some() {
                            // Illegal state of 2 stores
                            return Err(CPUStateStrParseErr::RegexFail);
                        }
                        ret_delta.memory_op = Some(
                            MemoryOperation::MemoryLoadStore{
                                addr: *addr, value: *value
                            }
                        );
                    },
                    Ok(MemoryOperation::MemoryLoad { addr }) => {
                        if load_addr.is_some() {
                            // Illegal state of 2 loads
                            return Err(CPUStateStrParseErr::RegexFail);
                        }
                        load_addr = Some(*addr)
                    }
                    _ => {
                        return Err(CPUStateStrParseErr::RegexFail)
                    }
                }
                if load_addr.is_none() {
                    return Err(CPUStateStrParseErr::RegexFail);
                }
            },
            _ => {
                return Err(CPUStateStrParseErr::RegexFail);
            }
        }
        Ok(ret_delta)
    }
}

pub fn get_pc(line: &str) -> Result<u64, CPUStateStrParseErr> {
    let line_capture = TRACE_PC_REGEX.captures(line).ok_or(CPUStateStrParseErr::RegexFail)?;
    Ok(u64::from_str_radix(&line_capture[1], 16).unwrap())
}

pub fn parse_commit_line(line: &str) -> Result<(ParsedCPUState, ParsedCPUStateDelta), CPUStateStrParseErr> {
    let line_capture = TRACE_REGEX.captures(line).ok_or(CPUStateStrParseErr::RegexFail)?;
    let priv_level = u8::from_str(&line_capture[1]).unwrap();
    if ![0,1,3].contains(&priv_level) {
        return Err(CPUStateStrParseErr::InvalidPrivilege(priv_level));
    }
    let pc = u64::from_str_radix(&line_capture[2], 16).unwrap();
    // Approximation: we'd expect a real CPU to decode compressed instructions
    let instr = u32::from_str_radix(&line_capture[3], 16).unwrap();

    let cpu_state = ParsedCPUState {
        privilege_state: priv_level, 
        instr,
        pc
    };
    let delta = match line_capture.get(4) {
        Some(m) => ParsedCPUStateDelta::from_str(m.as_str())?,
        None => ParsedCPUStateDelta::default()
    };
    Ok((cpu_state, delta))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn mem_regex() {
        let input_str_1 = " mem 0x0000003ffffffa08 0x0000000000014288";
        let captures_1 = MEM_CHANGE.captures(input_str_1);
        assert!(captures_1.is_some());
        let captures_1 = captures_1.unwrap();
        assert_eq!(&captures_1[1], "0000003ffffffa08");
        assert_eq!(&captures_1[2], "0000000000014288");
        let input_str_2 = " mem 0x000000008000fe60 0x0";
        let captures_2 = MEM_CHANGE.captures(input_str_2);
        assert!(captures_2.is_some());
        let captures_2 = captures_2.unwrap();
        assert_eq!(&captures_2[1], "000000008000fe60");
        assert_eq!(&captures_2[2], "0");
    }
}