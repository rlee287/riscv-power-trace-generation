use crate::cpu::cpu_structs::{CPUState, MemoryOperation};
use crate::cpu::memory::StoreVal;

use lazy_static::lazy_static;
use regex::Regex;

use std::str::FromStr;
use std::fmt;

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

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct ParsedCPUState {
    pub(super) privilege_state: u8,
    pub(super) pc: u64,
    pub(super) instr: u32
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

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct ParsedCPUStateDelta {
    pub(super) x_register: Option<(u8, u64)>,
    pub(super) csr_registers: [Option<(u16, u64)>; 2],
    pub(super) memory_op: Option<MemoryOperation>
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CPUStateStrParseErr {
    InvalidPrivilege(u8),
    InvalidRegister(u8),
    InvalidCsr(u16),
    InvalidStoreWidth(usize),
    UnexpectedOperation(&'static str),
    RegexFail
}
impl fmt::Display for CPUStateStrParseErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidPrivilege(r#priv) => write!(f, "Invalid privilege {} ", r#priv),
            Self::InvalidRegister(reg) => write!(f, "Invalid register {}", reg),
            Self::InvalidCsr(csr) => write!(f, "Invalid csr {}", csr),
            Self::InvalidStoreWidth(width) => write!(f, "Invalid store width {}", width),
            Self::UnexpectedOperation(str) => write!(f, "Unexpected operation {}", str),
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
            return Err(CPUStateStrParseErr::UnexpectedOperation("More than 2 CSR operations"));
        }
        if mem_captures.len()>2 {
            return Err(CPUStateStrParseErr::UnexpectedOperation("More than 2 memory operations"));
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
            if !(0..4096).contains(&csr_addr) {
                return Err(CPUStateStrParseErr::InvalidCsr(csr_addr));
            }
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
                            return Err(CPUStateStrParseErr::UnexpectedOperation("CPU attempted two stores"));
                        }
                        if load_addr.unwrap() != *addr {
                            return Err(CPUStateStrParseErr::UnexpectedOperation("CPU load and store at different addrs"));
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
                            return Err(CPUStateStrParseErr::UnexpectedOperation("CPU attempted two loads"));
                        }
                        load_addr = Some(*addr)
                    }
                    _ => {
                        return Err(CPUStateStrParseErr::RegexFail)
                    }
                }
                if load_addr.is_none() {
                    unreachable!("2 Mem stores not caught earlier");
                }
            },
            _ => {
                unreachable!("More than 2 memory operations not caught earlier");
            }
        }
        Ok(ret_delta)
    }
}

pub fn get_pc(line: &str) -> Result<u64, CPUStateStrParseErr> {
    let line_capture = TRACE_PC_REGEX.captures(line).ok_or(CPUStateStrParseErr::RegexFail)?;
    Ok(u64::from_str_radix(&line_capture[1], 16).unwrap())
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