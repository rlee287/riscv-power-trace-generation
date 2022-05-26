use std::collections::HashMap;
#[cfg(feature = "mem_track")]
use std::collections::{BTreeSet, BTreeMap};

use crate::power_config::CPUPowerSettings;
use crate::arithmetic_utils;
use crate::cpu::memory::StoreVal;
#[cfg(feature = "mem_track")]
use crate::cpu::memory::MemoryState;

use crate::ParsedCPUStateDelta;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct CPUState {
    pub(super) privilege_state: u8,
    pub(super) pc: u64,
    pub(super) instr: u32,
    pub(super) xregs: [u64; 31], // + zero-reg
    // Memory bus values
    pub(super) membus_addr: u64,
    pub(super) membus_read: u64,
    pub(super) membus_write: u64,
    pub(super) membus_strobe: u8,
    // Use BTreeMap and its arrays for locality
    #[cfg(feature = "mem_track")]
    pub(super) memory: MemoryState,
    /*
     * CSRs not expected to have similar locality
     * Make CSRs a sparse array
     */
    pub(super) csr: HashMap<u16, u64>
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
        // Don't need to GC for space as there are at most 4096
        if let Some((reg, val)) = delta.csr_registers[0] {
            if val!=0 {
                self.csr.insert(reg, val);
            }
        }
        if let Some((reg, val)) = delta.csr_registers[1] {
            if val!=0 {
                self.csr.insert(reg, val);
            }
        }
        match delta.memory_op {
            Some(MemoryOperation::MemoryLoad { addr }) => {
                self.membus_addr = addr;
                if let Some((_reg, val)) = delta.x_register {
                    self.membus_read = val;
                }
                self.membus_strobe = 0x00;
            }
            Some(MemoryOperation::MemoryStore { addr, value }) => {
                self.membus_addr = addr;
                #[cfg(feature = "mem_track")]
                self.write_store(addr, value);

                self.membus_write = value.into();
                self.membus_strobe = StoreVal::get_strobe(&value, addr);
            },
            Some(MemoryOperation::MemoryLoadStore { addr , value}) => {
                self.membus_addr = addr;
                #[cfg(feature = "mem_track")]
                self.write_store(addr, value);

                self.membus_read = value.into();
                self.membus_write = value.into();
                self.membus_strobe = StoreVal::get_strobe(&value, addr);
            },
            None => {}
        };
    }
    pub fn compute_power(&self, power: &CPUPowerSettings) -> f64 {
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

        arithmetic_utils::sum(weight_iter)
    }
}

// Keep even without mem_track for tracking the bus
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoryOperation {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CPUStateDelta {
    // TODO: find a way to tie the delta to the original CPUState?
    parsed_state_set: bool,
    privilege_state_xor: u8,
    pc_xor: u64,
    instr_xor: u32,

    parsed_delta_set: bool,
    xreg_xor: u64,
    membus_addr_xor: u64,
    membus_read_xor: u64,
    membus_write_xor: u64,
    membus_strobe_xor: u8,
    csr_xor: [u64; 2]
    // When mem_track on, infer memory xor from memory bus and old mem
}
impl CPUStateDelta {
    pub fn new() -> CPUStateDelta {
        CPUStateDelta {
            parsed_state_set: false,
            privilege_state_xor: 0,
            pc_xor: 0,
            instr_xor: 0,

            parsed_delta_set: false,
            xreg_xor: 0,
            membus_addr_xor: 0,
            membus_read_xor: 0,
            membus_write_xor: 0,
            membus_strobe_xor: 0,
            csr_xor: [0; 2]
        }
    }
    pub fn set_old_state_xor(&mut self, old_state: &CPUState, new_state: &CPUState) {
        assert!(!self.parsed_state_set, "Attempted to set old state XOR twice");
        self.privilege_state_xor = old_state.privilege_state ^ new_state.privilege_state;
        self.pc_xor = old_state.pc ^ new_state.pc;
        self.instr_xor = old_state.instr ^ new_state.instr;
        self.parsed_state_set = true;
    }
    pub fn set_delta_xor(&mut self, old_state: &CPUState, state_delta: &ParsedCPUStateDelta) {
        assert!(!self.parsed_delta_set, "Attempted to set delta XOR twice");
        if let Some((reg_id, new_val)) = state_delta.x_register {
            // Index check will catch reg_id==0
            self.xreg_xor = old_state.xregs[usize::from(reg_id-1)] ^ new_val;
        }
        for (i, csr_change) in state_delta.csr_registers.iter().enumerate() {
            if let Some((csr_id, new_val)) = csr_change {
                self.csr_xor[i] = old_state.csr.get(csr_id).unwrap_or(&0) ^ new_val;
            }
        }
        // TODO: Modified from CPUState::apply, find way to reduce duplication
        match state_delta.memory_op {
            Some(MemoryOperation::MemoryLoad { addr }) => {
                self.membus_addr_xor = old_state.membus_addr ^ addr;
                if let Some((_reg, val)) = state_delta.x_register {
                    self.membus_read_xor = old_state.membus_read ^ val;
                }
                self.membus_strobe_xor = old_state.membus_strobe ^ 0x00;
            }
            Some(MemoryOperation::MemoryStore { addr, value }) => {
                self.membus_addr_xor = old_state.membus_addr ^ addr;
                // TODO: replace
                #[cfg(feature = "mem_track")]
                self.write_store(addr, value);

                self.membus_write_xor = old_state.membus_write ^ u64::from(value);
                self.membus_strobe_xor = old_state.membus_strobe ^ StoreVal::get_strobe(&value, addr);
            },
            Some(MemoryOperation::MemoryLoadStore { addr , value}) => {
                self.membus_addr_xor = old_state.membus_addr ^ addr;
                // TODO: replace
                #[cfg(feature = "mem_track")]
                self.write_store(addr, value);

                self.membus_read_xor = old_state.membus_read ^ u64::from(value);
                self.membus_write_xor = old_state.membus_write ^ u64::from(value);
                self.membus_strobe_xor = old_state.membus_strobe ^ StoreVal::get_strobe(&value, addr);
            },
            None => {}
        }

        self.parsed_delta_set = true;
    }

    pub fn compute_power(&self, power: &CPUPowerSettings) -> f64 {
        assert!(self.parsed_state_set && self.parsed_delta_set, "Full state needs to be set before power computation");

        let priv_power = power.r#priv.delta_multiplier *
            f64::from(self.privilege_state_xor.count_ones());
        let pc_power = power.pc.delta_multiplier *
            f64::from(self.pc_xor.count_ones());
        let instr_power = power.instr.delta_multiplier *
            f64::from(self.instr_xor.count_ones());
        let xreg_power = power.xregs.delta_multiplier * f64::from(self.xreg_xor.count_ones());
        let membus_addr_power = power.membus.delta_multiplier *
            f64::from(self.membus_addr_xor.count_ones());
        let membus_read_power = power.membus.weight_multiplier *
            f64::from(self.membus_read_xor.count_ones());
        let membus_write_power = power.membus.weight_multiplier *
            f64::from(self.membus_write_xor.count_ones());
        let membus_strobe_power = power.membus.weight_multiplier *
            f64::from(self.membus_strobe_xor.count_ones());
        // TODO: replace
        #[cfg(feature = "mem_track")]
        let memory_power = delta_state.memory.values().map(|byte| {
            power.memory.delta_multiplier * f64::from(byte.count_ones())
        });
        #[cfg(not(feature = "mem_track"))]
        let memory_power = [0.0];
        let csr_power = self.csr_xor.map(|csr_reg| {
            power.csr.delta_multiplier * f64::from(csr_reg.count_ones())
        });
        let delta_iter = [priv_power].into_iter()
            .chain([pc_power, instr_power])
            .chain([membus_addr_power, membus_read_power])
            .chain([membus_write_power, membus_strobe_power])
            .chain([xreg_power])
            .chain(memory_power)
            .chain(csr_power);
        arithmetic_utils::sum(delta_iter)
    }
}
