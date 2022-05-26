use std::collections::{HashSet, HashMap};
#[cfg(feature = "mem_track")]
use std::collections::{BTreeSet, BTreeMap};
use std::ops::BitXor;

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

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct CPUStateDelta {
    pub(super) x_reg_xor: u64,
    pub(super) csr_reg_xor: [u64; 2],
    pub(super) memory_op: Option<MemoryOperation>
}
