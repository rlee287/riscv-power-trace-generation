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
    pub fn expand_to_u64(&self) -> u64 {
        match self {
            StoreVal::U8(val) => {
                let cast_u64: u64 = (*val).into();
                cast_u64 | (cast_u64 << 8) | (cast_u64 << 16)
                    | (cast_u64 << 24) | (cast_u64 << 32)
                    | (cast_u64 << 40) | (cast_u64 << 48)
                    | (cast_u64 << 56)
            },
            StoreVal::U16(val) => {
                let cast_u64: u64 = (*val).into();
                cast_u64 | (cast_u64 << 16)
                    | (cast_u64 << 32) | (cast_u64 << 48)
            },
            StoreVal::U32(val) => {
                let cast_u64: u64 = (*val).into();
                cast_u64 | (cast_u64 << 32)
            },
            StoreVal::U64(val) => *val
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

#[cfg(feature = "mem_track")]
const PAGE_SIZE: usize = 2_usize.pow(12);
#[cfg(feature = "mem_track")]
#[derive(Debug, Default)]
pub struct MemoryState {
    mem_state: BTreeMap<u64, [u8; PAGE_SIZE]>
}
impl Clone for MemoryState {
    fn clone(&self) -> Self {
        let gc_mem_state: BTreeMap<_, _> = self.mem_state.iter()
            .filter_map(|(k,v)| {
                debug_assert!(*k % (PAGE_SIZE as u64) == 0);
                for byte in *v {
                    if byte != 0 {
                        return Some((*k, *v));
                    }
                }
                return None;
            })
            .collect();
        Self { mem_state: gc_mem_state }
    }
}
#[cfg(feature = "mem_track")]
impl MemoryState {
    pub fn new() -> MemoryState {
        Self::default()
    }
    pub fn hamming_weight(&self) -> u64 {
        let mem_bytes = self.mem_state.values();
        let hamming_weight: u64 = mem_bytes.flatten().map(|x| u64::from(x.count_ones())).sum();
        hamming_weight
    }
    pub fn mem_contents(&self) -> &BTreeMap<u64, [u8; PAGE_SIZE]> {
        &self.mem_state
    }
    fn addr_to_page_addrs(address: u64) -> (u64, usize) {
        let page_offset = address % (PAGE_SIZE as u64);
        let page_addr = address - page_offset;
        assert_eq!(page_addr % (PAGE_SIZE as u64), 0);
        (page_addr, usize::try_from(page_offset).unwrap())
    }
    pub fn read_u64(&self, address: u64) -> u64 {
        let (page_addr, page_offset) = MemoryState::addr_to_page_addrs(address);
        let upper_addr = address + 7;
        let (upper_page_addr, _upper_page_offset) = MemoryState::addr_to_page_addrs(upper_addr);
        if upper_page_addr == page_addr {
            // Fast path: read within a page
            let page = self.mem_state.get(&page_addr);
            match page {
                Some(arr) => u64::from_le_bytes(arr[page_offset..page_offset+8].try_into().unwrap()),
                None => u64::from_le_bytes([0x00;8])
            }
        } else {
            //let mut load_arr: [u8; 8] = [0x00; 8];
            todo!("Reads across pages currently unsupported");
            //u64::from_le_bytes(load_arr)
        }
    }
    pub fn write_store(&mut self, address: u64, store: &StoreVal) {
        let (page_addr, page_offset) = MemoryState::addr_to_page_addrs(address);

        let upper_addr = address + (store.byte_len() as u64) - 1;
        let (upper_page_addr, _) = MemoryState::addr_to_page_addrs(upper_addr);
        if upper_page_addr > page_addr {
            panic!("Unaligned write at 0x{:x} of len {} crossed a page boundary", address, store.byte_len());
        }
        let mut byte_arr: [u8; 8] = [0x00; 8];

        match store {
            StoreVal::U8(val) => byte_arr[..1].copy_from_slice(&val.to_le_bytes()),
            StoreVal::U16(val) => byte_arr[..2].copy_from_slice(&val.to_le_bytes()),
            StoreVal::U32(val) => byte_arr[..4].copy_from_slice(&val.to_le_bytes()),
            StoreVal::U64(val) => byte_arr.copy_from_slice(&val.to_le_bytes())
        };
        let page_entry = self.mem_state.entry(page_addr);
        let page = page_entry.or_insert([0x00; PAGE_SIZE]);
        for (rel_addr, byte) in byte_arr[..store.byte_len()].iter().enumerate() {
            let byte_addr = page_offset.wrapping_add(rel_addr.try_into().unwrap());
            page[byte_addr as usize] = *byte;
        }
    }
    pub fn gc_page_map(&mut self) -> usize {
        let old_state_count = self.mem_state.len();
        self.mem_state.retain(|_k, v| {
            debug_assert!(*_k % (PAGE_SIZE as u64) == 0);
            for byte in v {
                if *byte != 0 {
                    return true;
                }
            }
            return false;
        });
        let new_state_count = self.mem_state.len();
        old_state_count - new_state_count
    }
}
