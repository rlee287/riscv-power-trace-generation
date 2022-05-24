use serde::{Serialize, Deserialize};

use std::collections::BTreeMap;
use range_union_find::IntRangeUnionFind;

#[derive(Serialize, Deserialize)]
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct PowerSettings {
    pub weight_multiplier: f64,
    pub delta_multiplier: f64
}

// If present, only track state between this range
// This will result in startup inaccuracies, but this should be fine
#[derive(Deserialize)]
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct PCRange {
    pub start: u64,
    pub stop: u64
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PCFilter {
    NonSticky(IntRangeUnionFind<u64>),
    Sticky(u64, u64)
}
#[derive(Deserialize)]
#[derive(Default, Debug, Clone, PartialEq)]
pub struct Config {
    pub pc_range: Option<PCRange>,
    // We rely on the key sorted property for consistent labeling
    pub pc_labels: Option<BTreeMap<String, Vec<(u64, u64)>>>,
    pub pc_labels_sticky: Option<BTreeMap<String, (u64, u64)>>,
    #[serde(flatten)]
    pub power_settings: CPUPowerSettings
}
#[derive(Serialize, Deserialize)]
#[derive(Default, Debug, Clone, PartialEq)]
pub struct CPUPowerSettings {
    #[serde(default)]
    pub jitter_amount: f64,
    pub r#priv: PowerSettings,
    pub pc: PowerSettings,
    pub instr: PowerSettings,
    pub xregs: PowerSettings,
    pub membus: PowerSettings,
    pub memory: PowerSettings,
    pub csr: PowerSettings
}