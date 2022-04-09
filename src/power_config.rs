use serde::{Serialize, Deserialize};

use std::collections::BTreeMap;

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
pub struct PCFilter {
    pub start: u64,
    pub stop: u64
}
#[derive(Deserialize)]
#[derive(Default, Debug, Clone, PartialEq)]
pub struct Config {
    pub pc_range: Option<PCFilter>,
    // We rely on the key sorted property for consistent labeling
    pub pc_labels: Option<BTreeMap<String, Vec<(u64, u64)>>>,
    #[serde(flatten)]
    pub power_settings: CPUPowerSettings
}
#[derive(Serialize, Deserialize)]
#[derive(Default, Debug, Clone, PartialEq)]
pub struct CPUPowerSettings {
    pub r#priv: PowerSettings,
    pub pc: PowerSettings,
    pub instr: PowerSettings,
    pub xregs: PowerSettings,
    pub membus: PowerSettings,
    pub memory: PowerSettings,
    pub csr: PowerSettings
}