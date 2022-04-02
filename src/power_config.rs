use serde::Deserialize;

#[derive(Deserialize)]
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
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct CPUPowerSettings {
    pub clock_frequency: f64,
    pub pc_range: Option<PCFilter>,
    pub r#priv: PowerSettings,
    pub pc: PowerSettings,
    pub instr: PowerSettings,
    pub xregs: PowerSettings,
    pub memaddr: PowerSettings,
    pub memory: PowerSettings,
    pub csr: PowerSettings
}