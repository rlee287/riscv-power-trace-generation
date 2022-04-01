use serde::Deserialize;

#[derive(Deserialize)]
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct PowerSettings {
    pub weight_multiplier: f64,
    pub delta_multiplier: f64
}
#[derive(Deserialize)]
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct CPUPowerSettings {
    pub clock_frequency: f64,
    pub r#priv: PowerSettings,
    pub pc: PowerSettings,
    pub instr: PowerSettings,
    pub xregs: PowerSettings,
    pub memaddr: PowerSettings,
    pub memory: PowerSettings,
    pub csr: PowerSettings
}