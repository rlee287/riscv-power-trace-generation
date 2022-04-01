
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