use std::fs::File;
use std::io::{Read, BufRead, BufReader};

use clap::{Arg, Command};

mod cpu;
mod power_config;
mod arithmetic_utils;
mod hdf5_helper;
//mod memory;

use cpu::parsing::{ParsedCPUState, ParsedCPUStateDelta};
use cpu::parsing::{parse_commit_line, get_pc};

use cpu::cpu_structs::CPUState;

use power_config::Config;
use power_config::PCFilter;

use range_union_find::IntRangeUnionFind;
use std::ops::Bound;
use std::collections::BTreeSet;

use crossbeam_utils::thread::scope;
use std::sync::Arc;
use std::sync::atomic::{Ordering, AtomicI32};

use indicatif::{ProgressBar, ProgressStyle, MultiProgress};

type LabelBitFlag = u32;
const MAX_LABELS: usize = LabelBitFlag::BITS as usize;

const HDF5_CHUNK_SIZE: usize = 256;
const CHANNEL_SIZE: usize = 1024;
const COMPRESSION_AMOUNT: u8 = 9;

fn main() {
    static_assertions::const_assert!(HDF5_CHUNK_SIZE < CHANNEL_SIZE);
    static_assertions::const_assert!(COMPRESSION_AMOUNT <= 9);
    let exit_code = run();
    std::process::exit(exit_code);
}
fn run() -> i32 {
    let cmd_parser = Command::new("RISCV power trace generator")
        .arg(Arg::new("config_file").takes_value(true).required(true)
            .long("config-file").short('c'))
        .arg(Arg::new("output_file").takes_value(true).required(true)
            .long("output-file").short('o'))
        .arg(Arg::new("log_files").takes_value(true).required(true)
            .last(true).multiple_values(true));
    let cmd_args = cmd_parser.get_matches();

    let config_file_name = cmd_args.value_of("config_file").unwrap();
    let mut config_file = match File::open(config_file_name) {
        Ok(fil) => fil,
        Err(e) => {
            eprintln!("Error opening config {}: {}", config_file_name, e);
            return 1;
        }
    };
    let mut config_file_contents = Vec::new();
    match config_file.read_to_end(&mut config_file_contents) {
        Ok(_) => {},
        Err(e) => {
            eprintln!("Error reading config file: {}", e);
            return 1;
        }
    }
    let config: Config = match toml::from_slice(&config_file_contents) {
        Ok(config) => config,
        Err(e) => {
            eprintln!("Error parsing config file: {}", e);
            return 1;
        }
    };
    drop(config_file_contents);
    drop(config_file);

    //println!("{:?}", config);

    let pc_range = config.pc_range;
    let label_indexes = {
        let non_sticky = match &config.pc_labels {
            Some(labels) => {
                labels.keys().cloned().collect::<BTreeSet<_>>()
            },
            None => BTreeSet::new()
        };
        let sticky = match &config.pc_labels_sticky {
            Some(labels_sticky) => {
                labels_sticky.keys().cloned().collect::<BTreeSet<_>>()
            },
            None => BTreeSet::new()
        };
        if non_sticky.intersection(&sticky).next().is_some() {
            eprintln!("Error parsing config files: non-sticky and sticky labels overlap");
            return 1;
        }
        let labels = non_sticky.union(&sticky).cloned().collect::<Vec<_>>();
        if labels.len() >= MAX_LABELS {
            eprintln!("Error parsing config files: too many classes");
            return 1;
        }
        labels
    };
    let label_index_str_option = match label_indexes.len() {
        0 => None,
        _ => Some(serde_json::to_string(&label_indexes).unwrap())
    };
    let pc_label_lookups = label_indexes.iter().map(|s| {
        if let Some(dict) = &config.pc_labels {
            if let Some(vec) = dict.get(s) {
                return PCFilter::NonSticky(IntRangeUnionFind::from_iter(
                    vec.iter().map(|(a,b)|
                        (Bound::Included(*a), Bound::Included(*b))
                    )
                ));
            }
        }
        if let Some(pc_label_lookups_sticky) = &config.pc_labels_sticky {
            if let Some((start, stop)) = pc_label_lookups_sticky.get(s) {
                return PCFilter::Sticky(*start, *stop);
            }
        }
        unreachable!();
    }).collect::<Vec<_>>();

    let power_config = config.power_settings;
    let power_config_str = serde_json::to_string(&power_config).unwrap();

    let output_file_name = cmd_args.value_of("output_file").unwrap();
    let out_file = match hdf5::File::create(output_file_name) {
        Ok(fil) => fil,
        Err(e) => {
            eprintln!("Error writing to file {}: {}", output_file_name, e);
            return 1;
        }
    };

    let log_file_names: Vec<_> = cmd_args.values_of("log_files").unwrap().collect();
    let log_file_count: u64 = log_file_names.len().try_into().unwrap();

    let log_file_pb_style = ProgressStyle::default_bar().template("{msg} {wide_bar} {pos}/{len}");
    let parse_spinner_style = ProgressStyle::default_spinner()
        .template("{spinner} ({pos}/{len}) {msg}");

    let pb_union = MultiProgress::new();
    let log_file_pb = pb_union.add(ProgressBar::new(log_file_count));
    log_file_pb.set_message("File progress");
    log_file_pb.set_style(log_file_pb_style);
    let parse_spinner = pb_union.add(ProgressBar::new_spinner());
    parse_spinner.set_length(3);
    parse_spinner.set_style(parse_spinner_style);
    parse_spinner.enable_steady_tick(125);

    let pb_thread_handle = std::thread::spawn(move || {
        pb_union.join().unwrap();
    });

    let mut real_exit_code = 0;

    for log_file_name in log_file_pb.wrap_iter(log_file_names.into_iter()) {
        let log_file = match File::open(log_file_name) {
            Ok(fil) => fil,
            Err(e) => {
                eprintln!("Error opening file {}: {}", log_file_name, e);
                return 1;
            }
        };

        let log_file_name_only = match log_file_name.split('/').last() {
            Some(name) => name,
            None => log_file_name
        };

        let (tx_parsed, rx_parsed) = crossbeam_channel::bounded::<(ParsedCPUState, ParsedCPUStateDelta)>(CHANNEL_SIZE);
    
        let (tx_pc, rx_pc) = crossbeam_channel::bounded(CHANNEL_SIZE);
        let (tx_state, rx_state) = crossbeam_channel::bounded(CHANNEL_SIZE);

        let group = out_file.create_group(log_file_name_only).unwrap();

        let pow_attr = group.new_attr::<hdf5::types::VarLenUnicode>()
            .create("power_config").unwrap();
        let power_config_writable: hdf5::types::VarLenUnicode = power_config_str.parse().unwrap();
        pow_attr.write_scalar(&power_config_writable).unwrap();

        let json_file_name = log_file_name.replace(".log",".json");
        if log_file_name != json_file_name {
            match std::fs::read(&json_file_name) {
                Ok(data) => {
                    let key_attr = group.new_attr::<hdf5::types::VarLenUnicode>()
                        .create("data_config").unwrap();
                    let data_writable: hdf5::types::VarLenUnicode = String::from_utf8(data).unwrap().parse().unwrap();
                    key_attr.write_scalar(&data_writable).unwrap();
                },
                Err(e) => {
                    eprintln!("Warning: could not open JSON {}: {}", json_file_name, e);
                }
            };
        }

        let exit_code = AtomicI32::new(0);
        scope(|s| {
            let (tx_pc_2, rx_pc_2) = match label_indexes.len() {
                0 => (None, None),
                _ => {
                    let (tx_2, rx_2) = crossbeam_channel::bounded(CHANNEL_SIZE);
                    (Some(tx_2), Some(rx_2))
                }
            };
            // CPU state thread
            s.spawn(|_| {
                //println!("CPU thread start");
                let mut prev_cpu_state = Arc::new(CPUState::default());
                for (recv_state, delta) in rx_parsed {
                    let mut new_state = recv_state.apply_persistent_state(&prev_cpu_state);
                    new_state.apply(delta);
                    let recv_state_arc = Arc::new(new_state);
                    let pc_val = recv_state_arc.pc();
                    tx_pc.send(pc_val).unwrap();
                    if let Some(ref tx_pc_2) = tx_pc_2 {
                        tx_pc_2.send(pc_val).unwrap()
                    }
                    // Stream over states to another thread for power calc
                    tx_state.send(recv_state_arc.clone()).unwrap();
                    prev_cpu_state = recv_state_arc;
                }
                //println!("Done computing CPU state");
                drop(tx_pc);
                // Dropping an Option::None is a no-op
                drop(tx_pc_2);
                drop(tx_state);
            });
            // Write pc vals to file
            s.spawn(|_| {
                //println!("PC write thread start");
                let pc_dataset = group.new_dataset::<u64>()
                    .deflate(9)
                    .chunk((HDF5_CHUNK_SIZE,))
                    .shape((0..,))
                    .create("pc").unwrap();
                hdf5_helper::write_iter_to_dataset(&pc_dataset, rx_pc);
                //println!("Done writing pcs");
            });
            if !pc_label_lookups.is_empty() {
                let label_iter = rx_pc_2.unwrap().into_iter().scan(0 as LabelBitFlag,|sticky_label, pc| {
                    for (i, pc_filter) in pc_label_lookups.iter().enumerate() {
                        assert!(i <= MAX_LABELS);
                        match pc_filter {
                            PCFilter::NonSticky(rangeset) => {
                                if rangeset.has_element(&pc) {
                                    *sticky_label |= 1 << i;
                                } else {
                                    *sticky_label &= !(1 << i);
                                }
                            },
                            PCFilter::Sticky(start, stop) => {
                                if pc == *start {
                                    *sticky_label |= 1 << i;
                                } else if pc == *stop {
                                    *sticky_label &= !(1 << i);
                                }
                            }
                        }
                    }
                    Some(*sticky_label)
                });
                // Location classifier thread
                s.spawn(|_| {
                    //println!("Label write thread start");
                    let label_attr = group.new_attr::<hdf5::types::VarLenUnicode>()
                    .create("label_mapping").unwrap();
                    let label_config_writable: hdf5::types::VarLenUnicode = label_index_str_option.as_ref().unwrap().parse().unwrap();
                    label_attr.write_scalar(&label_config_writable).unwrap();
        
                    let label_dataset = group.new_dataset::<LabelBitFlag>()
                        .deflate(COMPRESSION_AMOUNT)
                        .chunk((HDF5_CHUNK_SIZE,))
                        .shape((0..,))
                        .create("labels").unwrap();

                    hdf5_helper::write_iter_to_dataset(&label_dataset, label_iter);
                    //println!("Done writing labels");
                });
            }

            // Power computation thread
            s.spawn(|_| {
                //println!("Power thread start");
                let mut prev_state = Arc::new(CPUState::default());
                let pow_iter = rx_state.into_iter().map(|state| {
                    let power = state.compute_power(Some(&prev_state), &power_config);
                    prev_state = state;
                    power
                });
                let pow_dataset = group.new_dataset::<f64>()
                    .deflate(COMPRESSION_AMOUNT)
                    .chunk((HDF5_CHUNK_SIZE,))
                    .shape((0..,))
                    .create("power").unwrap();
                hdf5_helper::write_iter_to_dataset(&pow_dataset, pow_iter);
                //println!("Done writing power");
            });
            let log_file_reader = BufReader::new(log_file);

            let line_parse_iter = log_file_reader.lines();
            let mut track_state = pc_range.is_none();
            let mut sent_anything = false;
            for (line_no, line_result) in line_parse_iter.enumerate() {
                let line = match line_result {
                    Ok(ref s) => s,
                    Err(e) => {
                        eprintln!("Error reading file {}: {}", log_file_name, e);
                        exit_code.store(1, Ordering::Release);
                        break;
                    }
                };
                // When not capturing yet, use simpler regex to grab pc
                if !track_state {
                    if parse_spinner.position() != 1 {
                        parse_spinner.set_message("Waiting for start pc");
                        parse_spinner.set_position(1);
                    }
                    match get_pc(line) {
                        Ok(pc) => {
                            if pc == pc_range.unwrap().start {
                                parse_spinner.set_message("Computing power");
                                parse_spinner.set_position(2);
                                track_state = true;
                                sent_anything = true;
                            }
                        },
                        Err(e) => {
                            eprintln!("Error parsing file {} line {} {}: {}",
                                log_file_name, line_no+1, line, e);
                            exit_code.store(1, Ordering::Release);
                            break;
                        }
                    }
                }
                if track_state {
                    match parse_commit_line(line) {
                        Ok((state, delta)) => {
                            if let Some(range) = pc_range {
                                if state.pc() == range.stop {
                                    track_state = false;
                                    parse_spinner.set_message("Finishing");
                                    parse_spinner.set_position(3);
                                    break;
                                }
                            }
                            tx_parsed.send((state, delta)).unwrap();
                        },
                        Err(e) => {
                            eprintln!("Error parsing file {} line {} {}: {}",
                                log_file_name, line_no+1, line, e);
                            exit_code.store(1, Ordering::Release);
                            break;
                        }
                    }
                }
            }
            drop(tx_parsed);
            if !sent_anything {
                eprintln!("Warning: start pc of file {} was never hit", log_file_name);
            }
            if pc_range.is_some() && track_state {
                eprintln!("Warning: end pc of file {} was never hit", log_file_name);
            }
        }).unwrap();

        out_file.flush().unwrap();
        let exit_code_val = exit_code.load(Ordering::Acquire);
        if exit_code_val != 0 {
            real_exit_code = exit_code_val;
            break;
        }
    }

    parse_spinner.finish_and_clear();
    pb_thread_handle.join().unwrap();

    real_exit_code
}
