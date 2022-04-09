use std::fs::File;
use std::io::{Read, BufRead, BufReader, Seek};

use clap::{Arg, Command};

mod cpu_structs;
mod power_config;
mod arithmetic_utils;
mod hdf5_helper;

use cpu_structs::{CPUState, CPUStateDelta};
use cpu_structs::parse_commit_line;

use power_config::Config;

use range_union_find::IntRangeUnionFind;
use std::ops::Bound;

//use rayon::prelude::*;
use crossbeam_utils::thread::scope;
//use std::thread::spawn;
use std::sync::{Arc, Mutex};
use std::sync::atomic::{Ordering, AtomicI32};

type LabelBitFlag = u16;
const MAX_LABELS: usize = LabelBitFlag::BITS as usize;

const HDF5_CHUNK_SIZE: usize = 256;

fn main() {
    let exit_code = run();
    std::process::exit(exit_code);
}
fn run() -> i32 {
    let cmd_parser = Command::new("RISCV power trace generator")
        .arg(Arg::new("config_file").takes_value(true).required(true)
            .long("config-file"))
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
    let label_indexes = match &config.pc_labels {
        Some(labels) => {
            if labels.len() >= MAX_LABELS {
                eprintln!("Error parsing config files: too many classes");
                return 1;
            }
            Some(labels.keys().cloned().collect::<Vec<_>>())
        },
        None => {None}
    };
    let label_index_str_option = label_indexes.map(|label_vec| serde_json::to_string(&label_vec).unwrap());
    let range_lookups = config.pc_labels.map(|dict| {
        dict.values().map(|vec| {
            IntRangeUnionFind::from_iter(vec.iter().map(
                |(a,b)| (Bound::Included(*a), Bound::Included(*b))))
        }).collect::<Vec<_>>()
    });
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
    for log_file_name in log_file_names {
        let log_file = match File::open(log_file_name) {
            Ok(fil) => fil,
            Err(e) => {
                eprintln!("Error opening file {}: {}", log_file_name, e);
                return 1;
            }
        };

        let log_file_name_only = match log_file_name.split("/").last() {
            Some(name) => name,
            None => log_file_name
        };

        let (tx_parsed, rx_parsed) = crossbeam_channel::bounded::<(CPUState, CPUStateDelta)>(1024*4);
    
        let (tx_pc, rx_pc) = crossbeam_channel::bounded(1024*4);
        let (tx_state, rx_state) = crossbeam_channel::bounded(1024*4);
    
        let location_labels = Mutex::new(Vec::new());
        let pc_values = Mutex::new(Vec::new());
        let power_values = Mutex::new(Vec::new());

        let exit_code = AtomicI32::new(0);
        eprintln!("Generating power data for {}", log_file_name_only);
        scope(|s| {
            // CPU state thread
            s.spawn(|_| {
                let mut prev_cpu_state = Arc::new(CPUState::default());
                for (mut recv_state, delta) in rx_parsed {
                    if (&exit_code).load(Ordering::Acquire) != 0 {
                        break;
                    }
                    recv_state.copy_persistent_state(&prev_cpu_state);
                    recv_state.apply(delta);
                    let recv_state_arc = Arc::new(recv_state);
                    tx_pc.send(recv_state_arc.pc()).unwrap();
                    // Stream over states to another thread for power calc
                    tx_state.send(recv_state_arc.clone()).unwrap();
                    prev_cpu_state = recv_state_arc;
                }
                drop(tx_pc);
                drop(tx_state);
            });
            // Location classifier thread
            s.spawn(|_| {
                let mut pc_values = (&pc_values).lock().unwrap();
                let mut location_labels = (&location_labels).lock().unwrap();
                for pc in rx_pc {
                    //println!("PC queue len {}", rx_pc.len());
                    if (&exit_code).load(Ordering::Acquire) != 0 {
                        break;
                    }
                    pc_values.push(pc);
                    if let Some(ref range_obj_vec) = range_lookups {
                        let mut label: u32 = 0;
                        for (i, range_obj) in range_obj_vec.iter().enumerate() {
                            assert!(i <= MAX_LABELS);
                            if range_obj.has_element(&pc) {
                                label |= 1 << i;
                            }
                        }
                        location_labels.push(label);
                    }
                }
            });
            // Power computation thread
            s.spawn(|_| {
                let mut power_values = (&power_values).lock().unwrap();
                let mut prev_state = Arc::new(CPUState::default());
                for state in rx_state.clone() {
                    //println!("Power queue len {}", rx_state.len());
                    if (&exit_code).load(Ordering::Acquire) != 0 {
                        break;
                    }
                    let power = state.compute_power(Some(&prev_state), &power_config);
                    power_values.push(power);
                    prev_state = state;
                }
            });
            let mut log_file_reader = BufReader::new(log_file);
            log_file_reader.seek(std::io::SeekFrom::Start(0)).unwrap();

            let line_parse_iter = log_file_reader.lines();
            let mut track_state = pc_range.is_none();
            let mut sent_anything = false;
            for line_result in line_parse_iter {
                let line = match line_result {
                    Ok(ref s) => s,
                    Err(e) => {
                        eprintln!("Error reading file: {}", e);
                        exit_code.store(1, Ordering::Release);
                        break;
                    }
                };
                match parse_commit_line(&line) {
                    Ok((state, delta)) => {
                        if let Some(range) = pc_range {
                            if state.pc() == range.start {
                                eprintln!("Starting data capture");
                                track_state = true;
                                sent_anything = true;
                            } else if state.pc() == range.stop {
                                track_state = false;
                                eprintln!("Stopping data capture");
                                break;
                            }
                        }
                        if track_state {
                            tx_parsed.send((state, delta)).unwrap();
                        }
                        //println!("Parse queue len {}", tx_parsed.len());
                    },
                    Err(e) => {
                        eprintln!("Error with line {}: {}", line, e);
                        exit_code.store(1, Ordering::Release);
                        break;
                    }
                }
                //eprintln!("Line {}", line_ctr);
            }
            drop(tx_parsed);
            if !sent_anything {
                eprintln!("Warning: start pc was never hit")
            }
            if pc_range.is_some() && track_state == true {
                eprintln!("Warning: end pc was never hit")
            }
        }).unwrap();

        let exit_code_val = exit_code.load(Ordering::Acquire);
        if exit_code_val != 0 {
            return 1;
        }

        let out_pc_vec = pc_values.into_inner().unwrap();
        let out_power_vec = power_values.into_inner().unwrap();
        let out_label_vec = location_labels.into_inner().unwrap();

        let group = out_file.create_group(log_file_name_only).unwrap();

        let pc_dataset = group.new_dataset::<u64>()
            .deflate(9)
            .chunk((HDF5_CHUNK_SIZE,))
            .shape((0..,))
            .create("pc").unwrap();
        hdf5_helper::write_iter_to_dataset(&pc_dataset, out_pc_vec);

        let pow_dataset = group.new_dataset::<f64>()
            .deflate(9)
            .chunk((HDF5_CHUNK_SIZE,))
            .shape((0..,))
            .create("power").unwrap();
        hdf5_helper::write_iter_to_dataset(&pow_dataset, out_power_vec);

        let pow_attr = group.new_attr::<hdf5::types::VarLenUnicode>()
            .create("power_config").unwrap();
        let power_config_writable: hdf5::types::VarLenUnicode = power_config_str.parse().unwrap();
        pow_attr.write_scalar(&power_config_writable).unwrap();

        if let Some(ref label_str) = label_index_str_option {
            let label_attr = group.new_attr::<hdf5::types::VarLenUnicode>()
            .create("label_mapping").unwrap();
            let label_config_writable: hdf5::types::VarLenUnicode = label_str.parse().unwrap();
            label_attr.write_scalar(&label_config_writable).unwrap();

            let label_dataset = group.new_dataset::<u32>()
                .deflate(9)
                .chunk((HDF5_CHUNK_SIZE,))
                .shape((0..,))
                .create("labels").unwrap();
            
            hdf5_helper::write_iter_to_dataset(&label_dataset, out_label_vec);
        }
        out_file.flush().unwrap();
    }

    0
}
