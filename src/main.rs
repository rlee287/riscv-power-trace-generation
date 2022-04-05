use std::fs::File;
use std::io::{Read, Write, BufRead, BufReader, BufWriter};

use clap::{Arg, Command};

mod cpu_structs;
mod power_config;
mod arithmetic_utils;

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

fn main() {
    let exit_code = run();
    std::process::exit(exit_code);
}
fn run() -> i32 {
    let cmd_parser = Command::new("RISCV power trace generator")
        .arg(Arg::new("config_file").takes_value(true).required(true)
            .long("config-file"))
        .arg(Arg::new("print_power").long("print-power"))
        .arg(Arg::new("print_pc").long("print-pc").conflicts_with("print_power"))
        .arg(Arg::new("print_label").long("print-label")
            .conflicts_with("print_power").conflicts_with("print_pc"))
        .arg(Arg::new("output_file").takes_value(true).required(true)
            .long("output-file").short('o'))
        .arg(Arg::new("log_files").takes_value(true).required(true)
            .last(true));
    let cmd_args = cmd_parser.get_matches();

    if !cmd_args.is_present("print_pc") && !cmd_args.is_present("print_power") && !cmd_args.is_present("print_label") {
        todo!("Final HDF5 not ready");
    }

    let log_file_name = cmd_args.value_of("log_files").unwrap();
    let log_file = match File::open(log_file_name) {
        Ok(fil) => fil,
        Err(e) => {
            eprintln!("Error opening file {}: {}", log_file_name, e);
            return 1;
        }
    };
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

    println!("{:?}", config);
    //let clock_frequency = power_config.clock_frequency;
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
    let range_lookups = config.pc_labels.map(|dict| {
        dict.values().map(|vec| {
            IntRangeUnionFind::from_iter(vec.iter().map(
                |(a,b)| (Bound::Included(*a), Bound::Included(*b))))
        }).collect::<Vec<_>>()
    });
    let power_config = config.power_settings;

    let output_file_name = cmd_args.value_of("output_file").unwrap();
    let output_file = match File::create(output_file_name) {
        Ok(fil) => fil,
        Err(e) => {
            eprintln!("Error opening file {}: {}", output_file_name, e);
            return 1;
        }
    };

    let (tx_parsed, rx_parsed) = crossbeam_channel::bounded::<(CPUState, CPUStateDelta)>(1024*64);

    let (tx_pc, rx_pc) = crossbeam_channel::bounded(1024);
    let (tx_state, rx_state) = crossbeam_channel::bounded(1024*4);

    let location_labels = Arc::new(Mutex::new(Vec::new()));
    let pc_values = Arc::new(Mutex::new(Vec::new()));
    let power_values = Arc::new(Mutex::new(Vec::new()));

    let exit_code = Arc::new(AtomicI32::new(0));
    eprintln!("Generating power data");
    scope(|s| {
        let exit_code_ref = exit_code.clone();
        // CPU state thread
        s.spawn(move |_| {
            let mut prev_cpu_state = Arc::new(CPUState::default());
    
            for (mut recv_state, delta) in rx_parsed {
                if exit_code_ref.load(Ordering::Acquire) != 0 {
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
        });
        let exit_code_ref = exit_code.clone();
        let pc_values_lock = pc_values.clone();
        let location_label_lock = location_labels.clone();
        // Location classifier thread
        s.spawn(move |_| {
            let mut pc_values = pc_values_lock.lock().unwrap();
            let mut location_labels = location_label_lock.lock().unwrap();
            for pc in rx_pc.clone() {
                //println!("PC queue len {}", rx_pc.len());
                if exit_code_ref.load(Ordering::Acquire) != 0 {
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
        let exit_code_ref = exit_code.clone();
        let power_value_lock = power_values.clone();
        // Power computation thread
        s.spawn(move |_| {
            let mut power_values = power_value_lock.lock().unwrap();
            let mut prev_state = Arc::new(CPUState::default());
            for state in rx_state.clone() {
                //println!("Power queue len {}", rx_state.len());
                if exit_code_ref.load(Ordering::Acquire) != 0 {
                    break;
                }
                let power = state.compute_power(Some(&prev_state), &power_config);
                power_values.push(power);
                prev_state = state;
            }
        });
        let log_file_reader = BufReader::new(log_file);
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

    let out_pc_vec = Arc::try_unwrap(pc_values).unwrap().into_inner().unwrap();
    let out_power_vec = Arc::try_unwrap(power_values).unwrap().into_inner().unwrap();
    let out_location_vec = Arc::try_unwrap(location_labels).unwrap().into_inner().unwrap();

    eprintln!("Writing outputs to file");
    let mut buf_out_file = BufWriter::new(output_file);
    if cmd_args.is_present("print_power") {
        for val in out_power_vec {
            writeln!(buf_out_file, "{}", val).unwrap();
        }
    }
    if cmd_args.is_present("print_pc") {
        for val in out_pc_vec {
            writeln!(buf_out_file, "0x{:016x}", val).unwrap();
        }
    }
    if cmd_args.is_present("print_label") {
        for val in out_location_vec {
            writeln!(buf_out_file, "{:016b}", val).unwrap();
        }
    }
    exit_code.load(Ordering::Acquire)
}
