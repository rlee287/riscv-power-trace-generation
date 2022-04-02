use std::fs::File;
use std::io::{Read, Write, BufRead, BufReader, BufWriter};

mod cpu_structs;
mod power_config;
mod arithmetic_utils;

use cpu_structs::{CPUState, CPUStateDelta};
use cpu_structs::parse_commit_line;

use power_config::CPUPowerSettings;

//use rayon::prelude::*;
use crossbeam_utils::thread::scope;
//use std::thread::spawn;
use std::sync::{Arc, Mutex};
use std::sync::atomic::{Ordering, AtomicI32};

fn main() {
    let exit_code = run();
    std::process::exit(exit_code);
}
fn run() -> i32 {
    let input_args: Vec<_> = std::env::args().collect();
    if input_args.len() != 4 {
        eprintln!("Usage: {} log_file config_file output_file", input_args[0]);
        return 2;
    }
    let log_file_name = input_args[1].as_str();
    let log_file = match File::open(log_file_name) {
        Ok(fil) => fil,
        Err(e) => {
            eprintln!("Error opening file {}: {}", log_file_name, e);
            return 1;
        }
    };
    let config_file_name = input_args[2].as_str();
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
    let power_config: CPUPowerSettings = match toml::from_slice(&config_file_contents) {
        Ok(config) => config,
        Err(e) => {
            eprintln!("Error parsing config file: {}", e);
            return 1;
        }
    };
    drop(config_file_contents);
    drop(config_file);

    let output_file_name = input_args[3].as_str();
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

    let power_values = Arc::new(Mutex::new(Vec::new()));

    let exit_code = Arc::new(AtomicI32::new(0));
    eprintln!("Generating power data");
    scope(|s| {
        let exit_code_ref = exit_code.clone();
        let pc_range = power_config.pc_range;
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
        let location_label_lock = location_labels.clone();
        // Location classifier thread
        s.spawn(move |_| {
            let mut location_labels = location_label_lock.lock().unwrap();
            for pc in rx_pc.clone() {
                //println!("PC queue len {}", rx_pc.len());
                if exit_code_ref.load(Ordering::Acquire) != 0 {
                    break;
                }
                location_labels.push(pc);
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

    let out_vec = Arc::try_unwrap(power_values).unwrap().into_inner().unwrap();

    eprintln!("Writing outputs to file");
    let mut buf_out_file = BufWriter::new(output_file);
    for val in out_vec {
        writeln!(buf_out_file, "{}", val).unwrap();
    }
    exit_code.load(Ordering::Acquire)
}
