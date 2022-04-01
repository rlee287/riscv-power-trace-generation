use std::fs::File;
use std::io::{BufRead, BufReader};

mod cpu_structs;

use cpu_structs::CPUState;
use cpu_structs::parse_commit_line;

use std::collections::HashMap;

use rayon::prelude::*;
use atomic_refcell::AtomicRefCell;

fn main() {
    let exit_code = run();
    std::process::exit(exit_code);
}
fn run() -> i32 {
    let input_args: Vec<_> = std::env::args().collect();
    if input_args.len() != 2 {
        eprintln!("Usage: {} log_file", input_args[0]);
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
    let (tx_parsed, rx_parsed) = crossbeam_channel::unbounded();

    let log_file_reader = BufReader::new(log_file);
    let states = log_file_reader.lines().enumerate()
            .par_bridge().into_par_iter().try_for_each_with(tx_parsed, |tx, (i, line_result)| {
        let line = match line_result {
            Ok(ref s) => s,
            Err(e) => {
                eprintln!("Error reading file: {}", e);
                return Err(1);
            }
        };
        match parse_commit_line(&line) {
            Ok((state, delta)) => {
                tx.send((i, state, delta)).unwrap();
                return Ok(());
            },
            Err(e) => {
                eprintln!("Error with line {}: {}", line, e);
                return Err(1);
            }
        }
    });

    let (tx_pc, rx_pc) = crossbeam_channel::unbounded();
    let (tx_state, rx_state) = crossbeam_channel::unbounded();

    let location_labels = AtomicRefCell::new(Vec::new());
    let location_labels_ref = &location_labels;

    let power_values = AtomicRefCell::new(Vec::new());
    let power_values_ref = &power_values;

    rayon::scope(move |s| {
        s.spawn(move |_| {
            let mut prev_cpu_state = CPUState::default();

            let mut expected_index = 0;
            let mut stashed_states = HashMap::new();
            'process_states: loop {
                let mut sent_value = false;
                while !sent_value {
                    let (ctr, mut recv_state, delta) = match rx_parsed.recv() {
                        Ok(tup) => tup,
                        Err(_) => break 'process_states
                    };
                    if ctr == expected_index {
                        recv_state.copy_persistent_state(&prev_cpu_state);
                        recv_state.apply(delta);
                        tx_pc.send(recv_state.pc()).unwrap();
                        // TODO: stream over states to another thread for power calc
                        tx_state.send(recv_state.clone()).unwrap();
                        prev_cpu_state = recv_state;
                        sent_value = true;
                    } else {
                        stashed_states.insert(ctr, (recv_state, delta));
                        match stashed_states.remove(&expected_index) {
                            Some((mut recv_state, delta)) => {
                                recv_state.copy_persistent_state(&prev_cpu_state);
                                recv_state.apply(delta);
                                tx_pc.send(recv_state.pc()).unwrap();
                                // TODO: stream over states to another thread for power calc
                                tx_state.send(recv_state.clone()).unwrap();
                                prev_cpu_state = recv_state;
                                sent_value = true;
                            },
                            None => {}
                        }
                    }
                }
                expected_index+=1;
            }
            // Clear out the rest of the stashed states
            while let Some((mut recv_state, delta)) = stashed_states.remove(&expected_index) {
                recv_state.copy_persistent_state(&prev_cpu_state);
                recv_state.apply(delta);
                tx_pc.send(recv_state.pc()).unwrap();
                tx_state.send(recv_state.clone()).unwrap();
                // TODO: stream over states to another thread for power calc
                prev_cpu_state = recv_state;
                expected_index+=1;
            }
        
            assert_eq!(stashed_states.len(), 0);
        });
        s.spawn(move |_| {
            let mut location_vec = location_labels_ref.borrow_mut();
            for pc in rx_pc {
                location_vec.push(pc);
            }
        });
        s.spawn(move |_| {
            let mut power_vec = power_values_ref.borrow_mut();
            for state in rx_state {
                let hamming_weight: u32 = state.xregs().iter()
                    .map(|v| v.count_ones()).sum();
                power_vec.push(hamming_weight);
            }
        });
    });
    println!("{:x}", location_labels.borrow().last().unwrap());
    println!("{}", power_values.borrow().last().unwrap());
    match states {
        Ok(()) => 0,
        Err(e) => e
    }
}
