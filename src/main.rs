use std::fs::File;
use std::io::{BufRead, BufReader};

mod cpu_structs;
//mod iter_utils;

use cpu_structs::{CPUState, CPUStateDelta};
use cpu_structs::parse_commit_line;

use std::collections::HashMap;

use rayon::prelude::*;

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
    let (tx, rx) = crossbeam_channel::unbounded();

    let log_file_reader = BufReader::new(log_file);
    let states = log_file_reader.lines().enumerate()
            .par_bridge().into_par_iter().try_for_each_with(tx, |tx, (i, line_result)| {
        let line = match line_result {
            Ok(ref s) => s,
            Err(e) => {
                eprintln!("Error reading file: {}", e);
                return Err(1);
            }
        };
        match parse_commit_line(&line) {
            Ok((state, delta)) => {
                if tx.send((i, state, delta)).is_err() {
                    eprintln!("Error sending CPU state");
                    return Err(1);
                }
                return Ok(());
            },
            Err(e) => {
                eprintln!("Error with line {}: {}", line, e);
                return Err(1);
            }
        }
    });

    let mut prev_cpu_state = CPUState::default();

    let mut expected_index = 0;
    let mut stashed_states = HashMap::new();
    'process_states: loop {
        let mut sent_value = false;
        while !sent_value {
            let (ctr, mut recv_state, delta) = match rx.recv() {
                Ok(tup) => tup,
                Err(_) => break 'process_states
            };
            if ctr == expected_index {
                recv_state.copy_persistent_state(&prev_cpu_state);
                recv_state.apply(delta);
                // TODO: stream over states to another thread for power calc
                prev_cpu_state = recv_state;
                sent_value = true;
            } else {
                stashed_states.insert(ctr, (recv_state, delta));
                match stashed_states.remove(&expected_index) {
                    Some((mut recv_state, delta)) => {
                        recv_state.copy_persistent_state(&prev_cpu_state);
                        recv_state.apply(delta);
                        // TODO: stream over states to another thread for power calc
                        prev_cpu_state = recv_state;
                        sent_value = true;
                    },
                    None => {}
                }
            }
        }
        expected_index+=1;
    }
    match states {
        Ok(()) => 0,
        Err(e) => e
    }
}
