use std::fs::File;
use std::io::{BufRead, BufReader};

mod cpu_structs;
use cpu_structs::{CPUState, CPUStateDelta};
use cpu_structs::parse_commit_line;

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
    let mut prev_cpu_state = CPUState::default();
    for (i, line_result) in BufReader::new(log_file).lines().enumerate() {
        let mut new_cpu_state: CPUState;
        let cpu_delta: CPUStateDelta;
        let line = match line_result {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Error reading file: {}", e);
                return 1;
            }
        };
        match parse_commit_line(&line) {
            Ok((state, delta)) => {
                new_cpu_state = state;
                cpu_delta = delta;
            },
            Err(e) => {
                eprintln!("Error with line {}: {}", line, e);
                return 1;
            }
        }
        new_cpu_state.copy_persistent_state(&prev_cpu_state);
        new_cpu_state.apply(cpu_delta);
        // TODO: compute power values
        prev_cpu_state = new_cpu_state;
    }
    return 0;
}
