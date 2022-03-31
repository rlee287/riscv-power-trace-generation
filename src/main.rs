use std::fs::File;
use std::io::{BufRead, BufReader};

mod cpu_structs;
//use cpu_structs

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
    for (i, line) in BufReader::new(log_file).lines().enumerate() {
        
    }
    return 0;
}
