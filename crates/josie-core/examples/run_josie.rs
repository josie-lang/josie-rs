//! Run a .josie file: read → read_program → parse_program → execute_program.
//!
//! Usage: cargo run -p josie-core --example run_josie -- examples/hello.josie

use josie_core::program::{execute_program, parse_program};
use josie_core::{read_program, Operators};
use std::env;
use std::fs;
use std::process;

fn main() {
    let path = match env::args().nth(1) {
        Some(p) => p,
        None => {
            eprintln!("Usage: run_josie <path.josie>");
            eprintln!("  e.g. cargo run -p josie-core --example run_josie -- examples/hello.josie");
            process::exit(1);
        }
    };
    let src = fs::read_to_string(&path).unwrap_or_else(|e| {
        eprintln!("read {}: {}", path, e);
        process::exit(1);
    });
    let doc = read_program(&src).unwrap_or_else(|e| {
        eprintln!("parse {}: {:?}", path, e);
        process::exit(1);
    });
    let program = parse_program(&doc).unwrap_or_else(|e| {
        eprintln!("validate {}: {} ({})", path, e.code, e.message);
        process::exit(1);
    });
    let operators = Operators::new();
    let out = execute_program(&program, &operators).unwrap_or_else(|e| {
        eprintln!("run {}: {} - {}", path, e.code, e.message);
        process::exit(1);
    });
    println!("value: {}", serde_json::to_string_pretty(&out.value).unwrap());
    let state_json = serde_json::json!({
        "client": out.state.client,
        "server": out.state.server,
    });
    println!("state: {}", serde_json::to_string_pretty(&state_json).unwrap());
}
