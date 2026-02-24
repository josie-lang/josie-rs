//! Data engineering example: Rust injects rows into server.raw_data,
//! Josie wrangles (group_by region, aggregate, sort) and writes server.result.
//!
//! Usage: cargo run -p josie-core --example run_data_eng

use josie_core::program::{execute_program, parse_program};
use josie_core::{read_program, Operators};
use serde_json::json;
use std::process;

static WRANGLE_SCRIPT: &str = include_str!("wrangle.josie");

fn main() {
    let mut doc = read_program(WRANGLE_SCRIPT).unwrap_or_else(|e| {
        eprintln!("parse: {:?}", e);
        process::exit(1);
    });

    // Inject data from Rust (e.g. from DB, file, API)
    let raw_data = json!([
        { "region": "APAC", "amount": 100, "product": "A" },
        { "region": "EMEA", "amount": 50, "product": "B" },
        { "region": "APAC", "amount": 80, "product": "A" },
        { "region": "Americas", "amount": 120, "product": "C" },
        { "region": "EMEA", "amount": 30, "product": "B" },
        { "region": "APAC", "amount": 60, "product": "C" },
    ]);
    doc["state"]["server"] = json!({ "raw_data": raw_data });

    let program = parse_program(&doc).unwrap_or_else(|e| {
        eprintln!("validate: {} - {}", e.code, e.message);
        process::exit(1);
    });
    let operators = Operators::new();
    let out = execute_program(&program, &operators).unwrap_or_else(|e| {
        eprintln!("run: {} - {}", e.code, e.message);
        process::exit(1);
    });

    println!("server.result (by region: count, total amount, desc):");
    println!("{}", serde_json::to_string_pretty(&out.state.server["result"]).unwrap());
}
