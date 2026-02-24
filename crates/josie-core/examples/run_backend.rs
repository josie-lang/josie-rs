//! Backend example: Rust injects people data, Josie shapes custom JSON response.
//! Output format: { name, schemaVersion, properties: { totalCount, people: [...] } }
//!
//! Usage: cargo run -p josie-core --example run_backend

use josie_core::program::{execute_program, parse_program};
use josie_core::{read_program, Operators};
use serde_json::json;
use std::process;

static BACKEND_SCRIPT: &str = include_str!("backend_shape.josie");

fn main() {
    let mut doc = read_program(BACKEND_SCRIPT).unwrap_or_else(|e| {
        eprintln!("parse: {:?}", e);
        process::exit(1);
    });

    // Inject people data (e.g. from DB or API)
    let raw_people = json!([
        {
            "id": 1,
            "first": "Jane",
            "last": "Doe",
            "age": 28,
            "city": "London",
            "country": "UK",
            "email": "jane.doe@example.com",
            "phone": "+44 20 7946 0958",
            "tags": ["customer", "vip"]
        },
        {
            "id": 2,
            "first": "Bob",
            "last": "Smith",
            "age": 34,
            "city": "Berlin",
            "country": "DE",
            "email": "bob.smith@example.com",
            "phone": "+49 30 12345678",
            "tags": ["partner"]
        },
        {
            "id": 3,
            "first": "Alice",
            "last": "Wu",
            "age": 41,
            "city": "Singapore",
            "country": "SG",
            "email": "alice.wu@example.com",
            "phone": null,
            "tags": []
        },
    ]);
    doc["state"]["server"] = json!({ "raw_people": raw_people });

    let program = parse_program(&doc).unwrap_or_else(|e| {
        eprintln!("validate: {} - {}", e.code, e.message);
        process::exit(1);
    });
    let operators = Operators::new();
    let out = execute_program(&program, &operators).unwrap_or_else(|e| {
        eprintln!("run: {} - {}", e.code, e.message);
        process::exit(1);
    });

    println!("server.response (custom JSON shape):");
    println!("{}", serde_json::to_string_pretty(&out.state.server["response"]).unwrap());
}
