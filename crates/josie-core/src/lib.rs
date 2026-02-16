//! JOSIE Core crate.
//!
//! This crate intentionally separates execution concerns into layers:
//!
//! - `runtime`: baseline tree evaluator and operator registry.
//! - `program`: pipeline document contract, validation, compilation, and
//!   multi-strategy execution dispatch.
//! - `compiler` + `vm` + `jval`: typed expression lowering and fast VM
//!   execution used by compiled pipeline paths.
//!
//! Performance strategy (high level):
//!
//! 1. Validate once (`parse_program` / `validate_program`).
//! 2. Compile once (`compile_program`) into the best available execution body:
//!    pattern-specialized fast plan, compiled pipeline, or tree fallback.
//! 3. Execute many times with mutable state.
//!
//! The critical design rule is semantic equivalence across execution tiers:
//! every optimization path must produce the same observable output as the
//! generic evaluator.

pub mod runtime;
pub mod program;
pub mod jval;
pub mod compiler;
pub mod vm;

pub use runtime::{
    Context, EvalError, EvalResult, EventContext, FunctionDef, JsonNode, Operator, Operators,
    State, evaluate, get_path, set_path,
};
pub use jval::JVal;
pub use compiler::Expr;
pub use vm::{IterLocals, eval_expr};
