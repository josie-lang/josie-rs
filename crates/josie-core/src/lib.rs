//! # josie-core
//!
//! Core runtime and execution planner for **JOSIE**
//! (JSON Omni Safe Interactive Expressions).
//!
//! ## Module Roles
//!
//! | Module | Responsibility | Public Entry Points |
//! |---|---|---|
//! | `runtime` | Tree evaluator + operator registry | [`evaluate`], [`Operators`], [`State`] |
//! | `program` | Program envelope validation, compile planning, pipeline execution | `parse_program`, `compile_program`, `execute_program*` |
//! | `compiler` | `serde_json::Value` expression -> typed IR | [`Expr`] and `compile_expr` |
//! | `vm` | Fast typed IR evaluator | [`eval_expr`], [`IterLocals`] |
//! | `jval` | Internal value representation for VM execution | [`JVal`] |
//!
//! ## Execution Contract
//!
//! 1. Parse/validate once.
//! 2. Compile once into best available strategy.
//! 3. Execute many times.
//!
//! Optimization tiers in `program`:
//!
//! 1. Specialized internal fast plans.
//! 2. Specialized typed host-call fast plans.
//! 3. Compiled pipeline IR path.
//! 4. Generic tree/pipeline fallback.
//!
//! **Rule:** all optimized paths must preserve the same observable semantics
//! as the generic evaluator.
//!
//! ## Hello World (Simple API)
//!
//! ```no_run
//! use josie_core::eval;
//! use serde_json::json;
//!
//! let out = eval(&json!(["util.concat", "Hello", ", ", "World"])).expect("eval");
//! assert_eq!(out, json!("Hello, World"));
//! ```
//!
//! ## Inject Custom `x.*` Operator
//!
//! ```no_run
//! use josie_core::{Context, EvalResult, Operator, Operators, State, evaluate};
//! use serde_json::{json, Value};
//!
//! fn op_x_echo(args: &[Value], _ctx: &mut Context) -> EvalResult {
//!     Ok(args.first().cloned().unwrap_or(Value::Null))
//! }
//!
//! let mut operators = Operators::new();
//! operators.register("x.echo", op_x_echo as Operator);
//!
//! let mut state = State::new();
//! let expr = json!(["x.echo", {"ok": true}]);
//! let mut ctx = Context { state: &mut state, operators: &operators, event: None };
//! let out = evaluate(&expr, &mut ctx).expect("x.echo");
//! assert_eq!(out, json!({"ok": true}));
//! ```

use serde_json::Value;

pub mod compiler;
pub mod engine;
pub mod jval;
pub mod program;
pub mod runtime;
pub mod vm;

pub use compiler::Expr;
pub use engine::Engine;
pub use jval::JVal;
pub use runtime::{
    Context, EvalError, EvalResult, EventContext, FunctionDef, JsonNode, Operator, Operators,
    State, evaluate, get_path, set_path,
};
pub use vm::{IterLocals, eval_expr};

/// Evaluate an expression with default runtime components:
/// - empty [`State`]
/// - default [`Operators`]
/// - no [`EventContext`]
pub fn eval(node: &Value) -> EvalResult {
    let mut state = State::new();
    let operators = Operators::new();
    let mut ctx = Context {
        state: &mut state,
        operators: &operators,
        event: None,
    };
    evaluate(node, &mut ctx)
}

/// Evaluate an expression with caller-provided state and default operators.
pub fn eval_with_state(node: &Value, state: &mut State) -> EvalResult {
    let operators = Operators::new();
    let mut ctx = Context {
        state,
        operators: &operators,
        event: None,
    };
    evaluate(node, &mut ctx)
}

/// Evaluate an expression with caller-provided operators and empty state.
pub fn eval_with_operators(node: &Value, operators: &Operators) -> EvalResult {
    let mut state = State::new();
    let mut ctx = Context {
        state: &mut state,
        operators,
        event: None,
    };
    evaluate(node, &mut ctx)
}
