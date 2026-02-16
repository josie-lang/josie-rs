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
//! ## Hello World (Tree Evaluation)
//!
//! ```no_run
//! use josie_core::{evaluate, Context, Operators, State};
//! use serde_json::json;
//!
//! let mut state = State::new();
//! let operators = Operators::new();
//! let expr = json!(["u.concat", "Hello", ", ", "World"]);
//!
//! let mut ctx = Context {
//!     state: &mut state,
//!     operators: &operators,
//!     event: None,
//! };
//! let out = evaluate(&expr, &mut ctx).expect("evaluate");
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

pub mod compiler;
pub mod jval;
pub mod program;
pub mod runtime;
pub mod vm;

pub use compiler::Expr;
pub use jval::JVal;
pub use runtime::{
    Context, EvalError, EvalResult, EventContext, FunctionDef, JsonNode, Operator, Operators,
    State, evaluate, get_path, set_path,
};
pub use vm::{IterLocals, eval_expr};
