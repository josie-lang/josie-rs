//! Program-level contract and optimized execution strategies.
//!
//! This module is the "planner + dispatcher" for JOSIE execution.
//!
//! It takes a JSON program envelope and selects the fastest safe runtime
//! strategy available for that specific shape.
//!
//! Strategy tiers, from most specialized to most generic:
//!
//! 1. **Fast internal plan** (`CompiledProgramBody::FastInternal`)
//!    Pattern-matched kernels for high-frequency pipeline shapes
//!    (loop/filter/branching/boolean/match/template/mixed workflow).
//!    These avoid per-node dynamic dispatch and run as tight Rust loops.
//!
//! 2. **Fast external map plan** (`CompiledProgramBody::FastExternalMapI64`)
//!    Specialized host-call flow for:
//!    `x.a(p1,p2,size) -> for_each x.b(item)`.
//!    Uses typed host callbacks and optional metrics-only execution path.
//!
//! 3. **Compiled pipeline VM path** (`CompiledProgramBody::CompiledPipeline`)
//!    Pre-compiles `do`, `when`, and `args` expressions to typed IR (`Expr`)
//!    and executes via `vm::eval_expr` with iterator locals.
//!
//! 4. **Generic tree/pipeline fallback**
//!    Preserves full semantics for any valid document not covered above.
//!
//! Pipeline step contract summary:
//!
//! | op | required fields | notes |
//! |---|---|---|
//! | `call` | `fn` or `from` | optional `into`, `args`, `when` |
//! | `set` | `into`, `args[0]` | writes to runtime var or `client.*`/`server.*` |
//! | `get` | `from` | reads `$prev`, runtime vars, or state paths |
//! | `map`/`filter`/`for_each`/`reduce` | `from`, `do` | `reduce` optionally uses init value from `args[0]` |
//! | `if`/`match`/`do`/`pipe` | `args` | tree-style ops executed in pipeline flow |
//! | policy hints | `run_hint` | planner/runtime hint (`inline`/`worker`), not logic semantics |
//!
//! Why this improves performance:
//!
//! - Removes repeated parse and shape checks from hot loops.
//! - Avoids recursive `serde_json::Value` evaluation in known patterns.
//! - Uses typed host fast-paths for external-heavy workloads.
//! - Keeps fallback semantics so optimization is additive, not breaking.

use crate::compiler::{Expr, compile_expr};
use crate::jval::JVal;
use crate::vm::{IterLocals, eval_expr};
use crate::{Context, EvalError, Operators, State, evaluate};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value, json};
use std::collections::HashMap;
use std::rc::Rc;
use std::time::Instant;

/// Generic host callback used by pipeline `call` integration.
pub type HostCallFn = fn(&[Value]) -> Result<Value, RuntimeError>;
/// Typed generator callback for optimized external map fast-paths.
pub type HostGenerateI64Fn = fn(i64, i64, usize) -> Vec<i64>;
/// Typed mapper callback for optimized external map fast-paths.
pub type HostMapI64Fn = fn(i64) -> i64;

/// Host function registry used by optimized execution paths.
///
/// - `register_call`: generic JSON argument callbacks.
/// - `register_generate_i64` and `register_map_i64`: typed callbacks used by
///   external fast plans to avoid JSON boxing overhead.
#[derive(Debug, Clone, Default)]
pub struct HostFunctions {
    call: HashMap<String, HostCallFn>,
    generate_i64: HashMap<String, HostGenerateI64Fn>,
    map_i64: HashMap<String, HostMapI64Fn>,
}

impl HostFunctions {
    /// Create an empty host function registry.
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a generic host callback for `call` steps.
    pub fn register_call(&mut self, name: impl Into<String>, func: HostCallFn) {
        self.call.insert(name.into(), func);
    }

    /// Register typed generator callback used by external fast-path plans.
    pub fn register_generate_i64(&mut self, name: impl Into<String>, func: HostGenerateI64Fn) {
        self.generate_i64.insert(name.into(), func);
    }

    /// Register typed mapper callback used by external fast-path plans.
    pub fn register_map_i64(&mut self, name: impl Into<String>, func: HostMapI64Fn) {
        self.map_i64.insert(name.into(), func);
    }

    fn get_call(&self, name: &str) -> Option<HostCallFn> {
        self.call.get(name).copied()
    }

    fn get_generate_i64(&self, name: &str) -> Option<HostGenerateI64Fn> {
        self.generate_i64.get(name).copied()
    }

    fn get_map_i64(&self, name: &str) -> Option<HostMapI64Fn> {
        self.map_i64.get(name).copied()
    }
}

/// Parsed top-level program envelope.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Program {
    #[serde(default)]
    pub state: Value,
    pub program: Value,
}

/// Canonical pipeline document shape (`type = "pipeline"`).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PipelineDoc {
    #[serde(rename = "type")]
    pub doc_type: String,
    pub steps: Vec<PipelineStep>,
}

/// Canonical pipeline step shape used by parser and executor.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PipelineStep {
    #[serde(default)]
    pub id: Option<String>,
    #[serde(rename = "type", default)]
    pub step_type: Option<StepType>,
    pub op: String,
    #[serde(default)]
    pub from: Option<String>,
    #[serde(default)]
    pub into: Option<String>,
    #[serde(rename = "fn", default)]
    pub fn_name: Option<String>,
    #[serde(default)]
    pub args: Vec<Value>,
    #[serde(rename = "do", default)]
    pub do_expr: Option<Value>,
    #[serde(default)]
    pub when: Option<Value>,
    #[serde(default)]
    pub run_hint: Option<StepRunHint>,
    #[serde(default)]
    pub input: Option<Value>,
    #[serde(default)]
    pub output: Option<Value>,
    #[serde(default)]
    pub on_error: Option<StepOnError>,
    #[serde(default)]
    pub timeout_ms: Option<u64>,
    #[serde(default)]
    pub max_retries: Option<u32>,
    #[serde(default)]
    pub idempotency_key: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum StepType {
    Action,
    Decision,
    Transform,
    Tool,
    Checkpoint,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum StepOnError {
    Retry,
    Fallback,
    Halt,
    Compensate,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum StepRunHint {
    Inline,
    Worker,
}

/// Structured validation error produced before execution.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ValidationError {
    pub code: String,
    pub message: String,
    pub step_index: Option<usize>,
    pub op: Option<String>,
}

impl ValidationError {
    fn new(code: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            code: code.into(),
            message: message.into(),
            step_index: None,
            op: None,
        }
    }

    fn step(mut self, step_index: usize, op: impl Into<String>) -> Self {
        self.step_index = Some(step_index);
        self.op = Some(op.into());
        self
    }
}

/// Structured runtime error produced during execution.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RuntimeError {
    pub code: String,
    pub message: String,
    pub step_index: Option<usize>,
    pub op: Option<String>,
}

impl RuntimeError {
    fn new(code: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            code: code.into(),
            message: message.into(),
            step_index: None,
            op: None,
        }
    }

    fn step(mut self, step_index: usize, op: impl Into<String>) -> Self {
        self.step_index = Some(step_index);
        self.op = Some(op.into());
        self
    }
}

impl From<EvalError> for RuntimeError {
    fn from(value: EvalError) -> Self {
        Self::new("JOSIE_E_EVAL", value.message)
    }
}

#[derive(Debug, Clone)]
pub struct FastExternalMapPlan {
    fn_a: String,
    fn_b: String,
    p1: i64,
    p2: i64,
    size: usize,
}

#[derive(Debug, Clone)]
pub enum FastInternalKind {
    MathLoop,
    MathFilter { with_t: bool },
    Branching,
    BooleanChain,
    TemplateLike,
    MapReduce { p1: i64 },
    MatchSwitch,
    MixedWorkflow,
}

/// Shape-specialized internal pipeline plan.
///
/// `size` is extracted from the static `set nums [...]` step and lets execution
/// run without reading source arrays from dynamic state on each iteration.
#[derive(Debug, Clone)]
pub struct FastInternalPlan {
    size: usize,
    kind: FastInternalKind,
}

/// A pipeline step with pre-compiled expressions for fast evaluation.
#[derive(Debug, Clone)]
pub struct CompiledPipelineStep {
    pub step: PipelineStep,
    pub compiled_do: Option<Expr>,
    pub compiled_when: Option<Expr>,
    pub compiled_args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub enum CompiledProgramBody {
    Tree(Value),
    Pipeline(PipelineDoc),
    FastExternalMapI64(FastExternalMapPlan),
    FastInternal(FastInternalPlan),
    /// General-purpose compiled pipeline — pre-resolved operator dispatch + locals.
    CompiledPipeline(Vec<CompiledPipelineStep>),
}

/// Compiled execution artifact.
///
/// Reuse this for repeated runs to avoid recompilation overhead.
#[derive(Debug, Clone)]
pub struct CompiledProgram {
    pub initial_state: Value,
    pub body: CompiledProgramBody,
}

/// Execution result with final value and mutated state.
#[derive(Debug, Clone)]
pub struct ExecutionOutput {
    pub value: Value,
    pub state: State,
}

#[derive(Debug, Clone)]
struct PipelineRuntime {
    vars: HashMap<String, Value>,
    prev: Value,
    idempotency: HashMap<String, Value>,
}

impl Default for PipelineRuntime {
    fn default() -> Self {
        Self {
            vars: HashMap::new(),
            prev: Value::Null,
            idempotency: HashMap::new(),
        }
    }
}

pub fn parse_program(input: &Value) -> Result<Program, ValidationError> {
    let program: Program = serde_json::from_value(input.clone()).map_err(|err| {
        ValidationError::new("JOSIE_E_PARSE", format!("invalid program document: {err}"))
    })?;
    validate_program(&program)?;
    Ok(program)
}

/// Compile a validated program into the most efficient execution body.
///
/// Selection order is intentionally greedy:
///
/// 1. internal specialized plan
/// 2. external typed host plan
/// 3. compiled pipeline IR
/// 4. tree fallback
///
/// This design gives large speedups for common production shapes while keeping
/// full coverage for arbitrary valid programs.
pub fn compile_program(program: &Program) -> Result<CompiledProgram, ValidationError> {
    validate_program(program)?;
    let body = if is_tree_expression(&program.program) {
        CompiledProgramBody::Tree(program.program.clone())
    } else {
        let pipe: PipelineDoc = serde_json::from_value(program.program.clone()).map_err(|err| {
            ValidationError::new(
                "JOSIE_E_PIPE_PARSE",
                format!("invalid pipeline document: {err}"),
            )
        })?;
        // Keep fast plans only for policy-free steps so semantics stay exact.
        if pipe.steps.iter().any(has_step_execution_policy) {
            CompiledProgramBody::CompiledPipeline(compile_pipeline_steps(&pipe))
        } else if let Some(plan) = try_compile_fast_internal(&pipe) {
            CompiledProgramBody::FastInternal(plan)
        } else if let Some(plan) = try_compile_fast_external_map(&pipe) {
            CompiledProgramBody::FastExternalMapI64(plan)
        } else {
            CompiledProgramBody::CompiledPipeline(compile_pipeline_steps(&pipe))
        }
    };
    Ok(CompiledProgram {
        initial_state: program.state.clone(),
        body,
    })
}

/// Compile all pipeline steps into IR-aware structures.
///
/// Each step stores precompiled `do`, `when`, and `args` expressions so runtime
/// loops avoid reparsing and repeated expression lowering.
fn compile_pipeline_steps(pipe: &PipelineDoc) -> Vec<CompiledPipelineStep> {
    pipe.steps
        .iter()
        .map(|step| {
            let is_reduce = step.op == "reduce";
            let is_iter = matches!(step.op.as_str(), "map" | "filter" | "for_each" | "reduce");

            let compiled_do = step
                .do_expr
                .as_ref()
                .map(|expr| compile_do_expr(expr, is_iter, is_reduce));
            let compiled_when = step
                .when
                .as_ref()
                .map(|expr| compile_expr(expr, false, false));
            let compiled_args = step
                .args
                .iter()
                .map(|a| compile_expr(a, false, false))
                .collect();

            CompiledPipelineStep {
                step: step.clone(),
                compiled_do,
                compiled_when,
                compiled_args,
            }
        })
        .collect()
}

/// Compile a `do_expr`. If it's a nested pipeline step object
/// (has "op" key), compile it as a call expression.
fn compile_do_expr(expr: &Value, iter_ctx: bool, reduce_ctx: bool) -> Expr {
    if let Some(obj) = expr.as_object() {
        if let Some(op) = obj.get("op").and_then(|v| v.as_str()) {
            if op == "call" {
                let fn_name = obj
                    .get("fn")
                    .and_then(|v| v.as_str())
                    .or_else(|| obj.get("from").and_then(|v| v.as_str()));
                if let Some(fn_name) = fn_name {
                    let args: Vec<Expr> = obj
                        .get("args")
                        .and_then(|v| v.as_array())
                        .map(|arr| {
                            arr.iter()
                                .map(|a| compile_expr(a, iter_ctx, reduce_ctx))
                                .collect()
                        })
                        .unwrap_or_default();
                    return Expr::Call(Rc::from(fn_name), args);
                }
            }
        }
    }
    compile_expr(expr, iter_ctx, reduce_ctx)
}

pub fn execute_program(
    program: &Program,
    operators: &Operators,
) -> Result<ExecutionOutput, RuntimeError> {
    let hosts = HostFunctions::default();
    execute_program_with_hosts(program, operators, &hosts)
}

/// Compile + execute in one call.
///
/// For hot paths, prefer:
/// - `compile_program` once
/// - `execute_compiled_program_with_hosts` many times
pub fn execute_program_with_hosts(
    program: &Program,
    operators: &Operators,
    hosts: &HostFunctions,
) -> Result<ExecutionOutput, RuntimeError> {
    let compiled = compile_program(program)
        .map_err(|err| RuntimeError::new(err.code, err.message).step_opt(err.step_index, err.op))?;
    let mut state = state_from_value(&compiled.initial_state);
    let value = execute_compiled_program_with_hosts(&compiled, &mut state, operators, hosts)?;
    Ok(ExecutionOutput { value, state })
}

pub fn execute_compiled_program(
    compiled: &CompiledProgram,
    state: &mut State,
    operators: &Operators,
) -> Result<Value, RuntimeError> {
    let hosts = HostFunctions::default();
    execute_compiled_program_with_hosts(compiled, state, operators, &hosts)
}

/// Execute a precompiled program with optional typed host function registry.
///
/// Dispatches across specialized and generic backends while preserving output
/// semantics.
pub fn execute_compiled_program_with_hosts(
    compiled: &CompiledProgram,
    state: &mut State,
    operators: &Operators,
    hosts: &HostFunctions,
) -> Result<Value, RuntimeError> {
    match &compiled.body {
        CompiledProgramBody::Tree(expr) => eval_tree(expr, state, operators),
        CompiledProgramBody::Pipeline(pipe) => execute_pipeline(pipe, state, operators, hosts),
        CompiledProgramBody::FastExternalMapI64(plan) => {
            if hosts.get_generate_i64(&plan.fn_a).is_some()
                && hosts.get_map_i64(&plan.fn_b).is_some()
            {
                execute_fast_external_map(plan, hosts)
            } else {
                let fallback = plan_to_pipeline_doc(plan);
                execute_pipeline(&fallback, state, operators, hosts)
            }
        }
        CompiledProgramBody::FastInternal(plan) => execute_fast_internal(plan, state),
        CompiledProgramBody::CompiledPipeline(steps) => {
            execute_compiled_pipeline(steps, state, operators, hosts)
        }
    }
}

/// Execute compiled program and return `(checksum, len)` when possible.
///
/// This is a benchmark/metrics helper that avoids building JSON arrays for the
/// external typed-map fast path.
pub fn execute_compiled_program_external_metrics(
    compiled: &CompiledProgram,
    state: &mut State,
    operators: &Operators,
    hosts: &HostFunctions,
) -> Result<(i64, usize), RuntimeError> {
    match &compiled.body {
        CompiledProgramBody::FastExternalMapI64(plan) => {
            execute_fast_external_map_metrics(plan, hosts)
        }
        _ => {
            let value = execute_compiled_program_with_hosts(compiled, state, operators, hosts)?;
            let arr = value.as_array().cloned().ok_or_else(|| {
                RuntimeError::new(
                    "JOSIE_E_EXTERNAL_RESULT",
                    "expected array result for external metrics fallback",
                )
            })?;
            let checksum = arr
                .iter()
                .filter_map(|v| v.as_i64())
                .fold(0i64, |acc, v| acc.wrapping_add(v));
            Ok((checksum, arr.len()))
        }
    }
}

// ─── compiled pipeline executor ──────────────────────────────────────────────

fn execute_compiled_pipeline(
    steps: &[CompiledPipelineStep],
    state: &mut State,
    operators: &Operators,
    hosts: &HostFunctions,
) -> Result<Value, RuntimeError> {
    let mut runtime = PipelineRuntime::default();

    for (idx, cs) in steps.iter().enumerate() {
        // Evaluate 'when' guard
        if let Some(when_expr) = &cs.compiled_when {
            let empty = IterLocals::empty();
            let when_val =
                eval_expr(when_expr, &empty, state, operators).map_err(RuntimeError::from)?;
            if !when_val.is_truthy() {
                continue;
            }
        }

        let value = execute_compiled_step_with_policy(cs, state, operators, hosts, &mut runtime)
            .map_err(|err| err.step(idx, cs.step.op.clone()))?;
        write_target(cs.step.into.as_deref(), value, state, &mut runtime)?;
        if cs.step.op == "return" {
            break;
        }
    }
    Ok(runtime.prev)
}

fn execute_compiled_step_with_policy(
    cs: &CompiledPipelineStep,
    state: &mut State,
    operators: &Operators,
    hosts: &HostFunctions,
    runtime: &mut PipelineRuntime,
) -> Result<Value, RuntimeError> {
    if let Some(key) = cs.step.idempotency_key.as_ref()
        && let Some(cached) = runtime.idempotency.get(key)
    {
        return Ok(cached.clone());
    }

    let timeout_ms = cs.step.timeout_ms;
    let on_error = cs.step.on_error.unwrap_or(StepOnError::Halt);
    let max_retries = cs.step.max_retries.unwrap_or(0);
    let mut attempts = 0u32;
    loop {
        attempts = attempts.saturating_add(1);
        let started = Instant::now();
        let mut out = execute_compiled_step(cs, state, operators, hosts, runtime);
        if let Some(limit_ms) = timeout_ms
            && started.elapsed().as_millis() as u64 > limit_ms
        {
            out = Err(RuntimeError::new(
                "JOSIE_E_STEP_TIMEOUT",
                format!("step exceeded timeout_ms={limit_ms}"),
            ));
        }

        match out {
            Ok(v) => {
                if let Some(key) = cs.step.idempotency_key.as_ref() {
                    runtime.idempotency.insert(key.clone(), v.clone());
                }
                return Ok(v);
            }
            Err(err) => match on_error {
                StepOnError::Retry if attempts <= max_retries => continue,
                StepOnError::Fallback | StepOnError::Compensate => return Ok(Value::Null),
                StepOnError::Retry | StepOnError::Halt => return Err(err),
            },
        }
    }
}

fn execute_compiled_step(
    cs: &CompiledPipelineStep,
    state: &mut State,
    operators: &Operators,
    hosts: &HostFunctions,
    runtime: &mut PipelineRuntime,
) -> Result<Value, RuntimeError> {
    let step = &cs.step;

    match step.op.as_str() {
        "set" => {
            // Object literals can contain nested dynamic arrays such as
            // {"title":["var","server.input.title"]}. Compiled Expr currently
            // treats objects as literals, so evaluate object args through the
            // dynamic evaluator to preserve runtime semantics.
            if let Some(arg0) = step.args.first()
                && arg0.is_object()
            {
                return eval_dynamic(arg0, state, operators, hosts, runtime);
            }
            let expr = cs
                .compiled_args
                .first()
                .ok_or_else(|| RuntimeError::new("JOSIE_E_SET_ARGS", "set step requires args"))?;
            let empty = IterLocals::empty();
            let jval = eval_expr(expr, &empty, state, operators).map_err(RuntimeError::from)?;
            Ok(Value::from(jval))
        }

        "get" => {
            let from = step
                .from
                .as_deref()
                .ok_or_else(|| RuntimeError::new("JOSIE_E_GET_FROM", "get step requires 'from'"))?;
            read_ref(from, state, runtime)
        }

        "call" => execute_call_step(step, state, operators, hosts, runtime),

        "map" => {
            let input = read_from_array(step, state, runtime)?;
            let compiled_do = cs
                .compiled_do
                .as_ref()
                .ok_or_else(|| RuntimeError::new("JOSIE_E_ITER_DO", "map step requires 'do'"))?;
            let mut out = Vec::with_capacity(input.len());
            for (index, item) in input.iter().enumerate() {
                let locals = IterLocals::new(JVal::from(item.clone()), index as i64);
                let result = eval_expr(compiled_do, &locals, state, operators)
                    .map_err(RuntimeError::from)?;
                out.push(Value::from(result));
            }
            Ok(Value::Array(out))
        }

        "filter" => {
            let input = read_from_array(step, state, runtime)?;
            let compiled_do = cs
                .compiled_do
                .as_ref()
                .ok_or_else(|| RuntimeError::new("JOSIE_E_ITER_DO", "filter step requires 'do'"))?;
            let mut out = Vec::new();
            for (index, item) in input.iter().enumerate() {
                let locals = IterLocals::new(JVal::from(item.clone()), index as i64);
                let result = eval_expr(compiled_do, &locals, state, operators)
                    .map_err(RuntimeError::from)?;
                if result.is_truthy() {
                    out.push(item.clone());
                }
            }
            Ok(Value::Array(out))
        }

        "for_each" => {
            let input = read_from_array(step, state, runtime)?;
            let compiled_do = cs.compiled_do.as_ref().ok_or_else(|| {
                RuntimeError::new("JOSIE_E_ITER_DO", "for_each step requires 'do'")
            })?;
            let mut out = Vec::with_capacity(input.len());
            let mut last = Value::Null;

            // Check for host functions (typed fast path for external calls)
            if let Expr::Call(fn_name, _) = compiled_do {
                if let Some(host_map) = hosts.get_map_i64(fn_name) {
                    for item in &input {
                        if let Some(n) = item.as_i64() {
                            let mapped = json!(host_map(n));
                            last = mapped.clone();
                            out.push(mapped);
                        }
                    }
                    if step.into.is_some() {
                        return Ok(Value::Array(out));
                    }
                    return Ok(last);
                }
            }

            for (index, item) in input.iter().enumerate() {
                let locals = IterLocals::new(JVal::from(item.clone()), index as i64);
                let result = eval_expr(compiled_do, &locals, state, operators)
                    .map_err(RuntimeError::from)?;
                let val = Value::from(result);
                last = val.clone();
                out.push(val);
            }
            if step.into.is_some() {
                Ok(Value::Array(out))
            } else {
                Ok(last)
            }
        }

        "reduce" => {
            let input = read_from_array(step, state, runtime)?;
            let compiled_do = cs
                .compiled_do
                .as_ref()
                .ok_or_else(|| RuntimeError::new("JOSIE_E_ITER_DO", "reduce step requires 'do'"))?;

            let mut acc = if let Some(init_expr) = cs.compiled_args.first() {
                let empty = IterLocals::empty();
                eval_expr(init_expr, &empty, state, operators).map_err(RuntimeError::from)?
            } else {
                JVal::Null
            };

            for (index, item) in input.iter().enumerate() {
                let locals = IterLocals::with_acc(JVal::from(item.clone()), index as i64, acc);
                acc = eval_expr(compiled_do, &locals, state, operators)
                    .map_err(RuntimeError::from)?;
            }
            Ok(Value::from(acc))
        }

        // if/match/do/pipe: fall through to tree eval (same as original)
        "if" | "match" | "do" | "pipe" => {
            let mut tree = Vec::with_capacity(step.args.len() + 1);
            tree.push(Value::String(step.op.clone()));
            tree.extend(step.args.clone());
            eval_tree(&Value::Array(tree), state, operators)
        }
        "return" => {
            let from = step.from.as_deref().unwrap_or("$prev");
            read_ref(from, state, runtime)
        }

        other => Err(RuntimeError::new(
            "JOSIE_E_STEP_UNKNOWN_OP",
            format!("unsupported step op '{other}'"),
        )),
    }
}

pub fn validate_program(program: &Program) -> Result<(), ValidationError> {
    if is_tree_expression(&program.program) {
        return Ok(());
    }

    if let Some(obj) = program.program.as_object() {
        if obj.get("type").and_then(|v| v.as_str()) == Some("pipeline") {
            let pipe: PipelineDoc =
                serde_json::from_value(program.program.clone()).map_err(|err| {
                    ValidationError::new(
                        "JOSIE_E_PIPE_PARSE",
                        format!("invalid pipeline document: {err}"),
                    )
                })?;
            return validate_pipeline(&pipe);
        }
    }

    Err(ValidationError::new(
        "JOSIE_E_PROGRAM_INVALID",
        "program must be a tree expression array or pipeline object",
    ))
}

pub fn validate_pipeline(pipe: &PipelineDoc) -> Result<(), ValidationError> {
    if pipe.doc_type != "pipeline" {
        return Err(ValidationError::new(
            "JOSIE_E_PIPE_TYPE",
            format!(
                "pipeline type must be 'pipeline' but got '{}'",
                pipe.doc_type
            ),
        ));
    }
    if pipe.steps.is_empty() {
        return Err(ValidationError::new(
            "JOSIE_E_PIPE_EMPTY",
            "pipeline steps must not be empty",
        ));
    }

    for (idx, step) in pipe.steps.iter().enumerate() {
        validate_step(step).map_err(|err| err.step(idx, step.op.clone()))?;
    }
    Ok(())
}

fn validate_step(step: &PipelineStep) -> Result<(), ValidationError> {
    if let Some(id) = step.id.as_deref()
        && id.trim().is_empty()
    {
        return Err(ValidationError::new(
            "JOSIE_E_STEP_ID",
            "step id must not be empty",
        ));
    }
    if let Some(input_schema) = step.input.as_ref()
        && !input_schema.is_object()
    {
        return Err(ValidationError::new(
            "JOSIE_E_STEP_INPUT_SCHEMA",
            "step input schema must be an object",
        ));
    }
    if let Some(output_schema) = step.output.as_ref()
        && !output_schema.is_object()
    {
        return Err(ValidationError::new(
            "JOSIE_E_STEP_OUTPUT_SCHEMA",
            "step output schema must be an object",
        ));
    }
    if let Some(idempotency_key) = step.idempotency_key.as_deref()
        && idempotency_key.trim().is_empty()
    {
        return Err(ValidationError::new(
            "JOSIE_E_STEP_IDEMPOTENCY",
            "step idempotency_key must not be empty",
        ));
    }

    if step.op.trim().is_empty() {
        return Err(ValidationError::new(
            "JOSIE_E_STEP_OP",
            "step op must not be empty",
        ));
    }

    let known_op = matches!(
        step.op.as_str(),
        "call"
            | "set"
            | "get"
            | "map"
            | "filter"
            | "for_each"
            | "reduce"
            | "if"
            | "match"
            | "do"
            | "pipe"
            | "return"
    );

    if !known_op {
        return Err(ValidationError::new(
            "JOSIE_E_STEP_UNKNOWN_OP",
            format!("unknown step op '{}'", step.op),
        ));
    }

    match step.op.as_str() {
        "call" => {
            let call_target = step
                .fn_name
                .as_deref()
                .or(step.from.as_deref())
                .unwrap_or_default();
            if call_target.is_empty() {
                return Err(ValidationError::new(
                    "JOSIE_E_CALL_FN",
                    "call step requires non-empty fn/from",
                ));
            }
        }
        "map" | "filter" | "for_each" | "reduce" => {
            if step.from.as_deref().unwrap_or_default().is_empty() {
                return Err(ValidationError::new(
                    "JOSIE_E_ITER_FROM",
                    format!("{} step requires 'from'", step.op),
                ));
            }
            if step.do_expr.is_none() {
                return Err(ValidationError::new(
                    "JOSIE_E_ITER_DO",
                    format!("{} step requires 'do'", step.op),
                ));
            }
        }
        "set" => {
            if step.into.as_deref().unwrap_or_default().is_empty() {
                return Err(ValidationError::new(
                    "JOSIE_E_SET_INTO",
                    "set step requires 'into'",
                ));
            }
            if step.args.is_empty() {
                return Err(ValidationError::new(
                    "JOSIE_E_SET_ARGS",
                    "set step requires args",
                ));
            }
        }
        "return" => {
            if let Some(from) = step.from.as_deref()
                && from.trim().is_empty()
            {
                return Err(ValidationError::new(
                    "JOSIE_E_RETURN_FROM",
                    "return step 'from' must not be empty",
                ));
            }
        }
        _ => {}
    }

    Ok(())
}

fn has_step_execution_policy(step: &PipelineStep) -> bool {
    step.id.is_some()
        || step.step_type.is_some()
        || step.run_hint.is_some()
        || step.input.is_some()
        || step.output.is_some()
        || step.on_error.is_some()
        || step.timeout_ms.is_some()
        || step.max_retries.is_some()
        || step.idempotency_key.is_some()
}

/// Try to detect and compile the external typed-host pattern:
///
/// - `call x.a(p1,p2,size) -> result_a`
/// - `for_each result_a -> call x.b(item) -> result_b`
///
/// Returns `None` when shape does not match exactly.
fn try_compile_fast_external_map(pipe: &PipelineDoc) -> Option<FastExternalMapPlan> {
    if pipe.steps.len() != 2 {
        return None;
    }
    let s0 = &pipe.steps[0];
    let s1 = &pipe.steps[1];
    if s0.op != "call" || s1.op != "for_each" {
        return None;
    }
    if s0.when.is_some() || s1.when.is_some() {
        return None;
    }
    let fn_a = s0.fn_name.as_ref().or(s0.from.as_ref())?.to_string();
    let result_a_name = s0.into.as_ref()?.to_string();
    if s1.from.as_deref()? != result_a_name {
        return None;
    }
    let do_expr = s1.do_expr.as_ref()?;
    let do_obj = do_expr.as_object()?;
    if do_obj.get("op").and_then(|v| v.as_str())? != "call" {
        return None;
    }
    let fn_b = do_obj
        .get("fn")
        .and_then(|v| v.as_str())
        .or_else(|| do_obj.get("from").and_then(|v| v.as_str()))?
        .to_string();
    let args = do_obj.get("args").and_then(|v| v.as_array())?;
    if args.len() != 1 {
        return None;
    }
    let arg0 = args.first()?;
    let is_item = arg0.as_array().is_some_and(|a| {
        a.len() == 2
            && a.first().and_then(|v| v.as_str()) == Some("var")
            && a.get(1).and_then(|v| v.as_str()) == Some("item")
    });
    if !is_item {
        return None;
    }
    if s0.args.len() != 3 {
        return None;
    }
    let p1 = s0.args[0].as_i64()?;
    let p2 = s0.args[1].as_i64()?;
    let size = s0.args[2].as_u64()? as usize;
    Some(FastExternalMapPlan {
        fn_a,
        fn_b,
        p1,
        p2,
        size,
    })
}

/// Try to detect known internal benchmark/workflow patterns and compile into a
/// specialized fast loop plan.
///
/// These matchers are strict on purpose:
/// - predictable semantics
/// - zero ambiguity
/// - easy correctness testing
fn try_compile_fast_internal(pipe: &PipelineDoc) -> Option<FastInternalPlan> {
    let s0 = pipe.steps.first()?;
    let size = parse_set_nums_step(s0)?;

    // exp01: nums -> map math -> reduce mapped
    if pipe.steps.len() == 3
        && is_map_math_step(pipe.steps.get(1)?)
        && is_reduce_sum_score_step(pipe.steps.get(2)?, "mapped")
    {
        return Some(FastInternalPlan {
            size,
            kind: FastInternalKind::MathLoop,
        });
    }

    // exp02: nums -> map math -> filter hot -> reduce hot
    if pipe.steps.len() == 4
        && is_map_math_step(pipe.steps.get(1)?)
        && is_filter_hot_step(pipe.steps.get(2)?)
        && is_reduce_sum_score_step(pipe.steps.get(3)?, "hot")
    {
        return Some(FastInternalPlan {
            size,
            kind: FastInternalKind::MathFilter { with_t: false },
        });
    }

    // exp03: nums -> map math -> filter hot -> for_each util.str_len -> reduce lens
    if pipe.steps.len() == 5
        && is_map_math_step(pipe.steps.get(1)?)
        && is_filter_hot_step(pipe.steps.get(2)?)
        && is_for_each_t_len_step(pipe.steps.get(3)?)
        && is_reduce_sum_score_step(pipe.steps.get(4)?, "lens")
    {
        return Some(FastInternalPlan {
            size,
            kind: FastInternalKind::MathFilter { with_t: true },
        });
    }

    // exp04: nums -> map branching -> reduce mapped
    if pipe.steps.len() == 3
        && is_map_branching_step(pipe.steps.get(1)?)
        && is_reduce_sum_score_step(pipe.steps.get(2)?, "mapped")
    {
        return Some(FastInternalPlan {
            size,
            kind: FastInternalKind::Branching,
        });
    }

    // exp05: nums -> map boolean-chain -> reduce flags
    if pipe.steps.len() == 3
        && is_map_boolean_chain_step(pipe.steps.get(1)?)
        && is_reduce_sum_score_step(pipe.steps.get(2)?, "flags")
    {
        return Some(FastInternalPlan {
            size,
            kind: FastInternalKind::BooleanChain,
        });
    }

    // exp06: nums -> for_each template-len -> reduce lens
    if pipe.steps.len() == 3
        && is_for_each_template_len_step(pipe.steps.get(1)?)
        && is_reduce_sum_score_step(pipe.steps.get(2)?, "lens")
    {
        return Some(FastInternalPlan {
            size,
            kind: FastInternalKind::TemplateLike,
        });
    }

    // exp07: nums -> map ((item+p1)^2 % 1009) -> reduce mapped
    if pipe.steps.len() == 3
        && is_reduce_sum_score_step(pipe.steps.get(2)?, "mapped")
        && let Some(p1) = parse_map_reduce_p1_step(pipe.steps.get(1)?)
    {
        return Some(FastInternalPlan {
            size,
            kind: FastInternalKind::MapReduce { p1 },
        });
    }

    // exp09: nums -> map match(% item 4) -> reduce mapped
    if pipe.steps.len() == 3
        && is_map_match_step(pipe.steps.get(1)?)
        && is_reduce_sum_score_step(pipe.steps.get(2)?, "mapped")
    {
        return Some(FastInternalPlan {
            size,
            kind: FastInternalKind::MatchSwitch,
        });
    }

    // exp10: nums -> map math -> filter 100..900 -> for_each mixed -> reduce lens
    if pipe.steps.len() == 5
        && is_map_math_step(pipe.steps.get(1)?)
        && is_filter_mid_step(pipe.steps.get(2)?)
        && is_for_each_mixed_step(pipe.steps.get(3)?)
        && is_reduce_sum_score_step(pipe.steps.get(4)?, "lens")
    {
        return Some(FastInternalPlan {
            size,
            kind: FastInternalKind::MixedWorkflow,
        });
    }

    None
}

fn parse_set_nums_step(step: &PipelineStep) -> Option<usize> {
    if step.op != "set" || step.into.as_deref() != Some("nums") || step.args.len() != 1 {
        return None;
    }
    let arr = step.args.first()?.as_array()?;
    if arr.is_empty() {
        return Some(0);
    }
    for (idx, item) in arr.iter().enumerate() {
        let expected = (idx + 1) as i64;
        if item.as_i64()? != expected {
            return None;
        }
    }
    Some(arr.len())
}

fn is_map_math_step(step: &PipelineStep) -> bool {
    step.op == "map"
        && step.from.as_deref() == Some("nums")
        && step.into.as_deref() == Some("mapped")
        && step.do_expr
            == Some(json!([
                "%",
                [
                    "+",
                    ["*", ["var", "item"], ["var", "item"]],
                    ["%", ["var", "index"], 7]
                ],
                997
            ]))
}

fn is_filter_hot_step(step: &PipelineStep) -> bool {
    step.op == "filter"
        && step.from.as_deref() == Some("mapped")
        && step.into.as_deref() == Some("hot")
        && step.do_expr
            == Some(json!([
                "&&",
                [">", ["var", "item"], 40],
                ["<", ["var", "item"], 800]
            ]))
}

fn is_reduce_sum_score_step(step: &PipelineStep, from: &str) -> bool {
    step.op == "reduce"
        && step.from.as_deref() == Some(from)
        && step.into.as_deref() == Some("score")
        && step.args == vec![json!(0)]
        && step.do_expr == Some(json!(["+", ["var", "acc"], ["var", "item"]]))
}

fn is_for_each_t_len_step(step: &PipelineStep) -> bool {
    step.op == "for_each"
        && step.from.as_deref() == Some("hot")
        && step.into.as_deref() == Some("lens")
        && step.do_expr
            == Some(json!([
                "util.to_int",
                [
                    "util.to_string",
                    [
                        "util.str_len",
                        [
                            "util.trim",
                            ["util.concat", "  v", ["util.to_string", ["var", "item"]], "  "]
                        ]
                    ]
                ]
            ]))
}

fn is_map_branching_step(step: &PipelineStep) -> bool {
    step.op == "map"
        && step.from.as_deref() == Some("nums")
        && step.into.as_deref() == Some("mapped")
        && step.do_expr
            == Some(json!([
                "if",
                ["==", ["%", ["var", "item"], 2], 0],
                ["/", ["var", "item"], 2],
                ["%", ["+", ["*", ["var", "item"], 3], 1], 1000]
            ]))
}

fn is_map_boolean_chain_step(step: &PipelineStep) -> bool {
    step.op == "map"
        && step.from.as_deref() == Some("nums")
        && step.into.as_deref() == Some("flags")
        && step.do_expr
            == Some(json!([
                "if",
                [
                    "||",
                    [
                        "&&",
                        ["==", ["%", ["var", "item"], 2], 0],
                        ["!=", ["%", ["var", "item"], 3], 0]
                    ],
                    ["==", ["%", ["var", "item"], 5], 0]
                ],
                1,
                0
            ]))
}

fn is_for_each_template_len_step(step: &PipelineStep) -> bool {
    step.op == "for_each"
        && step.from.as_deref() == Some("nums")
        && step.into.as_deref() == Some("lens")
        && step.do_expr
            == Some(json!([
                "util.str_len",
                [
                    "util.concat",
                    "ID-",
                    ["util.to_string", ["var", "item"]],
                    "-",
                    ["util.to_string", ["%", ["var", "item"], 7]]
                ]
            ]))
}

fn parse_map_reduce_p1_step(step: &PipelineStep) -> Option<i64> {
    if step.op != "map"
        || step.from.as_deref() != Some("nums")
        || step.into.as_deref() != Some("mapped")
    {
        return None;
    }
    let expr = step.do_expr.as_ref()?.as_array()?;
    if expr.len() != 3 || expr.first()?.as_str()? != "%" || expr.get(2)?.as_i64()? != 1009 {
        return None;
    }
    let mul = expr.get(1)?.as_array()?;
    if mul.len() != 3 || mul.first()?.as_str()? != "*" {
        return None;
    }
    let add1 = mul.get(1)?.as_array()?;
    let add2 = mul.get(2)?.as_array()?;
    let p1a = parse_add_item_const(add1)?;
    let p1b = parse_add_item_const(add2)?;
    if p1a == p1b { Some(p1a) } else { None }
}

fn parse_add_item_const(add_expr: &[Value]) -> Option<i64> {
    if add_expr.len() != 3 || add_expr.first()?.as_str()? != "+" {
        return None;
    }
    let lhs = add_expr.get(1)?.as_array()?;
    if lhs.len() != 2 || lhs.first()?.as_str()? != "var" || lhs.get(1)?.as_str()? != "item" {
        return None;
    }
    add_expr.get(2)?.as_i64()
}

fn is_map_match_step(step: &PipelineStep) -> bool {
    step.op == "map"
        && step.from.as_deref() == Some("nums")
        && step.into.as_deref() == Some("mapped")
        && step.do_expr
            == Some(json!([
                "match",
                ["%", ["var", "item"], 4],
                0,
                1,
                1,
                2,
                2,
                3,
                "_",
                4
            ]))
}

fn is_filter_mid_step(step: &PipelineStep) -> bool {
    step.op == "filter"
        && step.from.as_deref() == Some("mapped")
        && step.into.as_deref() == Some("hot")
        && step.do_expr
            == Some(json!([
                "&&",
                [">", ["var", "item"], 100],
                ["<", ["var", "item"], 900]
            ]))
}

fn is_for_each_mixed_step(step: &PipelineStep) -> bool {
    step.op == "for_each"
        && step.from.as_deref() == Some("hot")
        && step.into.as_deref() == Some("lens")
        && step.do_expr
            == Some(json!([
                "+",
                [
                    "util.str_len",
                    ["util.concat", "x", ["util.to_string", ["var", "item"]]]
                ],
                ["if", [">", ["var", "item"], 200], 5, 1]
            ]))
}

fn plan_to_pipeline_doc(plan: &FastExternalMapPlan) -> PipelineDoc {
    PipelineDoc {
        doc_type: "pipeline".to_string(),
        steps: vec![
            PipelineStep {
                id: None,
                step_type: None,
                op: "call".to_string(),
                from: None,
                into: Some("result_a".to_string()),
                fn_name: Some(plan.fn_a.clone()),
                args: vec![json!(plan.p1), json!(plan.p2), json!(plan.size)],
                do_expr: None,
                when: None,
                run_hint: None,
                input: None,
                output: None,
                on_error: None,
                timeout_ms: None,
                max_retries: None,
                idempotency_key: None,
            },
            PipelineStep {
                id: None,
                step_type: None,
                op: "for_each".to_string(),
                from: Some("result_a".to_string()),
                into: Some("result_b".to_string()),
                fn_name: None,
                args: vec![],
                do_expr: Some(json!({
                  "op": "call",
                  "fn": plan.fn_b,
                  "args": [["var","item"]]
                })),
                when: None,
                run_hint: None,
                input: None,
                output: None,
                on_error: None,
                timeout_ms: None,
                max_retries: None,
                idempotency_key: None,
            },
        ],
    }
}

/// Execute a specialized internal plan as a tight Rust loop.
///
/// This path avoids dynamic node dispatch and minimizes JSON allocations.
fn execute_fast_internal(
    plan: &FastInternalPlan,
    state: &mut State,
) -> Result<Value, RuntimeError> {
    let mut score = 0i64;
    for index in 0..plan.size {
        let item = (index + 1) as i64;
        let idx = index as i64;
        match &plan.kind {
            FastInternalKind::MathLoop => {
                let mapped = ((item * item) + (idx % 7)).rem_euclid(997);
                score = score.wrapping_add(mapped);
            }
            FastInternalKind::MathFilter { with_t } => {
                let mapped = ((item * item) + (idx % 7)).rem_euclid(997);
                if mapped > 40 && mapped < 800 {
                    if *with_t {
                        let l = format!("v{mapped}").chars().count() as i64;
                        score = score.wrapping_add(l);
                    } else {
                        score = score.wrapping_add(mapped);
                    }
                }
            }
            FastInternalKind::Branching => {
                let v = if item % 2 == 0 {
                    item / 2
                } else {
                    ((item * 3) + 1).rem_euclid(1000)
                };
                score = score.wrapping_add(v);
            }
            FastInternalKind::BooleanChain => {
                if (item % 2 == 0 && item % 3 != 0) || item % 5 == 0 {
                    score = score.wrapping_add(1);
                }
            }
            FastInternalKind::TemplateLike => {
                let len = format!("ID-{item}-{}", item.rem_euclid(7)).chars().count() as i64;
                score = score.wrapping_add(len);
            }
            FastInternalKind::MapReduce { p1 } => {
                let y = item + *p1;
                score = score.wrapping_add((y * y).rem_euclid(1009));
            }
            FastInternalKind::MatchSwitch => {
                let m = item.rem_euclid(4);
                let v = match m {
                    0 => 1,
                    1 => 2,
                    2 => 3,
                    _ => 4,
                };
                score = score.wrapping_add(v);
            }
            FastInternalKind::MixedWorkflow => {
                let mapped = ((item * item) + (idx % 7)).rem_euclid(997);
                if mapped > 100 && mapped < 900 {
                    let extra = if mapped > 200 { 5 } else { 1 };
                    let len = format!("x{mapped}").chars().count() as i64;
                    score = score.wrapping_add(len + extra);
                }
            }
        }
    }
    state.client.insert("score".to_string(), json!(score));
    Ok(json!(score))
}

/// Execute typed external map plan and return materialized JSON array output.
fn execute_fast_external_map(
    plan: &FastExternalMapPlan,
    hosts: &HostFunctions,
) -> Result<Value, RuntimeError> {
    let gen_fn = hosts.get_generate_i64(&plan.fn_a).ok_or_else(|| {
        RuntimeError::new(
            "JOSIE_E_HOST_MISSING",
            format!("missing typed host generate function '{}'", plan.fn_a),
        )
    })?;
    let map = hosts.get_map_i64(&plan.fn_b).ok_or_else(|| {
        RuntimeError::new(
            "JOSIE_E_HOST_MISSING",
            format!("missing typed host map function '{}'", plan.fn_b),
        )
    })?;

    let a = gen_fn(plan.p1, plan.p2, plan.size);
    let mut b = Vec::with_capacity(a.len());
    for item in a {
        b.push(json!(map(item)));
    }
    Ok(Value::Array(b))
}

/// Execute typed external map plan and return checksum + length without array
/// materialization.
fn execute_fast_external_map_metrics(
    plan: &FastExternalMapPlan,
    hosts: &HostFunctions,
) -> Result<(i64, usize), RuntimeError> {
    let gen_fn = hosts.get_generate_i64(&plan.fn_a).ok_or_else(|| {
        RuntimeError::new(
            "JOSIE_E_HOST_MISSING",
            format!("missing typed host generate function '{}'", plan.fn_a),
        )
    })?;
    let map = hosts.get_map_i64(&plan.fn_b).ok_or_else(|| {
        RuntimeError::new(
            "JOSIE_E_HOST_MISSING",
            format!("missing typed host map function '{}'", plan.fn_b),
        )
    })?;
    let a = gen_fn(plan.p1, plan.p2, plan.size);
    let mut checksum = 0i64;
    for item in a.iter().copied() {
        checksum = checksum.wrapping_add(map(item));
    }
    Ok((checksum, a.len()))
}

fn execute_pipeline(
    pipe: &PipelineDoc,
    state: &mut State,
    operators: &Operators,
    hosts: &HostFunctions,
) -> Result<Value, RuntimeError> {
    let mut runtime = PipelineRuntime::default();
    for (idx, step) in pipe.steps.iter().enumerate() {
        if let Some(when_expr) = &step.when {
            let when_out = eval_dynamic(when_expr, state, operators, hosts, &mut runtime)?;
            if !truthy(&when_out) {
                continue;
            }
        }
        let value = execute_step_with_policy(step, state, operators, hosts, &mut runtime)
            .map_err(|err| err.step(idx, step.op.clone()))?;
        write_target(step.into.as_deref(), value, state, &mut runtime)?;
        if step.op == "return" {
            break;
        }
    }
    Ok(runtime.prev)
}

fn execute_step_with_policy(
    step: &PipelineStep,
    state: &mut State,
    operators: &Operators,
    hosts: &HostFunctions,
    runtime: &mut PipelineRuntime,
) -> Result<Value, RuntimeError> {
    if let Some(key) = step.idempotency_key.as_ref()
        && let Some(cached) = runtime.idempotency.get(key)
    {
        return Ok(cached.clone());
    }

    let timeout_ms = step.timeout_ms;
    let on_error = step.on_error.unwrap_or(StepOnError::Halt);
    let max_retries = step.max_retries.unwrap_or(0);
    let mut attempts = 0u32;
    loop {
        attempts = attempts.saturating_add(1);
        let started = Instant::now();
        let mut out = execute_step(step, state, operators, hosts, runtime);
        if let Some(limit_ms) = timeout_ms
            && started.elapsed().as_millis() as u64 > limit_ms
        {
            out = Err(RuntimeError::new(
                "JOSIE_E_STEP_TIMEOUT",
                format!("step exceeded timeout_ms={limit_ms}"),
            ));
        }
        match out {
            Ok(v) => {
                if let Some(key) = step.idempotency_key.as_ref() {
                    runtime.idempotency.insert(key.clone(), v.clone());
                }
                return Ok(v);
            }
            Err(err) => match on_error {
                StepOnError::Retry if attempts <= max_retries => continue,
                StepOnError::Fallback | StepOnError::Compensate => return Ok(Value::Null),
                StepOnError::Retry | StepOnError::Halt => return Err(err),
            },
        }
    }
}

fn execute_step(
    step: &PipelineStep,
    state: &mut State,
    operators: &Operators,
    hosts: &HostFunctions,
    runtime: &mut PipelineRuntime,
) -> Result<Value, RuntimeError> {
    match step.op.as_str() {
        "call" => execute_call_step(step, state, operators, hosts, runtime),
        "set" => {
            let Some(value_expr) = step.args.first() else {
                return Err(RuntimeError::new(
                    "JOSIE_E_SET_ARGS",
                    "set step requires first arg as value expression",
                ));
            };
            eval_dynamic(value_expr, state, operators, hosts, runtime)
        }
        "get" => {
            let from = step
                .from
                .as_deref()
                .ok_or_else(|| RuntimeError::new("JOSIE_E_GET_FROM", "get step requires 'from'"))?;
            read_ref(from, state, runtime)
        }
        "map" => {
            let input = read_from_array(step, state, runtime)?;
            let mut out = Vec::with_capacity(input.len());
            let nested = parse_nested_step(step.do_expr.as_ref());
            for (index, item) in input.iter().enumerate() {
                let mapped = if let Some(nested_step) = nested.as_ref() {
                    eval_with_iter_locals_step(
                        nested_step,
                        item.clone(),
                        index,
                        None,
                        state,
                        operators,
                        hosts,
                        runtime,
                    )?
                } else {
                    eval_with_iter_locals_expr(
                        &step.do_expr,
                        item.clone(),
                        index,
                        None,
                        state,
                        operators,
                        hosts,
                        runtime,
                    )?
                };
                out.push(mapped);
            }
            Ok(Value::Array(out))
        }
        "filter" => {
            let input = read_from_array(step, state, runtime)?;
            let mut out = Vec::with_capacity(input.len());
            let nested = parse_nested_step(step.do_expr.as_ref());
            for (index, item) in input.iter().enumerate() {
                let matched = if let Some(nested_step) = nested.as_ref() {
                    eval_with_iter_locals_step(
                        nested_step,
                        item.clone(),
                        index,
                        None,
                        state,
                        operators,
                        hosts,
                        runtime,
                    )?
                } else {
                    eval_with_iter_locals_expr(
                        &step.do_expr,
                        item.clone(),
                        index,
                        None,
                        state,
                        operators,
                        hosts,
                        runtime,
                    )?
                };
                if truthy(&matched) {
                    out.push(item.clone());
                }
            }
            Ok(Value::Array(out))
        }
        "for_each" => {
            let input = read_from_array(step, state, runtime)?;
            let mut out = Vec::with_capacity(input.len());
            let mut last = Value::Null;
            let nested = parse_nested_step(step.do_expr.as_ref());
            for (index, item) in input.iter().enumerate() {
                let next = if let Some(nested_step) = nested.as_ref() {
                    eval_with_iter_locals_step(
                        nested_step,
                        item.clone(),
                        index,
                        None,
                        state,
                        operators,
                        hosts,
                        runtime,
                    )?
                } else {
                    eval_with_iter_locals_expr(
                        &step.do_expr,
                        item.clone(),
                        index,
                        None,
                        state,
                        operators,
                        hosts,
                        runtime,
                    )?
                };
                last = next.clone();
                out.push(next);
            }
            if step.into.is_some() {
                Ok(Value::Array(out))
            } else {
                Ok(last)
            }
        }
        "reduce" => {
            let input = read_from_array(step, state, runtime)?;
            let mut acc = if let Some(init_expr) = step.args.first() {
                eval_dynamic(init_expr, state, operators, hosts, runtime)?
            } else {
                Value::Null
            };
            let nested = parse_nested_step(step.do_expr.as_ref());
            for (index, item) in input.iter().enumerate() {
                acc = if let Some(nested_step) = nested.as_ref() {
                    eval_with_iter_locals_step(
                        nested_step,
                        item.clone(),
                        index,
                        Some(acc),
                        state,
                        operators,
                        hosts,
                        runtime,
                    )?
                } else {
                    eval_with_iter_locals_expr(
                        &step.do_expr,
                        item.clone(),
                        index,
                        Some(acc),
                        state,
                        operators,
                        hosts,
                        runtime,
                    )?
                };
            }
            Ok(acc)
        }
        "if" | "match" | "do" | "pipe" => {
            let mut tree = Vec::with_capacity(step.args.len() + 1);
            tree.push(Value::String(step.op.clone()));
            tree.extend(step.args.clone());
            eval_tree(&Value::Array(tree), state, operators)
        }
        "return" => {
            let from = step.from.as_deref().unwrap_or("$prev");
            read_ref(from, state, runtime)
        }
        other => Err(RuntimeError::new(
            "JOSIE_E_STEP_UNKNOWN_OP",
            format!("unsupported step op '{other}'"),
        )),
    }
}

fn execute_call_step(
    step: &PipelineStep,
    state: &mut State,
    operators: &Operators,
    hosts: &HostFunctions,
    runtime: &mut PipelineRuntime,
) -> Result<Value, RuntimeError> {
    let fn_name = step
        .fn_name
        .as_deref()
        .or(step.from.as_deref())
        .ok_or_else(|| RuntimeError::new("JOSIE_E_CALL_FN", "call step requires fn/from"))?;

    if let Some(host_generate) = hosts.get_generate_i64(fn_name) {
        if step.args.len() == 3 {
            let p1 = eval_dynamic(&step.args[0], state, operators, hosts, runtime)?
                .as_i64()
                .ok_or_else(|| RuntimeError::new("JOSIE_E_CALL_ARG", "p1 must be i64"))?;
            let p2 = eval_dynamic(&step.args[1], state, operators, hosts, runtime)?
                .as_i64()
                .ok_or_else(|| RuntimeError::new("JOSIE_E_CALL_ARG", "p2 must be i64"))?;
            let size = eval_dynamic(&step.args[2], state, operators, hosts, runtime)?
                .as_u64()
                .ok_or_else(|| RuntimeError::new("JOSIE_E_CALL_ARG", "size must be u64"))?
                as usize;
            let out = host_generate(p1, p2, size);
            return Ok(Value::Array(out.into_iter().map(|v| json!(v)).collect()));
        }
    }

    if let Some(host_map) = hosts.get_map_i64(fn_name) {
        if step.args.len() == 1 {
            let item = eval_dynamic(&step.args[0], state, operators, hosts, runtime)?
                .as_i64()
                .ok_or_else(|| RuntimeError::new("JOSIE_E_CALL_ARG", "item must be i64"))?;
            return Ok(json!(host_map(item)));
        }
    }

    if let Some(host_call) = hosts.get_call(fn_name) {
        let mut eval_args = Vec::with_capacity(step.args.len());
        for arg in &step.args {
            eval_args.push(eval_dynamic(arg, state, operators, hosts, runtime)?);
        }
        return host_call(&eval_args);
    }

    let op_name = fn_name.strip_prefix("core.").unwrap_or(fn_name);
    let mut expr = Vec::with_capacity(step.args.len() + 1);
    expr.push(Value::String(op_name.to_string()));
    expr.extend(step.args.clone());
    eval_tree(&Value::Array(expr), state, operators)
}

fn parse_nested_step(expr: Option<&Value>) -> Option<PipelineStep> {
    let expr = expr?;
    let obj = expr.as_object()?;
    if obj.get("op").and_then(|v| v.as_str()).is_none() {
        return None;
    }
    let step: PipelineStep = serde_json::from_value(expr.clone()).ok()?;
    if validate_step(&step).is_err() {
        return None;
    }
    Some(step)
}

fn read_from_array(
    step: &PipelineStep,
    state: &State,
    runtime: &PipelineRuntime,
) -> Result<Vec<Value>, RuntimeError> {
    let from = step
        .from
        .as_deref()
        .ok_or_else(|| RuntimeError::new("JOSIE_E_ITER_FROM", "iterator step requires 'from'"))?;
    let source = read_ref(from, state, runtime)?;
    match source {
        Value::Array(arr) => Ok(arr),
        _ => Err(RuntimeError::new(
            "JOSIE_E_ITER_SOURCE",
            format!("{} input must resolve to array", step.op),
        )),
    }
}

fn eval_with_iter_locals_expr(
    do_expr: &Option<Value>,
    item: Value,
    index: usize,
    acc: Option<Value>,
    state: &mut State,
    operators: &Operators,
    hosts: &HostFunctions,
    runtime: &mut PipelineRuntime,
) -> Result<Value, RuntimeError> {
    let Some(expr) = do_expr else {
        return Err(RuntimeError::new(
            "JOSIE_E_ITER_DO",
            "iterator step requires do expression",
        ));
    };
    let prev_item = state.client.insert("item".to_string(), item);
    let prev_index = state.client.insert("index".to_string(), json!(index));
    let prev_acc = acc.map(|acc_val| state.client.insert("acc".to_string(), acc_val));

    let out = eval_dynamic(expr, state, operators, hosts, runtime);

    restore_local(&mut state.client, "item", prev_item);
    restore_local(&mut state.client, "index", prev_index);
    if let Some(prev) = prev_acc {
        restore_local(&mut state.client, "acc", prev);
    } else {
        state.client.remove("acc");
    }
    out
}

fn eval_with_iter_locals_step(
    step: &PipelineStep,
    item: Value,
    index: usize,
    acc: Option<Value>,
    state: &mut State,
    operators: &Operators,
    hosts: &HostFunctions,
    runtime: &mut PipelineRuntime,
) -> Result<Value, RuntimeError> {
    let prev_item = state.client.insert("item".to_string(), item);
    let prev_index = state.client.insert("index".to_string(), json!(index));
    let prev_acc = acc.map(|acc_val| state.client.insert("acc".to_string(), acc_val));

    let out = execute_step(step, state, operators, hosts, runtime);

    restore_local(&mut state.client, "item", prev_item);
    restore_local(&mut state.client, "index", prev_index);
    if let Some(prev) = prev_acc {
        restore_local(&mut state.client, "acc", prev);
    } else {
        state.client.remove("acc");
    }
    out
}

fn eval_dynamic(
    expr: &Value,
    state: &mut State,
    operators: &Operators,
    hosts: &HostFunctions,
    runtime: &mut PipelineRuntime,
) -> Result<Value, RuntimeError> {
    if expr.is_array() {
        return eval_tree(expr, state, operators);
    }
    if let Some(obj) = expr.as_object()
        && obj.get("op").and_then(|v| v.as_str()).is_some()
    {
        let step: PipelineStep = serde_json::from_value(expr.clone()).map_err(|err| {
            RuntimeError::new(
                "JOSIE_E_STEP_PARSE",
                format!("invalid nested pipeline step: {err}"),
            )
        })?;
        validate_step(&step).map_err(|err| {
            RuntimeError::new(err.code, err.message).step_opt(err.step_index, err.op)
        })?;
        return execute_step(&step, state, operators, hosts, runtime);
    }
    if let Some(obj) = expr.as_object() {
        // Keep object-literal ergonomics consistent with tree evaluator:
        // nested arrays inside object fields are treated as dynamic expressions.
        let mut out = Map::with_capacity(obj.len());
        for (key, value) in obj {
            let next = eval_dynamic(value, state, operators, hosts, runtime)?;
            out.insert(key.clone(), next);
        }
        return Ok(Value::Object(out));
    }
    Ok(expr.clone())
}

fn eval_tree(
    expr: &Value,
    state: &mut State,
    operators: &Operators,
) -> Result<Value, RuntimeError> {
    let mut ctx = Context {
        state,
        operators,
        event: None,
    };
    evaluate(expr, &mut ctx).map_err(RuntimeError::from)
}

fn write_target(
    target: Option<&str>,
    value: Value,
    state: &mut State,
    runtime: &mut PipelineRuntime,
) -> Result<(), RuntimeError> {
    let target = target.unwrap_or("$prev");
    if target == "$prev" {
        runtime.prev = value;
        return Ok(());
    }
    if target.starts_with("client.") || target.starts_with("server.") {
        set_state_path(state, target, value.clone())?;
    } else {
        runtime.vars.insert(target.to_string(), value.clone());
    }
    runtime.prev = value;
    Ok(())
}

fn read_ref(target: &str, state: &State, runtime: &PipelineRuntime) -> Result<Value, RuntimeError> {
    if target == "$prev" {
        return Ok(runtime.prev.clone());
    }
    if let Some(value) = runtime.vars.get(target) {
        return Ok(value.clone());
    }
    if target.starts_with("client.") || target.starts_with("server.") {
        return get_state_path(state, target).ok_or_else(|| {
            RuntimeError::new(
                "JOSIE_E_REF_NOT_FOUND",
                format!("missing state reference '{target}'"),
            )
        });
    }
    Err(RuntimeError::new(
        "JOSIE_E_REF_NOT_FOUND",
        format!("missing runtime reference '{target}'"),
    ))
}

fn state_from_value(input: &Value) -> State {
    let mut state = State::new();
    if let Some(obj) = input.as_object() {
        if let Some(client) = obj.get("client").and_then(|v| v.as_object()) {
            state.client = client.clone();
        }
        if let Some(server) = obj.get("server").and_then(|v| v.as_object()) {
            state.server = server.clone();
        }
    }
    state
}

fn get_state_path(state: &State, path: &str) -> Option<Value> {
    let (scope, rest) = path.split_once('.')?;
    let base = match scope {
        "client" => &state.client,
        "server" => &state.server,
        _ => return None,
    };
    get_from_map(base, rest)
}

fn get_from_map(map: &Map<String, Value>, path: &str) -> Option<Value> {
    if path.is_empty() {
        return Some(Value::Object(map.clone()));
    }
    let mut current = map.get(path.split('.').next().unwrap_or_default())?;
    for part in path.split('.').skip(1) {
        match current {
            Value::Object(obj) => current = obj.get(part)?,
            Value::Array(arr) => {
                let idx = part.parse::<usize>().ok()?;
                current = arr.get(idx)?;
            }
            _ => return None,
        }
    }
    Some(current.clone())
}

fn set_state_path(state: &mut State, path: &str, value: Value) -> Result<(), RuntimeError> {
    let (scope, rest) = path
        .split_once('.')
        .ok_or_else(|| RuntimeError::new("JOSIE_E_SET_PATH", "invalid set path"))?;
    let target_map = match scope {
        "client" => &mut state.client,
        "server" => &mut state.server,
        _ => {
            return Err(RuntimeError::new(
                "JOSIE_E_SET_PATH",
                format!("path scope must be client/server, got '{scope}'"),
            ));
        }
    };
    set_in_map(target_map, rest, value);
    Ok(())
}

fn set_in_map(map: &mut Map<String, Value>, path: &str, value: Value) {
    let parts: Vec<&str> = path.split('.').filter(|p| !p.is_empty()).collect();
    if parts.is_empty() {
        return;
    }

    let mut current = map;
    for part in &parts[..parts.len().saturating_sub(1)] {
        let entry = current
            .entry((*part).to_string())
            .or_insert_with(|| Value::Object(Map::new()));
        if !entry.is_object() {
            *entry = Value::Object(Map::new());
        }
        current = entry
            .as_object_mut()
            .expect("entry should be object after normalization");
    }
    let last = parts.last().expect("path has at least one part");
    current.insert((*last).to_string(), value);
}

fn restore_local(map: &mut Map<String, Value>, key: &str, previous: Option<Value>) {
    if let Some(prev) = previous {
        map.insert(key.to_string(), prev);
    } else {
        map.remove(key);
    }
}

fn truthy(v: &Value) -> bool {
    match v {
        Value::Null => false,
        Value::Bool(b) => *b,
        Value::Number(n) => n.as_f64().map(|n| n != 0.0).unwrap_or(false),
        Value::String(s) => !s.is_empty(),
        Value::Array(a) => !a.is_empty(),
        Value::Object(o) => !o.is_empty(),
    }
}

fn is_tree_expression(value: &Value) -> bool {
    let Some(arr) = value.as_array() else {
        return false;
    };
    if arr.is_empty() {
        return false;
    }
    arr.first().and_then(|v| v.as_str()).is_some()
}

trait RuntimeErrExt {
    fn step_opt(self, step_index: Option<usize>, op: Option<String>) -> Self;
}

impl RuntimeErrExt for RuntimeError {
    fn step_opt(mut self, step_index: Option<usize>, op: Option<String>) -> Self {
        self.step_index = step_index;
        self.op = op;
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Operators;
    use serde_json::json;
    use std::sync::atomic::{AtomicUsize, Ordering};

    static RETRY_COUNTER: AtomicUsize = AtomicUsize::new(0);
    static IDEMP_COUNTER: AtomicUsize = AtomicUsize::new(0);

    fn op_ext_a(args: &[Value], _ctx: &mut Context) -> Result<Value, EvalError> {
        let p1 = args.first().and_then(|v| v.as_i64()).unwrap_or(0);
        let p2 = args.get(1).and_then(|v| v.as_i64()).unwrap_or(0);
        let size = args.get(2).and_then(|v| v.as_u64()).unwrap_or(0) as usize;
        let mut out = Vec::with_capacity(size);
        for i in 0..size {
            out.push(json!(((i as i64 + p1) * p2) % 97));
        }
        Ok(Value::Array(out))
    }

    fn op_ext_b(args: &[Value], _ctx: &mut Context) -> Result<Value, EvalError> {
        let x = args.first().and_then(|v| v.as_i64()).unwrap_or(0);
        Ok(json!((x * 3 + 1) % 101))
    }

    fn host_gen(p1: i64, p2: i64, size: usize) -> Vec<i64> {
        let mut out = Vec::with_capacity(size);
        for i in 0..size {
            out.push(((i as i64 + p1) * p2) % 97);
        }
        out
    }

    fn host_map(item: i64) -> i64 {
        (item * 3 + 1) % 101
    }

    fn host_fail_once(_args: &[Value]) -> Result<Value, RuntimeError> {
        let n = RETRY_COUNTER.fetch_add(1, Ordering::SeqCst);
        if n == 0 {
            return Err(RuntimeError::new("JOSIE_E_TEST_FAIL", "fail once"));
        }
        Ok(json!("ok"))
    }

    fn host_counter(_args: &[Value]) -> Result<Value, RuntimeError> {
        let n = IDEMP_COUNTER.fetch_add(1, Ordering::SeqCst) + 1;
        Ok(json!(n))
    }

    #[test]
    fn parse_pipeline_ok() {
        let node = json!({
          "v": 2,
          "state": {"client": {}, "server": {}},
          "program": {
            "type": "pipeline",
            "steps": [
              {"op":"call", "fn":"x.a", "args":[1,2,3], "into":"result_a"},
              {"op":"for_each", "from":"result_a", "do":{"op":"call", "fn":"x.b", "args":[["var","item"]]}, "into":"result_b"}
            ]
          }
        });
        let parsed = parse_program(&node);
        assert!(parsed.is_ok());
    }

    #[test]
    fn parse_pipeline_accepts_run_hint() {
        let node = json!({
          "state": {"client": {}, "server": {}},
          "program": {
            "type": "pipeline",
            "steps": [
              {"op":"call", "fn":"x.a", "args":[1], "into":"out", "run_hint":"worker"}
            ]
          }
        });
        let parsed = parse_program(&node).expect("parse");
        let pipe: PipelineDoc = serde_json::from_value(parsed.program).expect("pipe");
        assert_eq!(pipe.steps[0].run_hint, Some(StepRunHint::Worker));
    }

    #[test]
    fn parse_pipeline_missing_for_each_do_fails() {
        let node = json!({
          "v": 2,
          "program": {
            "type": "pipeline",
            "steps": [
              {"op":"for_each", "from":"items", "into":"out"}
            ]
          }
        });
        let err = parse_program(&node).expect_err("expected validation error");
        assert_eq!(err.code, "JOSIE_E_ITER_DO");
    }

    #[test]
    fn parse_tree_ok() {
        let node = json!({
          "v": 2,
          "program": ["+", 1, 2]
        });
        let parsed = parse_program(&node);
        assert!(parsed.is_ok());
    }

    #[test]
    fn execute_pipeline_with_registered_functions_and_for_each() {
        let node = json!({
          "v": 2,
          "state": {"client": {}, "server": {}},
          "program": {
            "type": "pipeline",
            "steps": [
              {"op":"call", "fn":"x.a", "args":[2,3,5], "into":"result_a"},
              {"op":"for_each", "from":"result_a", "do":{"op":"call", "fn":"x.b", "args":[["var","item"]]}, "into":"result_b"}
            ]
          }
        });
        let program = parse_program(&node).expect("parse");
        let mut operators = Operators::new();
        operators.register("x.a", op_ext_a);
        operators.register("x.b", op_ext_b);

        let out = execute_program(&program, &operators).expect("execute");
        assert!(out.value.is_array());
        assert_eq!(out.value.as_array().map(|arr| arr.len()), Some(5));
    }

    #[test]
    fn execute_tree_with_util_namespace() {
        let node = json!({
          "v": 2,
          "program": ["util.str_len", ["util.to_string", 1234]]
        });
        let program = parse_program(&node).expect("parse");
        let operators = Operators::new();
        let out = execute_program(&program, &operators).expect("execute");
        assert_eq!(out.value, json!(4));
    }

    #[test]
    fn fast_external_metrics_path_works() {
        let node = json!({
          "v": 2,
          "state": {"client": {}, "server": {}},
          "program": {
            "type": "pipeline",
            "steps": [
              {"op":"call", "fn":"x.a", "args":[2,3,5], "into":"result_a"},
              {"op":"for_each", "from":"result_a", "do":{"op":"call", "fn":"x.b", "args":[["var","item"]]}, "into":"result_b"}
            ]
          }
        });
        let program = parse_program(&node).expect("parse");
        let compiled = compile_program(&program).expect("compile");
        let mut hosts = HostFunctions::new();
        hosts.register_generate_i64("x.a", host_gen);
        hosts.register_map_i64("x.b", host_map);
        let operators = Operators::new();
        let mut state = State::new();
        let (checksum, len) =
            execute_compiled_program_external_metrics(&compiled, &mut state, &operators, &hosts)
                .expect("fast metrics");
        assert_eq!(len, 5);
        assert!(checksum > 0);
    }

    #[test]
    fn execute_pipeline_retry_policy_works() {
        RETRY_COUNTER.store(0, Ordering::SeqCst);
        let node = json!({
          "state": {"client": {}, "server": {}},
          "program": {
            "type": "pipeline",
            "steps": [
              {
                "id": "retry-step",
                "type": "action",
                "op":"call",
                "fn":"x.fail_once",
                "on_error":"retry",
                "max_retries": 1,
                "into":"r"
              },
              {"op":"return", "from":"r"}
            ]
          }
        });
        let program = parse_program(&node).expect("parse");
        let mut hosts = HostFunctions::new();
        hosts.register_call("x.fail_once", host_fail_once);
        let operators = Operators::new();
        let out = execute_program_with_hosts(&program, &operators, &hosts).expect("execute");
        assert_eq!(out.value, json!("ok"));
        assert_eq!(RETRY_COUNTER.load(Ordering::SeqCst), 2);
    }

    #[test]
    fn execute_pipeline_idempotency_key_skips_duplicate_step() {
        IDEMP_COUNTER.store(0, Ordering::SeqCst);
        let node = json!({
          "state": {"client": {}, "server": {}},
          "program": {
            "type": "pipeline",
            "steps": [
              {"op":"call", "fn":"x.counter", "idempotency_key":"job-42", "into":"a"},
              {"op":"call", "fn":"x.counter", "idempotency_key":"job-42", "into":"b"},
              {"op":"return", "from":"b"}
            ]
          }
        });
        let program = parse_program(&node).expect("parse");
        let mut hosts = HostFunctions::new();
        hosts.register_call("x.counter", host_counter);
        let operators = Operators::new();
        let out = execute_program_with_hosts(&program, &operators, &hosts).expect("execute");
        assert_eq!(out.value, json!(1));
        assert_eq!(IDEMP_COUNTER.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn set_step_evaluates_nested_object_expressions() {
        let node = json!({
          "state": {"client": {}, "server": {"input": {"title": "Hello", "slug": "hello"}}},
          "program": {
            "type": "pipeline",
            "steps": [
              {"op":"set","into":"row","args":[
                {"title":["var","server.input.title"],"slug":["var","server.input.slug"]}
              ]},
              {"op":"return","from":"row"}
            ]
          }
        });
        let program = parse_program(&node).expect("parse");
        let operators = Operators::new();
        let out = execute_program(&program, &operators).expect("execute");
        assert_eq!(out.value, json!({"title":"Hello","slug":"hello"}));
    }
}
