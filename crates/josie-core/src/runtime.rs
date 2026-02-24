//! Runtime evaluator and operator registry for JOSIE tree expressions.
//!
//! This module defines:
//! - canonical runtime state ([`State`])
//! - execution context ([`Context`])
//! - operator table ([`Operators`])
//! - recursive evaluator ([`evaluate`])
//!
//! Operator names here are the canonical core surface used by web/cli adapters.

use regex::RegexBuilder;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value, json};
use std::collections::{HashMap, HashSet};

/// Canonical JSON node type accepted by the evaluator.
pub type JsonNode = Value;
/// Standard evaluator return type.
pub type EvalResult = Result<Value, EvalError>;

/// Runtime evaluation error.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EvalError {
    pub message: String,
}

impl EvalError {
    /// Create a new runtime evaluation error.
    pub fn new(msg: impl Into<String>) -> Self {
        Self {
            message: msg.into(),
        }
    }
}

/// Runtime-defined function metadata for `def` / `call`.
#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub params: Vec<String>,
    pub body: Value,
}

/// Runtime state split into `server` and `client` scopes.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct State {
    pub server: Map<String, Value>,
    pub client: Map<String, Value>,
    #[serde(skip)]
    pub fns: HashMap<String, FunctionDef>,
}

impl State {
    /// Create an empty runtime state.
    pub fn new() -> Self {
        Self {
            server: Map::new(),
            client: Map::new(),
            fns: HashMap::new(),
        }
    }
}

impl Default for State {
    fn default() -> Self {
        Self::new()
    }
}

/// Event context used by `w.event.*` operators.
#[derive(Debug, Clone)]
pub struct EventContext {
    pub value: Value,
    pub key: Value,
    pub prevent: bool,
}

impl Default for EventContext {
    fn default() -> Self {
        Self {
            value: Value::Null,
            key: Value::Null,
            prevent: false,
        }
    }
}

/// Mutable execution context passed to every operator call.
pub struct Context<'a> {
    pub state: &'a mut State,
    pub operators: &'a Operators,
    pub event: Option<&'a EventContext>,
}

/// Operator function signature used by the registry.
pub type Operator = fn(args: &[Value], ctx: &mut Context) -> EvalResult;

/// Canonical operator registry.
///
/// Use [`Operators::register`] to add host-specific operators (typically `x.*`).
pub struct Operators {
    ops: HashMap<String, Operator>,
}

impl Operators {
    /// Build the default canonical operator registry.
    pub fn new() -> Self {
        let mut ops: HashMap<String, Operator> = HashMap::new();
        ops.insert("var".into(), op_var as Operator);
        ops.insert("set".into(), op_set as Operator);
        ops.insert("+".into(), op_add as Operator);
        ops.insert("-".into(), op_sub as Operator);
        ops.insert("*".into(), op_mul as Operator);
        ops.insert("/".into(), op_div as Operator);
        ops.insert("%".into(), op_mod as Operator);
        ops.insert("==".into(), op_eq as Operator);
        ops.insert("!=".into(), op_neq as Operator);
        ops.insert(">".into(), op_gt as Operator);
        ops.insert("<".into(), op_lt as Operator);
        ops.insert(">=".into(), op_gte as Operator);
        ops.insert("<=".into(), op_lte as Operator);
        ops.insert("if".into(), op_if as Operator);
        ops.insert("&&".into(), op_and as Operator);
        ops.insert("||".into(), op_or as Operator);
        ops.insert("!".into(), op_not as Operator);
        ops.insert("util.concat".into(), op_concat as Operator);
        ops.insert("util.lower".into(), op_lower as Operator);
        ops.insert("util.upper".into(), op_upper as Operator);
        ops.insert("util.contains".into(), op_contains as Operator);
        ops.insert("util.template".into(), op_template as Operator);
        ops.insert("util.to_int".into(), op_to_int as Operator);
        ops.insert("util.to_float".into(), op_to_float as Operator);
        ops.insert("util.to_string".into(), op_to_string as Operator);
        ops.insert("util.trim".into(), op_trim as Operator);
        ops.insert("util.str_len".into(), op_str_len as Operator);
        ops.insert("util.get_path".into(), op_util_get_path as Operator);
        ops.insert("util.one_of".into(), op_util_one_of as Operator);
        ops.insert("util.pick".into(), op_util_pick as Operator);
        ops.insert("util.exists".into(), op_util_exists as Operator);
        ops.insert("util.omit".into(), op_util_omit as Operator);
        ops.insert("util.as_array".into(), op_util_as_array as Operator);
        ops.insert("util.to_bool".into(), op_util_to_bool as Operator);
        ops.insert("util.replace".into(), op_util_replace as Operator);
        ops.insert("util.replace_all".into(), op_util_replace_all as Operator);
        ops.insert("util.split".into(), op_util_split as Operator);
        ops.insert("util.join".into(), op_util_join as Operator);
        ops.insert("util.regex_match".into(), op_util_regex_match as Operator);
        ops.insert("util.regex_find".into(), op_util_regex_find as Operator);
        ops.insert("util.regex_find_all".into(), op_util_regex_find_all as Operator);
        ops.insert("util.regex_replace".into(), op_util_regex_replace as Operator);
        ops.insert("log".into(), op_log as Operator);
        ops.insert("len".into(), op_len as Operator);
        ops.insert("push".into(), op_push as Operator);
        ops.insert("get".into(), op_get as Operator);
        ops.insert("data.select".into(), op_data_select as Operator);
        ops.insert("data.rename".into(), op_data_rename as Operator);
        ops.insert("data.cast".into(), op_data_cast as Operator);
        ops.insert("data.chunk".into(), op_data_chunk as Operator);
        ops.insert("data.flat_map".into(), op_data_flat_map as Operator);
        ops.insert("data.distinct".into(), op_data_distinct as Operator);
        ops.insert("data.sort_by".into(), op_data_sort_by as Operator);
        ops.insert("data.group_by".into(), op_data_group_by as Operator);
        ops.insert("data.aggregate".into(), op_data_aggregate as Operator);
        ops.insert("data.join".into(), op_data_join as Operator);
        ops.insert("do".into(), op_do as Operator);
        ops.insert("pipe".into(), op_pipe as Operator);
        ops.insert("def".into(), op_def as Operator);
        ops.insert("call".into(), op_call as Operator);
        ops.insert("map".into(), op_map as Operator);
        ops.insert("filter".into(), op_filter as Operator);
        ops.insert("match".into(), op_match as Operator);
        ops.insert("w.event.value".into(), op_event_value as Operator);
        ops.insert("w.event.key".into(), op_event_key as Operator);
        ops.insert("w.event.prevent".into(), op_event_prevent as Operator);
        ops.insert("effect".into(), op_effect as Operator);
        Self { ops }
    }

    /// Lookup an operator by name.
    pub fn get(&self, name: &str) -> Option<Operator> {
        self.ops.get(name).copied()
    }

    /// Register or replace an operator. Returns previous handler if present.
    pub fn register(&mut self, name: impl Into<String>, operator: Operator) -> Option<Operator> {
        self.ops.insert(name.into(), operator)
    }
}

impl Default for Operators {
    fn default() -> Self {
        Self::new()
    }
}

/// Recursively evaluate a JOSIE tree expression.
///
/// If the first array element is a known string operator, dispatches to
/// that operator; otherwise evaluates array/object children as literals.
pub fn evaluate(node: &Value, ctx: &mut Context) -> EvalResult {
    match node {
        Value::Array(arr) => {
            if arr.is_empty() {
                return Ok(Value::Array(vec![]));
            }
            if let Some(op_name) = arr.first().and_then(|v| v.as_str()) {
                if let Some(op) = ctx.operators.get(op_name) {
                    return op(&arr[1..], ctx);
                }
            }
            let mut out = Vec::with_capacity(arr.len());
            for item in arr {
                out.push(evaluate(item, ctx)?);
            }
            Ok(Value::Array(out))
        }
        Value::Object(obj) => {
            let mut out = Map::new();
            for (k, v) in obj {
                out.insert(k.clone(), evaluate(v, ctx)?);
            }
            Ok(Value::Object(out))
        }
        _ => Ok(node.clone()),
    }
}

/// Read a value using dot-path semantics from a state map.
pub fn get_path(state: &Map<String, Value>, path: &Value) -> Option<Value> {
    let path = path.as_str()?;
    if let Some(rest) = path.strip_prefix("client.") {
        return state
            .get("client")
            .and_then(Value::as_object)
            .and_then(|m| map_get(m, rest));
    }
    if let Some(rest) = path.strip_prefix("server.") {
        return state
            .get("server")
            .and_then(Value::as_object)
            .and_then(|m| map_get(m, rest));
    }
    map_get(state, path)
}

/// Write a value using dot-path semantics into a state map.
pub fn set_path(state: &mut Map<String, Value>, path: &Value, value: Value) {
    let Some(path) = path.as_str() else {
        return;
    };
    if let Some(rest) = path.strip_prefix("client.") {
        let client = state
            .entry("client".to_string())
            .or_insert_with(|| Value::Object(Map::new()));
        if let Some(client_map) = client.as_object_mut() {
            map_set(client_map, rest, value);
        }
        return;
    }
    if let Some(rest) = path.strip_prefix("server.") {
        let server = state
            .entry("server".to_string())
            .or_insert_with(|| Value::Object(Map::new()));
        if let Some(server_map) = server.as_object_mut() {
            map_set(server_map, rest, value);
        }
        return;
    }
    map_set(state, path, value);
}

fn require_arg<'a>(args: &'a [Value], index: usize, op: &str) -> Result<&'a Value, EvalError> {
    args.get(index)
        .ok_or_else(|| EvalError::new(format!("{op} requires argument {index}")))
}

fn as_f64(value: &Value) -> Option<f64> {
    match value {
        Value::Number(n) => n.as_f64(),
        Value::String(s) => s.parse::<f64>().ok(),
        Value::Bool(b) => Some(if *b { 1.0 } else { 0.0 }),
        _ => None,
    }
}

fn as_i64(value: &Value) -> Option<i64> {
    match value {
        Value::Number(n) => n.as_i64().or_else(|| n.as_f64().map(|v| v as i64)),
        Value::String(s) => s.parse::<i64>().ok(),
        Value::Bool(b) => Some(if *b { 1 } else { 0 }),
        _ => None,
    }
}

fn truthy(v: &Value) -> bool {
    match v {
        Value::Null => false,
        Value::Bool(b) => *b,
        Value::Number(n) => n.as_f64().map(|f| f != 0.0).unwrap_or(false),
        Value::String(s) => !s.is_empty(),
        Value::Array(a) => !a.is_empty(),
        Value::Object(o) => !o.is_empty(),
    }
}

fn read_state_path(state: &State, path: &str) -> Option<Value> {
    if let Some(rest) = path.strip_prefix("client.") {
        return map_get(&state.client, rest);
    }
    if let Some(rest) = path.strip_prefix("server.") {
        return map_get(&state.server, rest);
    }
    map_get(&state.client, path).or_else(|| map_get(&state.server, path))
}

fn write_state_path(state: &mut State, path: &str, value: Value) {
    if let Some(rest) = path.strip_prefix("client.") {
        map_set(&mut state.client, rest, value);
        return;
    }
    if let Some(rest) = path.strip_prefix("server.") {
        map_set(&mut state.server, rest, value);
        return;
    }
    map_set(&mut state.client, path, value);
}

fn map_get(map: &Map<String, Value>, path: &str) -> Option<Value> {
    if path.is_empty() {
        return Some(Value::Object(map.clone()));
    }
    let mut parts = path.split('.');
    let first = parts.next()?;
    let mut current = map.get(first)?;
    for part in parts {
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

fn map_set(map: &mut Map<String, Value>, path: &str, value: Value) {
    let parts: Vec<&str> = path.split('.').filter(|p| !p.is_empty()).collect();
    if parts.is_empty() {
        return;
    }
    let mut cur = map;
    for part in &parts[..parts.len() - 1] {
        let entry = cur
            .entry((*part).to_string())
            .or_insert_with(|| Value::Object(Map::new()));
        if !entry.is_object() {
            *entry = Value::Object(Map::new());
        }
        cur = entry
            .as_object_mut()
            .expect("object ensured before descending");
    }
    cur.insert(parts[parts.len() - 1].to_string(), value);
}

#[derive(Debug, Clone)]
enum PathSegment {
    Key(String),
    Index(usize),
}

fn parse_path(path: &str) -> Option<Vec<PathSegment>> {
    if path.is_empty() {
        return Some(Vec::new());
    }

    let chars: Vec<char> = path.chars().collect();
    let mut i = 0usize;
    let mut key = String::new();
    let mut out = Vec::new();

    while i < chars.len() {
        match chars[i] {
            '.' => {
                if !key.is_empty() {
                    out.push(PathSegment::Key(std::mem::take(&mut key)));
                }
                i += 1;
            }
            '[' => {
                if !key.is_empty() {
                    out.push(PathSegment::Key(std::mem::take(&mut key)));
                }
                i += 1;
                let start = i;
                while i < chars.len() && chars[i] != ']' {
                    i += 1;
                }
                if i >= chars.len() || i == start {
                    return None;
                }
                let idx_text: String = chars[start..i].iter().collect();
                let idx = idx_text.parse::<usize>().ok()?;
                out.push(PathSegment::Index(idx));
                i += 1;
            }
            c => {
                key.push(c);
                i += 1;
            }
        }
    }

    if !key.is_empty() {
        out.push(PathSegment::Key(key));
    }
    Some(out)
}

fn value_get_path(value: &Value, path: &str) -> Option<Value> {
    let segs = parse_path(path)?;
    if segs.is_empty() {
        return Some(value.clone());
    }
    value_get_by_segments(value, &segs)
}

fn value_get_by_segments(value: &Value, segs: &[PathSegment]) -> Option<Value> {
    let mut cur = value;
    for seg in segs {
        match seg {
            PathSegment::Key(k) => cur = cur.as_object()?.get(k)?,
            PathSegment::Index(i) => cur = cur.as_array()?.get(*i)?,
        }
    }
    Some(cur.clone())
}

fn value_set_by_segments(target: &mut Value, segs: &[PathSegment], value: Value) {
    fn set_at(cur: &mut Value, segs: &[PathSegment], idx: usize, value: Value) {
        if idx >= segs.len() {
            *cur = value;
            return;
        }
        match &segs[idx] {
            PathSegment::Key(k) => {
                if !cur.is_object() {
                    *cur = Value::Object(Map::new());
                }
                let obj = cur.as_object_mut().expect("object ensured");
                let entry = obj.entry(k.clone()).or_insert(Value::Null);
                set_at(entry, segs, idx + 1, value);
            }
            PathSegment::Index(i) => {
                if !cur.is_array() {
                    *cur = Value::Array(Vec::new());
                }
                let arr = cur.as_array_mut().expect("array ensured");
                if arr.len() <= *i {
                    arr.resize(*i + 1, Value::Null);
                }
                set_at(&mut arr[*i], segs, idx + 1, value);
            }
        }
    }

    set_at(target, segs, 0, value);
}

fn value_take_by_segments(target: &mut Value, segs: &[PathSegment]) -> Option<Value> {
    if segs.is_empty() {
        return None;
    }

    fn take_at(cur: &mut Value, segs: &[PathSegment], idx: usize) -> Option<Value> {
        if idx + 1 == segs.len() {
            return match &segs[idx] {
                PathSegment::Key(k) => cur.as_object_mut()?.remove(k),
                PathSegment::Index(i) => {
                    let arr = cur.as_array_mut()?;
                    if *i >= arr.len() {
                        None
                    } else {
                        Some(std::mem::replace(&mut arr[*i], Value::Null))
                    }
                }
            };
        }

        match &segs[idx] {
            PathSegment::Key(k) => take_at(cur.as_object_mut()?.get_mut(k)?, segs, idx + 1),
            PathSegment::Index(i) => take_at(cur.as_array_mut()?.get_mut(*i)?, segs, idx + 1),
        }
    }

    take_at(target, segs, 0)
}

fn value_to_string(v: Value) -> String {
    match v {
        Value::String(s) => s,
        Value::Null => String::new(),
        other => other.to_string(),
    }
}

fn value_to_bool(v: Value) -> bool {
    match v {
        Value::Bool(b) => b,
        Value::Number(n) => n.as_f64().map(|x| x != 0.0).unwrap_or(false),
        Value::String(s) => {
            let norm = s.trim().to_ascii_lowercase();
            matches!(norm.as_str(), "1" | "true" | "yes" | "y" | "on")
        }
        Value::Array(a) => !a.is_empty(),
        Value::Object(o) => !o.is_empty(),
        Value::Null => false,
    }
}

fn cast_value(value: Value, ty: &str) -> Value {
    match ty.trim().to_ascii_lowercase().as_str() {
        "string" | "str" | "text" => Value::String(value_to_string(value)),
        "int" | "i64" | "u64" | "integer" => json!(as_i64(&value).unwrap_or(0)),
        "float" | "f64" | "number" => json!(as_f64(&value).unwrap_or(0.0)),
        "bool" | "boolean" => Value::Bool(value_to_bool(value)),
        "datetime" | "timestamp" => Value::String(value_to_string(value)),
        _ => value,
    }
}

#[derive(Debug, Clone, Default)]
struct RegexOpts {
    case_insensitive: bool,
    multi_line: bool,
    dot_matches_new_line: bool,
    unicode: bool,
    all: bool,
}

fn parse_regex_opts(raw: Option<&Value>) -> RegexOpts {
    let mut out = RegexOpts {
        unicode: true,
        all: true,
        ..RegexOpts::default()
    };

    let Some(raw) = raw else {
        return out;
    };

    match raw {
        Value::Bool(b) => out.all = *b,
        Value::String(flags) => {
            for ch in flags.chars() {
                match ch {
                    'i' | 'I' => out.case_insensitive = true,
                    'm' | 'M' => out.multi_line = true,
                    's' | 'S' => out.dot_matches_new_line = true,
                    'u' | 'U' => out.unicode = true,
                    'g' | 'G' => out.all = true,
                    _ => {}
                }
            }
        }
        Value::Object(map) => {
            out.case_insensitive = map
                .get("case_insensitive")
                .or_else(|| map.get("ignore_case"))
                .map(|v| value_to_bool(v.clone()))
                .unwrap_or(false);
            out.multi_line = map
                .get("multi_line")
                .or_else(|| map.get("multiline"))
                .map(|v| value_to_bool(v.clone()))
                .unwrap_or(false);
            out.dot_matches_new_line = map
                .get("dot_matches_new_line")
                .or_else(|| map.get("dot_all"))
                .map(|v| value_to_bool(v.clone()))
                .unwrap_or(false);
            out.unicode = map
                .get("unicode")
                .map(|v| value_to_bool(v.clone()))
                .unwrap_or(true);
            out.all = map
                .get("all")
                .map(|v| value_to_bool(v.clone()))
                .unwrap_or(true);
        }
        _ => {}
    }

    out
}

fn build_regex(pattern: &str, opts: &RegexOpts) -> Result<regex::Regex, EvalError> {
    let mut builder = RegexBuilder::new(pattern);
    builder
        .case_insensitive(opts.case_insensitive)
        .multi_line(opts.multi_line)
        .dot_matches_new_line(opts.dot_matches_new_line)
        .unicode(opts.unicode);
    builder
        .build()
        .map_err(|e| EvalError::new(format!("invalid regex '{pattern}': {e}")))
}

fn cmp_numbers_or_strings(a: &Value, b: &Value) -> Option<std::cmp::Ordering> {
    match (as_f64(a), as_f64(b)) {
        (Some(x), Some(y)) => x.partial_cmp(&y),
        _ => Some(a.to_string().cmp(&b.to_string())),
    }
}

#[derive(Debug, Clone)]
enum AggExpr {
    CountAll,
    CountField(Vec<PathSegment>),
    Sum(Vec<PathSegment>),
    Avg(Vec<PathSegment>),
    Min(Vec<PathSegment>),
    Max(Vec<PathSegment>),
}

fn parse_agg_expr(raw: &str) -> Option<AggExpr> {
    let raw = raw.trim();
    let text = raw.to_ascii_lowercase();
    if text == "count(*)" {
        return Some(AggExpr::CountAll);
    }
    let parse_fn = |name: &str| -> Option<Vec<PathSegment>> {
        let prefix = format!("{name}(");
        if !text.starts_with(&prefix) || !text.ends_with(')') {
            return None;
        }
        let inner = raw[prefix.len()..raw.len() - 1].trim();
        parse_path(inner)
    };

    parse_fn("count")
        .map(AggExpr::CountField)
        .or_else(|| parse_fn("sum").map(AggExpr::Sum))
        .or_else(|| parse_fn("avg").map(AggExpr::Avg))
        .or_else(|| parse_fn("min").map(AggExpr::Min))
        .or_else(|| parse_fn("max").map(AggExpr::Max))
}

fn op_var(args: &[Value], ctx: &mut Context) -> EvalResult {
    let raw = evaluate(require_arg(args, 0, "var")?, ctx)?;
    let Some(path) = raw.as_str() else {
        return Ok(Value::Null);
    };
    Ok(read_state_path(ctx.state, path).unwrap_or(Value::Null))
}

fn op_set(args: &[Value], ctx: &mut Context) -> EvalResult {
    let path_val = evaluate(require_arg(args, 0, "set")?, ctx)?;
    let value = evaluate(require_arg(args, 1, "set")?, ctx)?;
    let Some(path) = path_val.as_str() else {
        return Err(EvalError::new("set path must evaluate to string"));
    };
    write_state_path(ctx.state, path, value.clone());
    Ok(value)
}

fn op_add(args: &[Value], ctx: &mut Context) -> EvalResult {
    let mut total = 0.0;
    for arg in args {
        let v = evaluate(arg, ctx)?;
        total += as_f64(&v).unwrap_or(0.0);
    }
    Ok(json!(total))
}

fn op_sub(args: &[Value], ctx: &mut Context) -> EvalResult {
    let first = evaluate(require_arg(args, 0, "-")?, ctx)?;
    let second = evaluate(require_arg(args, 1, "-")?, ctx)?;
    Ok(json!(
        as_f64(&first).unwrap_or(0.0) - as_f64(&second).unwrap_or(0.0)
    ))
}

fn op_mul(args: &[Value], ctx: &mut Context) -> EvalResult {
    let mut product = 1.0;
    for arg in args {
        let v = evaluate(arg, ctx)?;
        product *= as_f64(&v).unwrap_or(0.0);
    }
    Ok(json!(product))
}

fn op_div(args: &[Value], ctx: &mut Context) -> EvalResult {
    let a = evaluate(require_arg(args, 0, "/")?, ctx)?;
    let b = evaluate(require_arg(args, 1, "/")?, ctx)?;
    let denom = as_f64(&b).unwrap_or(0.0);
    if denom == 0.0 {
        return Err(EvalError::new("div by zero"));
    }
    Ok(json!(as_f64(&a).unwrap_or(0.0) / denom))
}

fn op_mod(args: &[Value], ctx: &mut Context) -> EvalResult {
    let a = evaluate(require_arg(args, 0, "%")?, ctx)?;
    let b = evaluate(require_arg(args, 1, "%")?, ctx)?;
    let denom = as_i64(&b).unwrap_or(0);
    if denom == 0 {
        return Err(EvalError::new("mod by zero"));
    }
    Ok(json!(as_i64(&a).unwrap_or(0) % denom))
}

fn op_eq(args: &[Value], ctx: &mut Context) -> EvalResult {
    let a = evaluate(require_arg(args, 0, "==")?, ctx)?;
    let b = evaluate(require_arg(args, 1, "==")?, ctx)?;
    Ok(Value::Bool(a == b))
}

fn op_neq(args: &[Value], ctx: &mut Context) -> EvalResult {
    let a = evaluate(require_arg(args, 0, "!=")?, ctx)?;
    let b = evaluate(require_arg(args, 1, "!=")?, ctx)?;
    Ok(Value::Bool(a != b))
}

fn op_gt(args: &[Value], ctx: &mut Context) -> EvalResult {
    let a = evaluate(require_arg(args, 0, ">")?, ctx)?;
    let b = evaluate(require_arg(args, 1, ">")?, ctx)?;
    Ok(Value::Bool(
        cmp_numbers_or_strings(&a, &b).is_some_and(|o| o.is_gt()),
    ))
}

fn op_lt(args: &[Value], ctx: &mut Context) -> EvalResult {
    let a = evaluate(require_arg(args, 0, "<")?, ctx)?;
    let b = evaluate(require_arg(args, 1, "<")?, ctx)?;
    Ok(Value::Bool(
        cmp_numbers_or_strings(&a, &b).is_some_and(|o| o.is_lt()),
    ))
}

fn op_gte(args: &[Value], ctx: &mut Context) -> EvalResult {
    let a = evaluate(require_arg(args, 0, ">=")?, ctx)?;
    let b = evaluate(require_arg(args, 1, ">=")?, ctx)?;
    Ok(Value::Bool(
        cmp_numbers_or_strings(&a, &b).is_some_and(|o| !o.is_lt()),
    ))
}

fn op_lte(args: &[Value], ctx: &mut Context) -> EvalResult {
    let a = evaluate(require_arg(args, 0, "<=")?, ctx)?;
    let b = evaluate(require_arg(args, 1, "<=")?, ctx)?;
    Ok(Value::Bool(
        cmp_numbers_or_strings(&a, &b).is_some_and(|o| !o.is_gt()),
    ))
}

fn op_if(args: &[Value], ctx: &mut Context) -> EvalResult {
    let cond = evaluate(require_arg(args, 0, "if")?, ctx)?;
    if truthy(&cond) {
        if let Some(t) = args.get(1) {
            evaluate(t, ctx)
        } else {
            Ok(Value::Null)
        }
    } else if let Some(f) = args.get(2) {
        evaluate(f, ctx)
    } else {
        Ok(Value::Null)
    }
}

fn op_and(args: &[Value], ctx: &mut Context) -> EvalResult {
    let mut last = Value::Bool(true);
    for arg in args {
        last = evaluate(arg, ctx)?;
        if !truthy(&last) {
            return Ok(Value::Bool(false));
        }
    }
    Ok(last)
}

fn op_or(args: &[Value], ctx: &mut Context) -> EvalResult {
    for arg in args {
        let v = evaluate(arg, ctx)?;
        if truthy(&v) {
            return Ok(v);
        }
    }
    Ok(Value::Bool(false))
}

fn op_not(args: &[Value], ctx: &mut Context) -> EvalResult {
    let v = evaluate(require_arg(args, 0, "!")?, ctx)?;
    Ok(Value::Bool(!truthy(&v)))
}

fn op_concat(args: &[Value], ctx: &mut Context) -> EvalResult {
    let mut out = String::new();
    for arg in args {
        let v = evaluate(arg, ctx)?;
        match v {
            Value::String(s) => out.push_str(&s),
            Value::Null => {}
            other => out.push_str(&other.to_string()),
        }
    }
    Ok(Value::String(out))
}

fn op_lower(args: &[Value], ctx: &mut Context) -> EvalResult {
    let v = evaluate(require_arg(args, 0, "util.lower")?, ctx)?;
    Ok(Value::String(
        v.as_str().unwrap_or(&v.to_string()).to_lowercase(),
    ))
}

fn op_upper(args: &[Value], ctx: &mut Context) -> EvalResult {
    let v = evaluate(require_arg(args, 0, "util.upper")?, ctx)?;
    Ok(Value::String(
        v.as_str().unwrap_or(&v.to_string()).to_uppercase(),
    ))
}

fn op_contains(args: &[Value], ctx: &mut Context) -> EvalResult {
    let hay = evaluate(require_arg(args, 0, "util.contains")?, ctx)?;
    let needle = evaluate(require_arg(args, 1, "util.contains")?, ctx)?;
    let result = match hay {
        Value::String(s) => s.contains(needle.as_str().unwrap_or(&needle.to_string())),
        Value::Array(arr) => arr.contains(&needle),
        _ => false,
    };
    Ok(Value::Bool(result))
}

fn op_template(args: &[Value], ctx: &mut Context) -> EvalResult {
    let fmt = evaluate(require_arg(args, 0, "util.template")?, ctx)?;
    let mut out = fmt.as_str().unwrap_or(&fmt.to_string()).to_string();
    for arg in &args[1..] {
        let v = evaluate(arg, ctx)?;
        let rep = match v {
            Value::String(s) => s,
            Value::Null => String::new(),
            other => other.to_string(),
        };
        if out.contains("{}") {
            out = out.replacen("{}", &rep, 1);
        }
    }
    Ok(Value::String(out))
}

fn op_to_int(args: &[Value], ctx: &mut Context) -> EvalResult {
    let v = evaluate(require_arg(args, 0, "util.to_int")?, ctx)?;
    Ok(json!(as_i64(&v).unwrap_or(0)))
}

fn op_to_float(args: &[Value], ctx: &mut Context) -> EvalResult {
    let v = evaluate(require_arg(args, 0, "util.to_float")?, ctx)?;
    Ok(json!(as_f64(&v).unwrap_or(0.0)))
}

fn op_to_string(args: &[Value], ctx: &mut Context) -> EvalResult {
    let v = evaluate(require_arg(args, 0, "util.to_string")?, ctx)?;
    Ok(Value::String(match v {
        Value::String(s) => s,
        Value::Null => String::new(),
        other => other.to_string(),
    }))
}

fn op_trim(args: &[Value], ctx: &mut Context) -> EvalResult {
    let v = evaluate(require_arg(args, 0, "util.trim")?, ctx)?;
    Ok(Value::String(
        v.as_str().unwrap_or(&v.to_string()).trim().to_string(),
    ))
}

fn op_str_len(args: &[Value], ctx: &mut Context) -> EvalResult {
    let v = evaluate(require_arg(args, 0, "util.str_len")?, ctx)?;
    Ok(json!(v.as_str().unwrap_or(&v.to_string()).chars().count()))
}

fn op_util_get_path(args: &[Value], ctx: &mut Context) -> EvalResult {
    let source = evaluate(require_arg(args, 0, "util.get_path")?, ctx)?;
    let path = evaluate(require_arg(args, 1, "util.get_path")?, ctx)?;
    let fallback = if let Some(arg) = args.get(2) {
        evaluate(arg, ctx)?
    } else {
        Value::Null
    };
    let Some(path) = path.as_str() else {
        return Ok(fallback);
    };
    Ok(value_get_path(&source, path).unwrap_or(fallback))
}

fn op_util_one_of(args: &[Value], ctx: &mut Context) -> EvalResult {
    let source = evaluate(require_arg(args, 0, "util.one_of")?, ctx)?;
    let paths = evaluate(require_arg(args, 1, "util.one_of")?, ctx)?;
    let fallback = if let Some(arg) = args.get(2) {
        evaluate(arg, ctx)?
    } else {
        Value::Null
    };

    if let Some(list) = paths.as_array() {
        for item in list {
            if let Some(path) = item.as_str() {
                if let Some(v) = value_get_path(&source, path) {
                    return Ok(v);
                }
            }
        }
    } else if let Some(path) = paths.as_str() {
        if let Some(v) = value_get_path(&source, path) {
            return Ok(v);
        }
    }

    Ok(fallback)
}

fn op_util_pick(args: &[Value], ctx: &mut Context) -> EvalResult {
    let source = evaluate(require_arg(args, 0, "util.pick")?, ctx)?;
    let paths = evaluate(require_arg(args, 1, "util.pick")?, ctx)?;
    let mut out = Value::Object(Map::new());

    if let Some(list) = paths.as_array() {
        for item in list {
            let Some(path) = item.as_str() else {
                continue;
            };
            let Some(segs) = parse_path(path) else {
                continue;
            };
            if let Some(v) = value_get_by_segments(&source, &segs) {
                value_set_by_segments(&mut out, &segs, v);
            }
        }
    }

    Ok(out)
}

fn op_util_exists(args: &[Value], ctx: &mut Context) -> EvalResult {
    let source = evaluate(require_arg(args, 0, "util.exists")?, ctx)?;
    let path = evaluate(require_arg(args, 1, "util.exists")?, ctx)?;
    let Some(path) = path.as_str() else {
        return Ok(Value::Bool(false));
    };
    Ok(Value::Bool(value_get_path(&source, path).is_some()))
}

fn op_util_omit(args: &[Value], ctx: &mut Context) -> EvalResult {
    let source = evaluate(require_arg(args, 0, "util.omit")?, ctx)?;
    let paths = evaluate(require_arg(args, 1, "util.omit")?, ctx)?;
    let mut out = source;

    if let Some(list) = paths.as_array() {
        for item in list {
            let Some(path) = item.as_str() else {
                continue;
            };
            let Some(segs) = parse_path(path) else {
                continue;
            };
            let _ = value_take_by_segments(&mut out, &segs);
        }
    }

    Ok(out)
}

fn op_util_as_array(args: &[Value], ctx: &mut Context) -> EvalResult {
    let value = evaluate(require_arg(args, 0, "util.as_array")?, ctx)?;
    match value {
        Value::Array(arr) => Ok(Value::Array(arr)),
        Value::Null => Ok(Value::Array(Vec::new())),
        other => Ok(Value::Array(vec![other])),
    }
}

fn op_util_to_bool(args: &[Value], ctx: &mut Context) -> EvalResult {
    let value = evaluate(require_arg(args, 0, "util.to_bool")?, ctx)?;
    Ok(Value::Bool(value_to_bool(value)))
}

fn op_util_replace(args: &[Value], ctx: &mut Context) -> EvalResult {
    let source = value_to_string(evaluate(require_arg(args, 0, "util.replace")?, ctx)?);
    let from = value_to_string(evaluate(require_arg(args, 1, "util.replace")?, ctx)?);
    let to = value_to_string(evaluate(require_arg(args, 2, "util.replace")?, ctx)?);
    if from.is_empty() {
        return Ok(Value::String(source));
    }
    Ok(Value::String(source.replacen(&from, &to, 1)))
}

fn op_util_replace_all(args: &[Value], ctx: &mut Context) -> EvalResult {
    let source = value_to_string(evaluate(require_arg(args, 0, "util.replace_all")?, ctx)?);
    let from = value_to_string(evaluate(require_arg(args, 1, "util.replace_all")?, ctx)?);
    let to = value_to_string(evaluate(require_arg(args, 2, "util.replace_all")?, ctx)?);
    if from.is_empty() {
        return Ok(Value::String(source));
    }
    Ok(Value::String(source.replace(&from, &to)))
}

fn op_util_split(args: &[Value], ctx: &mut Context) -> EvalResult {
    let source = value_to_string(evaluate(require_arg(args, 0, "util.split")?, ctx)?);
    let delim = value_to_string(evaluate(require_arg(args, 1, "util.split")?, ctx)?);
    if delim.is_empty() {
        let parts: Vec<Value> = source
            .chars()
            .map(|c| Value::String(c.to_string()))
            .collect();
        return Ok(Value::Array(parts));
    }
    Ok(Value::Array(
        source
            .split(&delim)
            .map(|s| Value::String(s.to_string()))
            .collect(),
    ))
}

fn op_util_join(args: &[Value], ctx: &mut Context) -> EvalResult {
    let values = evaluate(require_arg(args, 0, "util.join")?, ctx)?;
    let delim = value_to_string(evaluate(require_arg(args, 1, "util.join")?, ctx)?);
    let Some(arr) = values.as_array() else {
        return Ok(Value::String(value_to_string(values)));
    };
    let out = arr
        .iter()
        .cloned()
        .map(value_to_string)
        .collect::<Vec<_>>()
        .join(&delim);
    Ok(Value::String(out))
}

fn op_util_regex_match(args: &[Value], ctx: &mut Context) -> EvalResult {
    let source = value_to_string(evaluate(require_arg(args, 0, "util.regex_match")?, ctx)?);
    let pattern = value_to_string(evaluate(require_arg(args, 1, "util.regex_match")?, ctx)?);
    let opts_val = if let Some(v) = args.get(2) {
        Some(evaluate(v, ctx)?)
    } else {
        None
    };
    let opts = parse_regex_opts(opts_val.as_ref());
    let re = build_regex(&pattern, &opts)?;
    Ok(Value::Bool(re.is_match(&source)))
}

fn op_util_regex_find(args: &[Value], ctx: &mut Context) -> EvalResult {
    let source = value_to_string(evaluate(require_arg(args, 0, "util.regex_find")?, ctx)?);
    let pattern = value_to_string(evaluate(require_arg(args, 1, "util.regex_find")?, ctx)?);
    let opts_val = if let Some(v) = args.get(2) {
        Some(evaluate(v, ctx)?)
    } else {
        None
    };
    let opts = parse_regex_opts(opts_val.as_ref());
    let re = build_regex(&pattern, &opts)?;
    Ok(re
        .find(&source)
        .map(|m| Value::String(m.as_str().to_string()))
        .unwrap_or(Value::Null))
}

fn op_util_regex_find_all(args: &[Value], ctx: &mut Context) -> EvalResult {
    let source = value_to_string(evaluate(require_arg(args, 0, "util.regex_find_all")?, ctx)?);
    let pattern = value_to_string(evaluate(require_arg(args, 1, "util.regex_find_all")?, ctx)?);
    let opts_val = if let Some(v) = args.get(2) {
        Some(evaluate(v, ctx)?)
    } else {
        None
    };
    let opts = parse_regex_opts(opts_val.as_ref());
    let re = build_regex(&pattern, &opts)?;
    Ok(Value::Array(
        re.find_iter(&source)
            .map(|m| Value::String(m.as_str().to_string()))
            .collect(),
    ))
}

fn op_util_regex_replace(args: &[Value], ctx: &mut Context) -> EvalResult {
    let source = value_to_string(evaluate(require_arg(args, 0, "util.regex_replace")?, ctx)?);
    let pattern = value_to_string(evaluate(require_arg(args, 1, "util.regex_replace")?, ctx)?);
    let replacement = value_to_string(evaluate(require_arg(args, 2, "util.regex_replace")?, ctx)?);
    let opts_val = if let Some(v) = args.get(3) {
        Some(evaluate(v, ctx)?)
    } else {
        None
    };
    let opts = parse_regex_opts(opts_val.as_ref());
    let re = build_regex(&pattern, &opts)?;

    let out = if opts.all {
        re.replace_all(&source, replacement.as_str()).to_string()
    } else {
        re.replace(&source, replacement.as_str()).to_string()
    };
    Ok(Value::String(out))
}

fn op_data_select(args: &[Value], ctx: &mut Context) -> EvalResult {
    let rows = evaluate(require_arg(args, 0, "data.select")?, ctx)?;
    let fields = evaluate(require_arg(args, 1, "data.select")?, ctx)?;
    let arr = rows
        .as_array()
        .ok_or_else(|| EvalError::new("data.select first arg must be array"))?;
    let fields = fields
        .as_array()
        .ok_or_else(|| EvalError::new("data.select second arg must be array"))?;

    let mut parsed_fields: Vec<Vec<PathSegment>> = Vec::new();
    for field in fields {
        if let Some(path) = field.as_str() {
            if let Some(segs) = parse_path(path) {
                parsed_fields.push(segs);
            }
        }
    }

    let mut out = Vec::with_capacity(arr.len());
    for row in arr {
        let mut next = Value::Object(Map::new());
        for segs in &parsed_fields {
            let val = value_get_by_segments(row, segs).unwrap_or(Value::Null);
            value_set_by_segments(&mut next, segs, val);
        }
        out.push(next);
    }
    Ok(Value::Array(out))
}

fn op_data_rename(args: &[Value], ctx: &mut Context) -> EvalResult {
    let rows = evaluate(require_arg(args, 0, "data.rename")?, ctx)?;
    let mapping = evaluate(require_arg(args, 1, "data.rename")?, ctx)?;
    let arr = rows
        .as_array()
        .ok_or_else(|| EvalError::new("data.rename first arg must be array"))?;
    let mapping = mapping
        .as_object()
        .ok_or_else(|| EvalError::new("data.rename second arg must be object"))?;

    let mut pairs = Vec::new();
    for (from, to) in mapping {
        let Some(to_path) = to.as_str() else {
            continue;
        };
        let (Some(from_segs), Some(to_segs)) = (parse_path(from), parse_path(to_path)) else {
            continue;
        };
        pairs.push((from_segs, to_segs));
    }

    let mut out = Vec::with_capacity(arr.len());
    for row in arr {
        let mut next = row.clone();
        for (from, to) in &pairs {
            if let Some(v) = value_take_by_segments(&mut next, from) {
                value_set_by_segments(&mut next, to, v);
            }
        }
        out.push(next);
    }

    Ok(Value::Array(out))
}

fn op_data_cast(args: &[Value], ctx: &mut Context) -> EvalResult {
    let rows = evaluate(require_arg(args, 0, "data.cast")?, ctx)?;
    let mapping = evaluate(require_arg(args, 1, "data.cast")?, ctx)?;
    let arr = rows
        .as_array()
        .ok_or_else(|| EvalError::new("data.cast first arg must be array"))?;
    let mapping = mapping
        .as_object()
        .ok_or_else(|| EvalError::new("data.cast second arg must be object"))?;

    let mut casts = Vec::new();
    for (field, ty) in mapping {
        let Some(ty_name) = ty.as_str() else {
            continue;
        };
        let Some(segs) = parse_path(field) else {
            continue;
        };
        casts.push((segs, ty_name.to_string()));
    }

    let mut out = Vec::with_capacity(arr.len());
    for row in arr {
        let mut next = row.clone();
        for (segs, ty) in &casts {
            if let Some(v) = value_get_by_segments(&next, segs) {
                let casted = cast_value(v, ty);
                value_set_by_segments(&mut next, segs, casted);
            }
        }
        out.push(next);
    }
    Ok(Value::Array(out))
}

fn op_data_chunk(args: &[Value], ctx: &mut Context) -> EvalResult {
    let rows = evaluate(require_arg(args, 0, "data.chunk")?, ctx)?;
    let size = evaluate(require_arg(args, 1, "data.chunk")?, ctx)?;
    let arr = rows
        .as_array()
        .ok_or_else(|| EvalError::new("data.chunk first arg must be array"))?;
    let n = as_i64(&size).unwrap_or(0);
    if n <= 0 {
        return Err(EvalError::new("data.chunk size must be > 0"));
    }
    let n = n as usize;

    let mut out = Vec::new();
    for chunk in arr.chunks(n) {
        out.push(Value::Array(chunk.to_vec()));
    }
    Ok(Value::Array(out))
}

fn op_data_flat_map(args: &[Value], ctx: &mut Context) -> EvalResult {
    let rows = evaluate(require_arg(args, 0, "data.flat_map")?, ctx)?;
    let path = evaluate(require_arg(args, 1, "data.flat_map")?, ctx)?;
    let arr = rows
        .as_array()
        .ok_or_else(|| EvalError::new("data.flat_map first arg must be array"))?;
    let Some(path) = path.as_str() else {
        return Err(EvalError::new(
            "data.flat_map second arg must be path string",
        ));
    };
    let segs = parse_path(path).ok_or_else(|| EvalError::new("data.flat_map path is invalid"))?;

    let mut out = Vec::new();
    for row in arr {
        let Some(value) = value_get_by_segments(row, &segs) else {
            continue;
        };
        match value {
            Value::Array(items) => {
                for item in items {
                    let mut next = row.clone();
                    value_set_by_segments(&mut next, &segs, item);
                    out.push(next);
                }
            }
            Value::Null => {}
            other => {
                let mut next = row.clone();
                value_set_by_segments(&mut next, &segs, other);
                out.push(next);
            }
        }
    }

    Ok(Value::Array(out))
}

fn op_data_distinct(args: &[Value], ctx: &mut Context) -> EvalResult {
    let rows = evaluate(require_arg(args, 0, "data.distinct")?, ctx)?;
    let arr = rows
        .as_array()
        .ok_or_else(|| EvalError::new("data.distinct first arg must be array"))?;

    let key_paths = if let Some(arg) = args.get(1) {
        let keys = evaluate(arg, ctx)?;
        keys.as_array().map(|list| {
            list.iter()
                .filter_map(|v| v.as_str())
                .filter_map(parse_path)
                .collect::<Vec<_>>()
        })
    } else {
        None
    };

    let mut seen = HashSet::new();
    let mut out = Vec::new();
    for row in arr {
        let sig = if let Some(paths) = &key_paths {
            let key_vals: Vec<Value> = paths
                .iter()
                .map(|segs| value_get_by_segments(row, segs).unwrap_or(Value::Null))
                .collect();
            serde_json::to_string(&key_vals).unwrap_or_else(|_| "[]".to_string())
        } else {
            serde_json::to_string(row).unwrap_or_else(|_| "null".to_string())
        };

        if seen.insert(sig) {
            out.push(row.clone());
        }
    }

    Ok(Value::Array(out))
}

fn op_data_sort_by(args: &[Value], ctx: &mut Context) -> EvalResult {
    let rows = evaluate(require_arg(args, 0, "data.sort_by")?, ctx)?;
    let spec = evaluate(require_arg(args, 1, "data.sort_by")?, ctx)?;
    let arr = rows
        .as_array()
        .ok_or_else(|| EvalError::new("data.sort_by first arg must be array"))?;
    let spec_arr = spec
        .as_array()
        .ok_or_else(|| EvalError::new("data.sort_by second arg must be array"))?;

    let mut order_specs: Vec<(Vec<PathSegment>, bool)> = Vec::new();
    for s in spec_arr {
        match s {
            Value::String(path) => {
                if let Some(segs) = parse_path(path) {
                    order_specs.push((segs, true));
                }
            }
            Value::Object(obj) => {
                let Some(col) = obj.get("col").and_then(Value::as_str) else {
                    continue;
                };
                let asc = !obj
                    .get("dir")
                    .and_then(Value::as_str)
                    .map(|d| d.eq_ignore_ascii_case("desc"))
                    .unwrap_or(false);
                if let Some(segs) = parse_path(col) {
                    order_specs.push((segs, asc));
                }
            }
            _ => {}
        }
    }

    let mut out = arr.clone();
    out.sort_by(|a, b| {
        for (segs, asc) in &order_specs {
            let av = value_get_by_segments(a, segs).unwrap_or(Value::Null);
            let bv = value_get_by_segments(b, segs).unwrap_or(Value::Null);
            let ord = cmp_numbers_or_strings(&av, &bv).unwrap_or(std::cmp::Ordering::Equal);
            if ord != std::cmp::Ordering::Equal {
                return if *asc { ord } else { ord.reverse() };
            }
        }
        std::cmp::Ordering::Equal
    });

    Ok(Value::Array(out))
}

fn op_data_group_by(args: &[Value], ctx: &mut Context) -> EvalResult {
    let rows = evaluate(require_arg(args, 0, "data.group_by")?, ctx)?;
    let keys = evaluate(require_arg(args, 1, "data.group_by")?, ctx)?;
    let arr = rows
        .as_array()
        .ok_or_else(|| EvalError::new("data.group_by first arg must be array"))?;
    let key_list = keys
        .as_array()
        .ok_or_else(|| EvalError::new("data.group_by second arg must be array"))?;

    let mut parsed_keys: Vec<(String, Vec<PathSegment>)> = Vec::new();
    for k in key_list {
        let Some(path) = k.as_str() else {
            continue;
        };
        if let Some(segs) = parse_path(path) {
            parsed_keys.push((path.to_string(), segs));
        }
    }

    let mut order: Vec<(Map<String, Value>, Vec<Value>)> = Vec::new();
    let mut index: HashMap<String, usize> = HashMap::new();
    for row in arr {
        let mut key_obj = Map::new();
        let mut key_vals = Vec::new();
        for (raw, segs) in &parsed_keys {
            let v = value_get_by_segments(row, segs).unwrap_or(Value::Null);
            key_vals.push(v.clone());
            key_obj.insert(raw.clone(), v);
        }
        let sig = serde_json::to_string(&key_vals).unwrap_or_else(|_| "[]".to_string());
        if let Some(i) = index.get(&sig).copied() {
            order[i].1.push(row.clone());
        } else {
            index.insert(sig, order.len());
            order.push((key_obj, vec![row.clone()]));
        }
    }

    let mut out = Vec::with_capacity(order.len());
    for (keys_obj, grouped_rows) in order {
        let mut obj = keys_obj;
        obj.insert("_rows".to_string(), Value::Array(grouped_rows));
        out.push(Value::Object(obj));
    }
    Ok(Value::Array(out))
}

fn op_data_aggregate(args: &[Value], ctx: &mut Context) -> EvalResult {
    let groups = evaluate(require_arg(args, 0, "data.aggregate")?, ctx)?;
    let spec = evaluate(require_arg(args, 1, "data.aggregate")?, ctx)?;
    let groups_arr = groups
        .as_array()
        .ok_or_else(|| EvalError::new("data.aggregate first arg must be array"))?;
    let spec_obj = spec
        .as_object()
        .ok_or_else(|| EvalError::new("data.aggregate second arg must be object"))?;

    let mut parsed_spec: Vec<(String, AggExpr)> = Vec::new();
    for (out_key, expr_val) in spec_obj {
        let Some(expr) = expr_val.as_str() else {
            continue;
        };
        if let Some(parsed) = parse_agg_expr(expr) {
            parsed_spec.push((out_key.clone(), parsed));
        }
    }

    let mut out = Vec::with_capacity(groups_arr.len());
    for group in groups_arr {
        let Some(group_obj) = group.as_object() else {
            continue;
        };
        let rows = group_obj
            .get("_rows")
            .and_then(Value::as_array)
            .cloned()
            .unwrap_or_default();

        let mut next = Map::new();
        for (k, v) in group_obj {
            if k != "_rows" {
                next.insert(k.clone(), v.clone());
            }
        }

        for (out_key, expr) in &parsed_spec {
            let value = match expr {
                AggExpr::CountAll => json!(rows.len()),
                AggExpr::CountField(path) => {
                    let n = rows
                        .iter()
                        .filter(|r| value_get_by_segments(r, path).is_some())
                        .count();
                    json!(n)
                }
                AggExpr::Sum(path) => {
                    let mut sum = 0.0f64;
                    for r in &rows {
                        if let Some(v) = value_get_by_segments(r, path) {
                            sum += as_f64(&v).unwrap_or(0.0);
                        }
                    }
                    json!(sum)
                }
                AggExpr::Avg(path) => {
                    let mut sum = 0.0f64;
                    let mut n = 0usize;
                    for r in &rows {
                        if let Some(v) = value_get_by_segments(r, path) {
                            sum += as_f64(&v).unwrap_or(0.0);
                            n += 1;
                        }
                    }
                    json!(if n == 0 { 0.0 } else { sum / n as f64 })
                }
                AggExpr::Min(path) => {
                    let mut best: Option<Value> = None;
                    for r in &rows {
                        if let Some(v) = value_get_by_segments(r, path) {
                            if let Some(cur) = &best {
                                if cmp_numbers_or_strings(&v, cur).is_some_and(|ord| ord.is_lt()) {
                                    best = Some(v);
                                }
                            } else {
                                best = Some(v);
                            }
                        }
                    }
                    best.unwrap_or(Value::Null)
                }
                AggExpr::Max(path) => {
                    let mut best: Option<Value> = None;
                    for r in &rows {
                        if let Some(v) = value_get_by_segments(r, path) {
                            if let Some(cur) = &best {
                                if cmp_numbers_or_strings(&v, cur).is_some_and(|ord| ord.is_gt()) {
                                    best = Some(v);
                                }
                            } else {
                                best = Some(v);
                            }
                        }
                    }
                    best.unwrap_or(Value::Null)
                }
            };
            next.insert(out_key.clone(), value);
        }

        out.push(Value::Object(next));
    }

    Ok(Value::Array(out))
}

fn op_data_join(args: &[Value], ctx: &mut Context) -> EvalResult {
    let left = evaluate(require_arg(args, 0, "data.join")?, ctx)?;
    let right = evaluate(require_arg(args, 1, "data.join")?, ctx)?;
    let options = evaluate(require_arg(args, 2, "data.join")?, ctx)?;
    let left_arr = left
        .as_array()
        .ok_or_else(|| EvalError::new("data.join first arg must be array"))?;
    let right_arr = right
        .as_array()
        .ok_or_else(|| EvalError::new("data.join second arg must be array"))?;
    let options = options
        .as_object()
        .ok_or_else(|| EvalError::new("data.join third arg must be object"))?;

    let on_fields = options
        .get("on")
        .and_then(Value::as_array)
        .ok_or_else(|| EvalError::new("data.join requires options.on array"))?;
    let join_type = options
        .get("type")
        .and_then(Value::as_str)
        .unwrap_or("inner")
        .to_ascii_lowercase();
    let is_left_join = join_type == "left";

    let mut parsed_on = Vec::new();
    for f in on_fields {
        let Some(path) = f.as_str() else {
            continue;
        };
        if let Some(segs) = parse_path(path) {
            parsed_on.push((path.to_string(), segs));
        }
    }

    let mut right_index: HashMap<String, Vec<Value>> = HashMap::new();
    for row in right_arr {
        let key_vals: Vec<Value> = parsed_on
            .iter()
            .map(|(_, segs)| value_get_by_segments(row, segs).unwrap_or(Value::Null))
            .collect();
        let sig = serde_json::to_string(&key_vals).unwrap_or_else(|_| "[]".to_string());
        right_index.entry(sig).or_default().push(row.clone());
    }

    let mut out = Vec::new();
    for l in left_arr {
        let key_vals: Vec<Value> = parsed_on
            .iter()
            .map(|(_, segs)| value_get_by_segments(l, segs).unwrap_or(Value::Null))
            .collect();
        let sig = serde_json::to_string(&key_vals).unwrap_or_else(|_| "[]".to_string());
        let matched = right_index.get(&sig);

        if let Some(rs) = matched {
            for r in rs {
                let mut merged = l.clone();
                if let (Some(lo), Some(ro)) = (merged.as_object_mut(), r.as_object()) {
                    for (k, v) in ro {
                        if lo.contains_key(k) {
                            lo.insert(format!("right_{k}"), v.clone());
                        } else {
                            lo.insert(k.clone(), v.clone());
                        }
                    }
                }
                out.push(merged);
            }
        } else if is_left_join {
            out.push(l.clone());
        }
    }

    Ok(Value::Array(out))
}

fn op_log(args: &[Value], ctx: &mut Context) -> EvalResult {
    let mut out = Vec::with_capacity(args.len());
    for arg in args {
        out.push(evaluate(arg, ctx)?);
    }
    eprintln!("[josie.log] {}", Value::Array(out.clone()));
    Ok(Value::Array(out))
}

fn op_len(args: &[Value], ctx: &mut Context) -> EvalResult {
    let v = evaluate(require_arg(args, 0, "len")?, ctx)?;
    let n = match v {
        Value::String(s) => s.chars().count(),
        Value::Array(a) => a.len(),
        Value::Object(o) => o.len(),
        _ => 0,
    };
    Ok(json!(n))
}

fn op_push(args: &[Value], ctx: &mut Context) -> EvalResult {
    let list = evaluate(require_arg(args, 0, "push")?, ctx)?;
    let item = evaluate(require_arg(args, 1, "push")?, ctx)?;
    let mut arr = match list {
        Value::Array(a) => a,
        _ => Vec::new(),
    };
    arr.push(item);
    Ok(Value::Array(arr))
}

fn op_get(args: &[Value], ctx: &mut Context) -> EvalResult {
    let collection = evaluate(require_arg(args, 0, "get")?, ctx)?;
    let key = evaluate(require_arg(args, 1, "get")?, ctx)?;
    let out = match (collection, key) {
        (Value::Object(obj), Value::String(k)) => obj.get(&k).cloned(),
        (Value::Array(arr), Value::Number(n)) => {
            n.as_u64().and_then(|i| arr.get(i as usize).cloned())
        }
        (Value::Array(arr), Value::String(s)) => {
            s.parse::<usize>().ok().and_then(|i| arr.get(i).cloned())
        }
        _ => None,
    };
    Ok(out.unwrap_or(Value::Null))
}

fn op_do(args: &[Value], ctx: &mut Context) -> EvalResult {
    let mut last = Value::Null;
    for arg in args {
        last = evaluate(arg, ctx)?;
    }
    Ok(last)
}

fn op_pipe(args: &[Value], ctx: &mut Context) -> EvalResult {
    let prev_pipe = ctx.state.client.get("pipe").cloned();
    let out = (|| {
        let mut last = Value::Null;
        for step in args {
            let mut pipe_obj = Map::new();
            pipe_obj.insert("prev".to_string(), last.clone());
            ctx.state
                .client
                .insert("pipe".to_string(), Value::Object(pipe_obj));
            last = evaluate(step, ctx)?;
        }
        Ok(last)
    })();

    if let Some(old) = prev_pipe {
        ctx.state.client.insert("pipe".to_string(), old);
    } else {
        ctx.state.client.remove("pipe");
    }
    out
}

fn op_def(args: &[Value], ctx: &mut Context) -> EvalResult {
    let name = require_arg(args, 0, "def")?
        .as_str()
        .ok_or_else(|| EvalError::new("def name must be string"))?
        .to_string();
    let params = args
        .get(1)
        .and_then(Value::as_array)
        .map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str().map(ToString::to_string))
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();
    let body = require_arg(args, 2, "def")?.clone();
    ctx.state.fns.insert(name, FunctionDef { params, body });
    Ok(Value::Null)
}

fn op_call(args: &[Value], ctx: &mut Context) -> EvalResult {
    let name_val = evaluate(require_arg(args, 0, "call")?, ctx)?;
    let name = name_val
        .as_str()
        .ok_or_else(|| EvalError::new("call function name must be string"))?
        .to_string();

    if let Some(def) = ctx.state.fns.get(&name).cloned() {
        let mut evaluated_args = Vec::new();
        for arg in &args[1..] {
            evaluated_args.push(evaluate(arg, ctx)?);
        }
        let mut call_state = ctx.state.clone();
        for (idx, param) in def.params.iter().enumerate() {
            if let Some(v) = evaluated_args.get(idx) {
                call_state.client.insert(param.clone(), v.clone());
            }
        }
        let mut call_ctx = Context {
            state: &mut call_state,
            operators: ctx.operators,
            event: ctx.event,
        };
        return evaluate(&def.body, &mut call_ctx);
    }

    if let Some(op) = ctx.operators.get(&name) {
        return op(&args[1..], ctx);
    }

    Err(EvalError::new(format!("function '{name}' not found")))
}

fn with_item_scope<F>(ctx: &mut Context, item: Value, index: usize, f: F) -> EvalResult
where
    F: FnOnce(&mut Context) -> EvalResult,
{
    let old_item = ctx.state.client.insert("item".to_string(), item);
    let old_index = ctx.state.client.insert("index".to_string(), json!(index));
    let out = f(ctx);
    if let Some(prev) = old_item {
        ctx.state.client.insert("item".to_string(), prev);
    } else {
        ctx.state.client.remove("item");
    }
    if let Some(prev) = old_index {
        ctx.state.client.insert("index".to_string(), prev);
    } else {
        ctx.state.client.remove("index");
    }
    out
}

fn op_map(args: &[Value], ctx: &mut Context) -> EvalResult {
    let list = evaluate(require_arg(args, 0, "map")?, ctx)?;
    let expr = require_arg(args, 1, "map")?.clone();
    let arr = list
        .as_array()
        .cloned()
        .ok_or_else(|| EvalError::new("map first arg must evaluate to array"))?;
    let mut out = Vec::with_capacity(arr.len());
    for (i, item) in arr.into_iter().enumerate() {
        out.push(with_item_scope(ctx, item, i, |inner| {
            evaluate(&expr, inner)
        })?);
    }
    Ok(Value::Array(out))
}

fn op_filter(args: &[Value], ctx: &mut Context) -> EvalResult {
    let list = evaluate(require_arg(args, 0, "filter")?, ctx)?;
    let expr = require_arg(args, 1, "filter")?.clone();
    let arr = list
        .as_array()
        .cloned()
        .ok_or_else(|| EvalError::new("filter first arg must evaluate to array"))?;
    let mut out = Vec::new();
    for (i, item) in arr.into_iter().enumerate() {
        let keep = with_item_scope(ctx, item.clone(), i, |inner| evaluate(&expr, inner))?;
        if truthy(&keep) {
            out.push(item);
        }
    }
    Ok(Value::Array(out))
}

fn op_match(args: &[Value], ctx: &mut Context) -> EvalResult {
    let value = evaluate(require_arg(args, 0, "match")?, ctx)?;
    let mut i = 1usize;
    while i + 1 < args.len() {
        let pat = evaluate(&args[i], ctx)?;
        if pat == Value::String("_".to_string()) || pat == value {
            return evaluate(&args[i + 1], ctx);
        }
        i += 2;
    }
    Ok(Value::Null)
}

fn op_event_value(_args: &[Value], ctx: &mut Context) -> EvalResult {
    Ok(ctx.event.map(|e| e.value.clone()).unwrap_or(Value::Null))
}

fn op_event_key(_args: &[Value], ctx: &mut Context) -> EvalResult {
    Ok(ctx.event.map(|e| e.key.clone()).unwrap_or(Value::Null))
}

fn op_event_prevent(_args: &[Value], ctx: &mut Context) -> EvalResult {
    Ok(Value::Bool(ctx.event.map(|e| e.prevent).unwrap_or(false)))
}

fn op_effect(args: &[Value], ctx: &mut Context) -> EvalResult {
    let mut last = Value::Null;
    for arg in args {
        last = evaluate(arg, ctx)?;
    }
    Ok(last)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn eval_expr(expr: Value) -> Value {
        let mut state = State::new();
        let operators = Operators::new();
        let mut ctx = Context {
            state: &mut state,
            operators: &operators,
            event: None,
        };
        evaluate(&expr, &mut ctx).expect("evaluate")
    }

    #[test]
    fn util_path_ops_work() {
        let payload = json!({
            "headers": {
                "Authorization": "Bearer A"
            },
            "user": {
                "id": 7,
                "profile": {
                    "email": "x@example.com"
                }
            },
            "items": [{"sku":"a"}, {"sku":"b"}]
        });

        let v = eval_expr(json!([
            "util.get_path",
            payload.clone(),
            "user.profile.email",
            null
        ]));
        assert_eq!(v, json!("x@example.com"));

        let v = eval_expr(json!([
            "util.one_of",
            payload.clone(),
            [
                "headers.authorization",
                "headers.Authorization",
                "header.auth"
            ],
            null
        ]));
        assert_eq!(v, json!("Bearer A"));

        let v = eval_expr(json!([
            "util.pick",
            payload,
            ["user.id", "user.profile.email", "items[1].sku"]
        ]));
        assert_eq!(
            v,
            json!({
                "user": {
                    "id": 7,
                    "profile": { "email": "x@example.com" }
                },
                "items": [null, {"sku":"b"}]
            })
        );

        let v = eval_expr(json!(["util.exists", json!({"a":{"b":[1,2,3]}}), "a.b[1]"]));
        assert_eq!(v, json!(true));

        let v = eval_expr(json!([
            "util.omit",
            json!({"a":1,"b":{"c":2},"arr":[1,2,3]}),
            ["b.c", "arr[1]"]
        ]));
        assert_eq!(v, json!({"a":1,"b":{},"arr":[1,null,3]}));
    }

    #[test]
    fn util_replace_ops_work() {
        let v = eval_expr(json!(["util.replace", "a-b-c", "-", "_"]));
        assert_eq!(v, json!("a_b-c"));

        let v = eval_expr(json!(["util.replace_all", "a-b-c", "-", "_"]));
        assert_eq!(v, json!("a_b_c"));

        let v = eval_expr(json!(["util.as_array", 9]));
        assert_eq!(v, json!([9]));

        let v = eval_expr(json!(["util.to_bool", "YES"]));
        assert_eq!(v, json!(true));

        let v = eval_expr(json!(["util.split", "a,b,c", ","]));
        assert_eq!(v, json!(["a", "b", "c"]));

        let v = eval_expr(json!(["util.join", ["a", 2, true], "-"]));
        assert_eq!(v, json!("a-2-true"));
    }

    #[test]
    fn util_regex_ops_work() {
        let v = eval_expr(json!(["util.regex_match", "HELLO", "^hello$", {"case_insensitive": true}]));
        assert_eq!(v, json!(true));

        let v = eval_expr(json!(["util.regex_find", "order-123-x", "\\d+"]));
        assert_eq!(v, json!("123"));

        let v = eval_expr(json!(["util.regex_find_all", "a1b22c333", "\\d+"]));
        assert_eq!(v, json!(["1", "22", "333"]));

        let v = eval_expr(json!(["util.regex_replace", "a1b2", "\\d+", "X"]));
        assert_eq!(v, json!("aXbX"));

        let v = eval_expr(json!(["util.regex_replace", "a1b2", "\\d+", "X", {"all": false}]));
        assert_eq!(v, json!("aXb2"));
    }

    #[test]
    fn data_ops_work() {
        let rows = json!([
            {"id":"1","amount":"12.5","name":"ALICE","extra":{"k":"x"}},
            {"id":"2","amount":"7","name":"BOB","extra":{"k":"y"}}
        ]);

        let v = eval_expr(json!([
            "data.select",
            rows.clone(),
            ["id", "extra.k", "missing"]
        ]));
        assert_eq!(
            v,
            json!([
                {"id":"1","extra":{"k":"x"},"missing":null},
                {"id":"2","extra":{"k":"y"},"missing":null}
            ])
        );

        let v =
            eval_expr(json!(["data.rename", rows.clone(), {"name":"full_name", "extra.k":"kind"}]));
        assert_eq!(
            v,
            json!([
                {"id":"1","amount":"12.5","full_name":"ALICE","extra":{},"kind":"x"},
                {"id":"2","amount":"7","full_name":"BOB","extra":{},"kind":"y"}
            ])
        );

        let v = eval_expr(json!(["data.cast", rows, {"id":"int", "amount":"float"}]));
        assert_eq!(
            v,
            json!([
                {"id":1,"amount":12.5,"name":"ALICE","extra":{"k":"x"}},
                {"id":2,"amount":7.0,"name":"BOB","extra":{"k":"y"}}
            ])
        );

        let v = eval_expr(json!(["data.chunk", [1, 2, 3, 4, 5], 2]));
        assert_eq!(v, json!([[1, 2], [3, 4], [5]]));

        let v = eval_expr(json!([
            "data.flat_map",
            [
                {"id":1,"items":[{"sku":"a"},{"sku":"b"}]},
                {"id":2,"items":[{"sku":"c"}]}
            ],
            "items"
        ]));
        assert_eq!(
            v,
            json!([
                {"id":1,"items":{"sku":"a"}},
                {"id":1,"items":{"sku":"b"}},
                {"id":2,"items":{"sku":"c"}}
            ])
        );

        let v = eval_expr(json!([
            "data.distinct",
            [
                {"id":1,"email":"a@x"},
                {"id":2,"email":"a@x"},
                {"id":3,"email":"b@x"}
            ],
            ["email"]
        ]));
        assert_eq!(
            v,
            json!([
                {"id":1,"email":"a@x"},
                {"id":3,"email":"b@x"}
            ])
        );

        let v = eval_expr(json!([
            "data.sort_by",
            [
                {"id":1,"score":9},
                {"id":2,"score":3},
                {"id":3,"score":7}
            ],
            [{"col":"score","dir":"desc"}]
        ]));
        assert_eq!(
            v,
            json!([
                {"id":1,"score":9},
                {"id":3,"score":7},
                {"id":2,"score":3}
            ])
        );

        let grouped = eval_expr(json!([
            "data.group_by",
            [
                {"user":"u1","amount":10},
                {"user":"u1","amount":5},
                {"user":"u2","amount":3}
            ],
            ["user"]
        ]));
        let v = eval_expr(json!([
            "data.aggregate",
            grouped,
            {"n":"count(*)","total":"sum(amount)"}
        ]));
        assert_eq!(
            v,
            json!([
                {"user":"u1","n":2,"total":15.0},
                {"user":"u2","n":1,"total":3.0}
            ])
        );

        let v = eval_expr(json!([
            "data.join",
            [
                {"user":"u1","l":1},
                {"user":"u2","l":2}
            ],
            [
                {"user":"u1","r":"A"}
            ],
            {"on":["user"],"type":"left"}
        ]));
        assert_eq!(
            v,
            json!([
                {"user":"u1","l":1,"right_user":"u1","r":"A"},
                {"user":"u2","l":2}
            ])
        );
    }

    #[test]
    fn pipe_restores_scope_on_error_without_previous_pipe() {
        let mut state = State::new();
        state.client.insert("stable".to_string(), json!("ok"));
        let operators = Operators::new();
        let mut ctx = Context {
            state: &mut state,
            operators: &operators,
            event: None,
        };
        let out = evaluate(
            &json!(["pipe", ["set", "x", 1], ["call", "missing_function"]]),
            &mut ctx,
        );
        assert!(out.is_err());
        assert!(ctx.state.client.get("pipe").is_none());
        assert_eq!(ctx.state.client.get("stable"), Some(&json!("ok")));
    }

    #[test]
    fn pipe_restores_previous_pipe_on_error() {
        let mut state = State::new();
        let prev = json!({"prev":"seed"});
        state.client.insert("pipe".to_string(), prev.clone());
        let operators = Operators::new();
        let mut ctx = Context {
            state: &mut state,
            operators: &operators,
            event: None,
        };
        let out = evaluate(&json!(["pipe", ["call", "missing_function"]]), &mut ctx);
        assert!(out.is_err());
        assert_eq!(ctx.state.client.get("pipe"), Some(&prev));
    }
}
