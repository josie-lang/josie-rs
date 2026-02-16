use serde::{Deserialize, Serialize};
use serde_json::{Map, Value, json};
use std::collections::HashMap;

pub type JsonNode = Value;
pub type EvalResult = Result<Value, EvalError>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EvalError {
    pub message: String,
}

impl EvalError {
    pub fn new(msg: impl Into<String>) -> Self {
        Self {
            message: msg.into(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub params: Vec<String>,
    pub body: Value,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct State {
    pub server: Map<String, Value>,
    pub client: Map<String, Value>,
    #[serde(skip)]
    pub fns: HashMap<String, FunctionDef>,
}

impl State {
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

pub struct Context<'a> {
    pub state: &'a mut State,
    pub operators: &'a Operators,
    pub event: Option<&'a EventContext>,
}

pub type Operator = fn(args: &[Value], ctx: &mut Context) -> EvalResult;

pub struct Operators {
    ops: HashMap<String, Operator>,
}

impl Operators {
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
        ops.insert("u.concat".into(), op_concat as Operator);
        ops.insert("u.lower".into(), op_lower as Operator);
        ops.insert("u.upper".into(), op_upper as Operator);
        ops.insert("u.contains".into(), op_contains as Operator);
        ops.insert("u.template".into(), op_template as Operator);
        ops.insert("u.to_int".into(), op_to_int as Operator);
        ops.insert("u.to_float".into(), op_to_float as Operator);
        ops.insert("u.to_string".into(), op_to_string as Operator);
        ops.insert("u.trim".into(), op_trim as Operator);
        ops.insert("u.str_len".into(), op_str_len as Operator);
        ops.insert("log".into(), op_log as Operator);
        ops.insert("len".into(), op_len as Operator);
        ops.insert("push".into(), op_push as Operator);
        ops.insert("get".into(), op_get as Operator);
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

    pub fn get(&self, name: &str) -> Option<Operator> {
        self.ops.get(name).copied()
    }

    pub fn register(&mut self, name: impl Into<String>, operator: Operator) -> Option<Operator> {
        self.ops.insert(name.into(), operator)
    }
}

impl Default for Operators {
    fn default() -> Self {
        Self::new()
    }
}

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

fn cmp_numbers_or_strings(a: &Value, b: &Value) -> Option<std::cmp::Ordering> {
    match (as_f64(a), as_f64(b)) {
        (Some(x), Some(y)) => x.partial_cmp(&y),
        _ => Some(a.to_string().cmp(&b.to_string())),
    }
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
    Ok(json!(as_f64(&first).unwrap_or(0.0) - as_f64(&second).unwrap_or(0.0)))
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
    let v = evaluate(require_arg(args, 0, "u.lower")?, ctx)?;
    Ok(Value::String(v.as_str().unwrap_or(&v.to_string()).to_lowercase()))
}

fn op_upper(args: &[Value], ctx: &mut Context) -> EvalResult {
    let v = evaluate(require_arg(args, 0, "u.upper")?, ctx)?;
    Ok(Value::String(v.as_str().unwrap_or(&v.to_string()).to_uppercase()))
}

fn op_contains(args: &[Value], ctx: &mut Context) -> EvalResult {
    let hay = evaluate(require_arg(args, 0, "u.contains")?, ctx)?;
    let needle = evaluate(require_arg(args, 1, "u.contains")?, ctx)?;
    let result = match hay {
        Value::String(s) => s.contains(needle.as_str().unwrap_or(&needle.to_string())),
        Value::Array(arr) => arr.contains(&needle),
        _ => false,
    };
    Ok(Value::Bool(result))
}

fn op_template(args: &[Value], ctx: &mut Context) -> EvalResult {
    let fmt = evaluate(require_arg(args, 0, "u.template")?, ctx)?;
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
    let v = evaluate(require_arg(args, 0, "u.to_int")?, ctx)?;
    Ok(json!(as_i64(&v).unwrap_or(0)))
}

fn op_to_float(args: &[Value], ctx: &mut Context) -> EvalResult {
    let v = evaluate(require_arg(args, 0, "u.to_float")?, ctx)?;
    Ok(json!(as_f64(&v).unwrap_or(0.0)))
}

fn op_to_string(args: &[Value], ctx: &mut Context) -> EvalResult {
    let v = evaluate(require_arg(args, 0, "u.to_string")?, ctx)?;
    Ok(Value::String(match v {
        Value::String(s) => s,
        Value::Null => String::new(),
        other => other.to_string(),
    }))
}

fn op_trim(args: &[Value], ctx: &mut Context) -> EvalResult {
    let v = evaluate(require_arg(args, 0, "u.trim")?, ctx)?;
    Ok(Value::String(
        v.as_str().unwrap_or(&v.to_string()).trim().to_string(),
    ))
}

fn op_str_len(args: &[Value], ctx: &mut Context) -> EvalResult {
    let v = evaluate(require_arg(args, 0, "u.str_len")?, ctx)?;
    Ok(json!(v.as_str().unwrap_or(&v.to_string()).chars().count()))
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
        (Value::Array(arr), Value::Number(n)) => n.as_u64().and_then(|i| arr.get(i as usize).cloned()),
        (Value::Array(arr), Value::String(s)) => s.parse::<usize>().ok().and_then(|i| arr.get(i).cloned()),
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
    let mut last = Value::Null;
    for step in args {
        let mut pipe_obj = Map::new();
        pipe_obj.insert("prev".to_string(), last.clone());
        ctx.state
            .client
            .insert("pipe".to_string(), Value::Object(pipe_obj));
        last = evaluate(step, ctx)?;
    }
    if let Some(old) = prev_pipe {
        ctx.state.client.insert("pipe".to_string(), old);
    } else {
        ctx.state.client.remove("pipe");
    }
    Ok(last)
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
        out.push(with_item_scope(ctx, item, i, |inner| evaluate(&expr, inner))?);
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
    Ok(ctx
        .event
        .map(|e| e.value.clone())
        .unwrap_or(Value::Null))
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
