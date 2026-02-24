//! Typed VM evaluator for compiled [`Expr`](crate::compiler::Expr).
//!
//! This module is the hot execution loop for compiled pipeline expressions.
//! It avoids runtime string operator dispatch by matching over enum variants.

use crate::compiler::Expr;
use crate::jval::{JVal, format_f64};
use crate::runtime::{Context, EvalError, Operators, State};
use serde_json::Value;
use std::rc::Rc;

/// Iterator-local variables — direct struct field access, no HashMap.
#[derive(Debug, Clone)]
pub struct IterLocals {
    pub item: JVal,
    pub index: JVal,
    pub acc: JVal,
}

impl IterLocals {
    /// Create iterator locals with `item` and `index`.
    #[inline]
    pub fn new(item: JVal, index: i64) -> Self {
        Self {
            item,
            index: JVal::Int(index),
            acc: JVal::Null,
        }
    }

    /// Create iterator locals with explicit reduce accumulator.
    #[inline]
    pub fn with_acc(item: JVal, index: i64, acc: JVal) -> Self {
        Self {
            item,
            index: JVal::Int(index),
            acc,
        }
    }

    /// Create empty locals for non-iterator evaluation contexts.
    #[inline]
    pub fn empty() -> Self {
        Self {
            item: JVal::Null,
            index: JVal::Int(0),
            acc: JVal::Null,
        }
    }
}

/// Evaluate a compiled expression.
///
/// Direct match on Expr variants — zero HashMap dispatch in the hot path.
/// Falls back to the existing Operators table only for `Expr::Call`.
pub fn eval_expr(
    expr: &Expr,
    locals: &IterLocals,
    state: &mut State,
    operators: &Operators,
) -> Result<JVal, EvalError> {
    match expr {
        // === Literals ===
        Expr::Literal(v) => Ok(v.clone()),

        // === Locals (zero overhead) ===
        Expr::LocalItem => Ok(locals.item.clone()),
        Expr::LocalIndex => Ok(locals.index.clone()),
        Expr::LocalAcc => Ok(locals.acc.clone()),

        // === State variable access ===
        Expr::Var(path) => Ok(read_state_jval(state, path).unwrap_or(JVal::Null)),
        Expr::VarDynamic(path_expr) => {
            let path_val = eval_expr(path_expr, locals, state, operators)?;
            match path_val {
                JVal::Str(s) => Ok(read_state_jval(state, &s).unwrap_or(JVal::Null)),
                _ => Ok(JVal::Null),
            }
        }

        // === Arithmetic ===
        Expr::Add(args) => {
            if args.len() == 2 {
                let a = eval_expr(&args[0], locals, state, operators)?;
                let b = eval_expr(&args[1], locals, state, operators)?;
                return Ok(add_jvals(a, b));
            }
            let mut total_i = 0i64;
            let mut all_int = true;
            let mut total_f = 0.0f64;
            for arg in args {
                let v = eval_expr(arg, locals, state, operators)?;
                match &v {
                    JVal::Int(n) if all_int => {
                        total_i = total_i.wrapping_add(*n);
                        total_f += *n as f64;
                    }
                    _ => {
                        all_int = false;
                        total_f += v.as_f64().unwrap_or(0.0);
                    }
                }
            }
            Ok(if all_int {
                JVal::Int(total_i)
            } else {
                JVal::Float(total_f)
            })
        }

        Expr::Sub(a, b) => {
            let av = eval_expr(a, locals, state, operators)?;
            let bv = eval_expr(b, locals, state, operators)?;
            match (&av, &bv) {
                (JVal::Int(x), JVal::Int(y)) => Ok(JVal::Int(x.wrapping_sub(*y))),
                _ => Ok(JVal::Float(
                    av.as_f64().unwrap_or(0.0) - bv.as_f64().unwrap_or(0.0),
                )),
            }
        }

        Expr::Mul(args) => {
            if args.len() == 2 {
                let a = eval_expr(&args[0], locals, state, operators)?;
                let b = eval_expr(&args[1], locals, state, operators)?;
                return Ok(mul_jvals(a, b));
            }
            let mut prod_i = 1i64;
            let mut all_int = true;
            let mut prod_f = 1.0f64;
            for arg in args {
                let v = eval_expr(arg, locals, state, operators)?;
                match &v {
                    JVal::Int(n) if all_int => {
                        prod_i = prod_i.wrapping_mul(*n);
                        prod_f *= *n as f64;
                    }
                    _ => {
                        all_int = false;
                        prod_f *= v.as_f64().unwrap_or(0.0);
                    }
                }
            }
            Ok(if all_int {
                JVal::Int(prod_i)
            } else {
                JVal::Float(prod_f)
            })
        }

        Expr::Div(a, b) => {
            let av = eval_expr(a, locals, state, operators)?;
            let bv = eval_expr(b, locals, state, operators)?;
            let denom = bv.as_f64().unwrap_or(0.0);
            if denom == 0.0 {
                return Err(EvalError::new("division by zero"));
            }
            let numer = av.as_f64().unwrap_or(0.0);
            // Return Int if both operands are Int and result is exact
            if let (JVal::Int(x), JVal::Int(y)) = (&av, &bv) {
                if *y != 0 && x % y == 0 {
                    return Ok(JVal::Int(x / y));
                }
            }
            Ok(JVal::Float(numer / denom))
        }

        Expr::Mod(a, b) => {
            let av = eval_expr(a, locals, state, operators)?;
            let bv = eval_expr(b, locals, state, operators)?;
            match (&av, &bv) {
                (JVal::Int(x), JVal::Int(y)) if *y != 0 => Ok(JVal::Int(x % y)),
                _ => {
                    let denom = bv.as_i64().unwrap_or(0);
                    if denom == 0 {
                        return Err(EvalError::new("mod by zero"));
                    }
                    Ok(JVal::Int(av.as_i64().unwrap_or(0) % denom))
                }
            }
        }

        // === Comparison ===
        Expr::Eq(a, b) => {
            let av = eval_expr(a, locals, state, operators)?;
            let bv = eval_expr(b, locals, state, operators)?;
            Ok(JVal::Bool(av == bv))
        }
        Expr::Neq(a, b) => {
            let av = eval_expr(a, locals, state, operators)?;
            let bv = eval_expr(b, locals, state, operators)?;
            Ok(JVal::Bool(av != bv))
        }
        Expr::Gt(a, b) => {
            let av = eval_expr(a, locals, state, operators)?;
            let bv = eval_expr(b, locals, state, operators)?;
            Ok(JVal::Bool(
                av.cmp_numeric_or_string(&bv).is_some_and(|o| o.is_gt()),
            ))
        }
        Expr::Lt(a, b) => {
            let av = eval_expr(a, locals, state, operators)?;
            let bv = eval_expr(b, locals, state, operators)?;
            Ok(JVal::Bool(
                av.cmp_numeric_or_string(&bv).is_some_and(|o| o.is_lt()),
            ))
        }
        Expr::Gte(a, b) => {
            let av = eval_expr(a, locals, state, operators)?;
            let bv = eval_expr(b, locals, state, operators)?;
            Ok(JVal::Bool(
                av.cmp_numeric_or_string(&bv).is_some_and(|o| !o.is_lt()),
            ))
        }
        Expr::Lte(a, b) => {
            let av = eval_expr(a, locals, state, operators)?;
            let bv = eval_expr(b, locals, state, operators)?;
            Ok(JVal::Bool(
                av.cmp_numeric_or_string(&bv).is_some_and(|o| !o.is_gt()),
            ))
        }

        // === Logic (short-circuit) ===
        Expr::And(args) => {
            let mut last = JVal::Bool(true);
            for arg in args {
                last = eval_expr(arg, locals, state, operators)?;
                if !last.is_truthy() {
                    return Ok(JVal::Bool(false));
                }
            }
            Ok(last)
        }
        Expr::Or(args) => {
            for arg in args {
                let v = eval_expr(arg, locals, state, operators)?;
                if v.is_truthy() {
                    return Ok(v);
                }
            }
            Ok(JVal::Bool(false))
        }
        Expr::Not(a) => {
            let v = eval_expr(a, locals, state, operators)?;
            Ok(JVal::Bool(!v.is_truthy()))
        }
        Expr::If(cond, then_expr, else_expr) => {
            let cv = eval_expr(cond, locals, state, operators)?;
            if cv.is_truthy() {
                eval_expr(then_expr, locals, state, operators)
            } else if let Some(e) = else_expr {
                eval_expr(e, locals, state, operators)
            } else {
                Ok(JVal::Null)
            }
        }

        // === String ===
        Expr::Concat(args) => {
            let mut out = String::new();
            for arg in args {
                let v = eval_expr(arg, locals, state, operators)?;
                jval_write_str(&v, &mut out);
            }
            Ok(JVal::Str(Rc::from(out.as_str())))
        }
        Expr::Trim(a) => {
            let v = eval_expr(a, locals, state, operators)?;
            Ok(JVal::Str(Rc::from(
                match &v {
                    JVal::Str(s) => s.trim().to_string(),
                    other => other.display_string().trim().to_string(),
                }
                .as_str(),
            )))
        }
        Expr::StrLen(a) => {
            let v = eval_expr(a, locals, state, operators)?;
            let n = match &v {
                JVal::Str(s) => s.chars().count(),
                other => other.display_string().chars().count(),
            };
            Ok(JVal::Int(n as i64))
        }
        Expr::Lower(a) => {
            let v = eval_expr(a, locals, state, operators)?;
            Ok(JVal::Str(Rc::from(
                match &v {
                    JVal::Str(s) => s.to_lowercase(),
                    other => other.display_string().to_lowercase(),
                }
                .as_str(),
            )))
        }
        Expr::Upper(a) => {
            let v = eval_expr(a, locals, state, operators)?;
            Ok(JVal::Str(Rc::from(
                match &v {
                    JVal::Str(s) => s.to_uppercase(),
                    other => other.display_string().to_uppercase(),
                }
                .as_str(),
            )))
        }
        Expr::Contains(hay, needle) => {
            let h = eval_expr(hay, locals, state, operators)?;
            let n = eval_expr(needle, locals, state, operators)?;
            let result = match &h {
                JVal::Str(s) => match &n {
                    JVal::Str(ns) => s.contains(ns.as_ref()),
                    _ => s.contains(&n.display_string().as_str()),
                },
                JVal::Array(arr) => arr.contains(&n),
                _ => false,
            };
            Ok(JVal::Bool(result))
        }
        Expr::Template(fmt_expr, arg_exprs) => {
            let fmt = eval_expr(fmt_expr, locals, state, operators)?;
            let mut out = fmt.display_string();
            for arg_expr in arg_exprs {
                let v = eval_expr(arg_expr, locals, state, operators)?;
                let rep = v.display_string();
                if let Some(pos) = out.find("{}") {
                    out.replace_range(pos..pos + 2, &rep);
                }
            }
            Ok(JVal::Str(Rc::from(out.as_str())))
        }

        // === Type conversion ===
        Expr::ToInt(a) => {
            let v = eval_expr(a, locals, state, operators)?;
            Ok(JVal::Int(v.as_i64().unwrap_or(0)))
        }
        Expr::ToFloat(a) => {
            let v = eval_expr(a, locals, state, operators)?;
            Ok(JVal::Float(v.as_f64().unwrap_or(0.0)))
        }
        Expr::ToString(a) => {
            let v = eval_expr(a, locals, state, operators)?;
            Ok(JVal::Str(Rc::from(
                match v {
                    JVal::Str(s) => return Ok(JVal::Str(s)),
                    JVal::Null => String::new(),
                    JVal::Int(n) => n.to_string(),
                    JVal::Float(f) => format_f64(f),
                    JVal::Bool(b) => b.to_string(),
                    other => other.display_string(),
                }
                .as_str(),
            )))
        }

        // === Collections ===
        Expr::Len(a) => {
            let v = eval_expr(a, locals, state, operators)?;
            let n = match &v {
                JVal::Str(s) => s.chars().count(),
                JVal::Array(a) => a.len(),
                JVal::Object(o) => o.len(),
                _ => 0,
            };
            Ok(JVal::Int(n as i64))
        }
        Expr::Push(list_expr, item_expr) => {
            let list = eval_expr(list_expr, locals, state, operators)?;
            let item = eval_expr(item_expr, locals, state, operators)?;
            let mut arr = match list {
                JVal::Array(a) => Rc::try_unwrap(a).unwrap_or_else(|rc| (*rc).clone()),
                _ => Vec::new(),
            };
            arr.push(item);
            Ok(JVal::Array(Rc::new(arr)))
        }
        Expr::Get(collection_expr, key_expr) => {
            let collection = eval_expr(collection_expr, locals, state, operators)?;
            let key = eval_expr(key_expr, locals, state, operators)?;
            let out = match (&collection, &key) {
                (JVal::Object(obj), JVal::Str(k)) => obj.get(k.as_ref()).cloned(),
                (JVal::Array(arr), JVal::Int(n)) if *n >= 0 => arr.get(*n as usize).cloned(),
                (JVal::Array(arr), JVal::Str(s)) => {
                    s.parse::<usize>().ok().and_then(|i| arr.get(i).cloned())
                }
                _ => None,
            };
            Ok(out.unwrap_or(JVal::Null))
        }

        // === Control flow ===
        Expr::Do(args) => {
            let mut last = JVal::Null;
            for arg in args {
                last = eval_expr(arg, locals, state, operators)?;
            }
            Ok(last)
        }
        Expr::Match(value_expr, cases) => {
            let value = eval_expr(value_expr, locals, state, operators)?;
            let wildcard = JVal::Str(Rc::from("_"));
            for (pat_expr, result_expr) in cases {
                let pat = eval_expr(pat_expr, locals, state, operators)?;
                if pat == wildcard || pat == value {
                    return eval_expr(result_expr, locals, state, operators);
                }
            }
            Ok(JVal::Null)
        }

        // === State mutation ===
        Expr::Set(path, value_expr) => {
            let value = eval_expr(value_expr, locals, state, operators)?;
            write_state_jval(state, path, value.clone());
            Ok(value)
        }

        // === Tree-level iteration ===
        Expr::Map(list_expr, body_expr) => {
            let list = eval_expr(list_expr, locals, state, operators)?;
            match list {
                JVal::Array(arr) => {
                    let mut out = Vec::with_capacity(arr.len());
                    for (i, item) in arr.iter().enumerate() {
                        let inner = IterLocals::new(item.clone(), i as i64);
                        out.push(eval_expr(body_expr, &inner, state, operators)?);
                    }
                    Ok(JVal::Array(Rc::new(out)))
                }
                _ => Err(EvalError::new("map first arg must be array")),
            }
        }
        Expr::Filter(list_expr, body_expr) => {
            let list = eval_expr(list_expr, locals, state, operators)?;
            match list {
                JVal::Array(arr) => {
                    let mut out = Vec::new();
                    for (i, item) in arr.iter().enumerate() {
                        let inner = IterLocals::new(item.clone(), i as i64);
                        let keep = eval_expr(body_expr, &inner, state, operators)?;
                        if keep.is_truthy() {
                            out.push(item.clone());
                        }
                    }
                    Ok(JVal::Array(Rc::new(out)))
                }
                _ => Err(EvalError::new("filter first arg must be array")),
            }
        }

        // === Events (fast path does not carry EventContext; return Null) ===
        Expr::EventValue | Expr::EventKey | Expr::EventPrevent => Ok(JVal::Null),

        // === Side effects ===
        Expr::Log(args) => {
            let mut vals = Vec::with_capacity(args.len());
            for arg in args {
                vals.push(eval_expr(arg, locals, state, operators)?);
            }
            let json_vals: Vec<Value> = vals.iter().map(Value::from).collect();
            eprintln!("[josie.log] {}", serde_json::Value::Array(json_vals));
            Ok(JVal::Array(Rc::new(vals)))
        }
        Expr::Effect(args) => {
            let mut last = JVal::Null;
            for arg in args {
                last = eval_expr(arg, locals, state, operators)?;
            }
            Ok(last)
        }

        // === Fallback: custom/unknown operator ===
        Expr::Call(op_name, arg_exprs) => {
            let mut json_args: Vec<Value> = Vec::with_capacity(arg_exprs.len());
            for arg_expr in arg_exprs {
                let jval = eval_expr(arg_expr, locals, state, operators)?;
                json_args.push(Value::from(jval));
            }
            if let Some(op) = operators.get(op_name) {
                let mut ctx = Context {
                    state,
                    operators,
                    event: None,
                };
                let result = op(&json_args, &mut ctx)?;
                Ok(JVal::from(result))
            } else {
                // Unknown op: try stripping "core." prefix
                let bare = op_name.strip_prefix("core.").unwrap_or(op_name);
                if let Some(op) = operators.get(bare) {
                    let mut ctx = Context {
                        state,
                        operators,
                        event: None,
                    };
                    let result = op(&json_args, &mut ctx)?;
                    return Ok(JVal::from(result));
                }
                Err(EvalError::new(format!("unknown operator '{op_name}'")))
            }
        }
    }
}

// ─── helpers ─────────────────────────────────────────────────────────────────

#[inline]
fn add_jvals(a: JVal, b: JVal) -> JVal {
    match (&a, &b) {
        (JVal::Int(x), JVal::Int(y)) => JVal::Int(x.wrapping_add(*y)),
        _ => JVal::Float(a.as_f64().unwrap_or(0.0) + b.as_f64().unwrap_or(0.0)),
    }
}

#[inline]
fn mul_jvals(a: JVal, b: JVal) -> JVal {
    match (&a, &b) {
        (JVal::Int(x), JVal::Int(y)) => JVal::Int(x.wrapping_mul(*y)),
        _ => JVal::Float(a.as_f64().unwrap_or(0.0) * b.as_f64().unwrap_or(0.0)),
    }
}

fn jval_write_str(v: &JVal, out: &mut String) {
    use std::fmt::Write;
    match v {
        JVal::Str(s) => out.push_str(s),
        JVal::Null => {}
        JVal::Int(n) => {
            let _ = write!(out, "{n}");
        }
        JVal::Float(f) => out.push_str(&format_f64(*f)),
        JVal::Bool(b) => {
            let _ = write!(out, "{b}");
        }
        other => out.push_str(&other.display_string()),
    }
}

fn read_state_jval(state: &State, path: &str) -> Option<JVal> {
    if let Some(rest) = path.strip_prefix("client.") {
        return map_get_jval(&state.client, rest);
    }
    if let Some(rest) = path.strip_prefix("server.") {
        return map_get_jval(&state.server, rest);
    }
    // Bare name: check client first, then server
    map_get_jval(&state.client, path).or_else(|| map_get_jval(&state.server, path))
}

fn map_get_jval(map: &serde_json::Map<String, Value>, path: &str) -> Option<JVal> {
    if path.is_empty() {
        return Some(JVal::from(Value::Object(map.clone())));
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
    Some(JVal::from(current.clone()))
}

fn write_state_jval(state: &mut State, path: &str, value: JVal) {
    let json_val = Value::from(value);
    let (scope, rest) = match path.split_once('.') {
        Some(pair) => pair,
        None => {
            state.client.insert(path.to_string(), json_val);
            return;
        }
    };
    let target = match scope {
        "client" => &mut state.client,
        "server" => &mut state.server,
        _ => {
            state.client.insert(path.to_string(), json_val);
            return;
        }
    };
    // Simple single-level key after scope
    if !rest.contains('.') {
        target.insert(rest.to_string(), json_val);
    } else {
        // Nested path: reuse existing set_in_map logic via Value mutation
        // For simplicity, insert via a temporary Value
        use serde_json::Map;
        fn set_nested(map: &mut Map<String, Value>, path: &str, value: Value) {
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
                current = entry.as_object_mut().unwrap();
            }
            current.insert(parts.last().unwrap().to_string(), value);
        }
        set_nested(target, rest, json_val);
    }
}
