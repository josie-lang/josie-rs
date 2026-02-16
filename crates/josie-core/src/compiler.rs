use crate::jval::JVal;
use serde_json::Value;
use std::rc::Rc;

/// Compiled expression — every operator is a direct enum variant.
/// No string matching at evaluation time.
#[derive(Debug, Clone)]
pub enum Expr {
    // === Literals ===
    Literal(JVal),

    // === Variable access (resolved at compile time) ===
    /// ["var", "item"] in iterator context
    LocalItem,
    /// ["var", "index"] in iterator context
    LocalIndex,
    /// ["var", "acc"] in reduce context
    LocalAcc,
    /// ["var", "some.path"] — state lookup
    Var(Rc<str>),
    /// ["var", <expr>] — dynamic path
    VarDynamic(Box<Expr>),

    // === Arithmetic ===
    Add(Vec<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Vec<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),

    // === Comparison ===
    Eq(Box<Expr>, Box<Expr>),
    Neq(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Gte(Box<Expr>, Box<Expr>),
    Lte(Box<Expr>, Box<Expr>),

    // === Logic ===
    And(Vec<Expr>),
    Or(Vec<Expr>),
    Not(Box<Expr>),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),

    // === String ===
    Concat(Vec<Expr>),
    Lower(Box<Expr>),
    Upper(Box<Expr>),
    Contains(Box<Expr>, Box<Expr>),
    Template(Box<Expr>, Vec<Expr>),
    Trim(Box<Expr>),
    StrLen(Box<Expr>),

    // === Type conversion ===
    ToInt(Box<Expr>),
    ToFloat(Box<Expr>),
    ToString(Box<Expr>),

    // === Collections ===
    Len(Box<Expr>),
    Push(Box<Expr>, Box<Expr>),
    Get(Box<Expr>, Box<Expr>),

    // === Control flow ===
    Do(Vec<Expr>),
    Match(Box<Expr>, Vec<(Expr, Expr)>),

    // === State ===
    Set(Rc<str>, Box<Expr>),

    // === Iteration (tree-level, not pipeline-level) ===
    Map(Box<Expr>, Box<Expr>),
    Filter(Box<Expr>, Box<Expr>),

    // === Side effects ===
    Log(Vec<Expr>),
    Effect(Vec<Expr>),

    // === Events ===
    EventValue,
    EventKey,
    EventPrevent,

    // === Fallback: unknown/custom operator ===
    Call(Rc<str>, Vec<Expr>),
}

/// Compile a serde_json::Value expression into a typed Expr.
///
/// `iter_ctx`: true when inside map/filter/for_each/reduce body —
///   enables LocalItem/LocalIndex resolution.
/// `reduce_ctx`: true when inside a reduce body —
///   enables LocalAcc resolution.
pub fn compile_expr(value: &Value, iter_ctx: bool, reduce_ctx: bool) -> Expr {
    match value {
        Value::Null => Expr::Literal(JVal::Null),
        Value::Bool(b) => Expr::Literal(JVal::Bool(*b)),
        Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Expr::Literal(JVal::Int(i))
            } else if let Some(f) = n.as_f64() {
                Expr::Literal(JVal::Float(f))
            } else {
                Expr::Literal(JVal::Null)
            }
        }
        Value::String(s) => Expr::Literal(JVal::Str(Rc::from(s.as_str()))),
        Value::Array(arr) if arr.is_empty() => {
            Expr::Literal(JVal::Array(Rc::new(vec![])))
        }
        Value::Array(arr) => {
            if let Some(op_name) = arr[0].as_str() {
                compile_op(op_name, &arr[1..], iter_ctx, reduce_ctx)
            } else {
                // Not an operator — literal array
                let elements: Vec<JVal> = arr.iter().map(|v| jval_from_literal(v)).collect();
                Expr::Literal(JVal::Array(Rc::new(elements)))
            }
        }
        Value::Object(_) => Expr::Literal(JVal::from(value.clone())),
    }
}

fn jval_from_literal(v: &Value) -> JVal {
    JVal::from(v.clone())
}

fn compile_op(op: &str, args: &[Value], iter_ctx: bool, reduce_ctx: bool) -> Expr {
    let c = |idx: usize| -> Expr {
        args.get(idx)
            .map(|v| compile_expr(v, iter_ctx, reduce_ctx))
            .unwrap_or(Expr::Literal(JVal::Null))
    };
    let cbox = |idx: usize| -> Box<Expr> { Box::new(c(idx)) };
    let call_args = || -> Vec<Expr> {
        args.iter()
            .map(|v| compile_expr(v, iter_ctx, reduce_ctx))
            .collect()
    };

    match op {
        // --- variable ---
        "var" => compile_var(args, iter_ctx, reduce_ctx),

        // --- set ---
        "set" => {
            if let Some(path) = args.first().and_then(|v| v.as_str()) {
                Expr::Set(Rc::from(path), cbox(1))
            } else {
                Expr::Call(Rc::from("set"), call_args())
            }
        }

        // --- arithmetic ---
        "+" => try_fold_add(call_args()),
        "-" => Expr::Sub(cbox(0), cbox(1)),
        "*" => try_fold_mul(call_args()),
        "/" => Expr::Div(cbox(0), cbox(1)),
        "%" => try_fold_mod(cbox(0), cbox(1)),

        // --- comparison ---
        "==" => Expr::Eq(cbox(0), cbox(1)),
        "!=" => Expr::Neq(cbox(0), cbox(1)),
        ">" => Expr::Gt(cbox(0), cbox(1)),
        "<" => Expr::Lt(cbox(0), cbox(1)),
        ">=" => Expr::Gte(cbox(0), cbox(1)),
        "<=" => Expr::Lte(cbox(0), cbox(1)),

        // --- logic ---
        "&&" => Expr::And(call_args()),
        "||" => Expr::Or(call_args()),
        "!" => Expr::Not(cbox(0)),
        "if" => Expr::If(
            cbox(0),
            cbox(1),
            args.get(2)
                .map(|v| Box::new(compile_expr(v, iter_ctx, reduce_ctx))),
        ),

        // --- string ---
        "u.concat" => Expr::Concat(call_args()),
        "u.lower" => Expr::Lower(cbox(0)),
        "u.upper" => Expr::Upper(cbox(0)),
        "u.contains" => Expr::Contains(cbox(0), cbox(1)),
        "u.template" => Expr::Template(cbox(0), args[1..].iter().map(|v| compile_expr(v, iter_ctx, reduce_ctx)).collect()),
        "u.trim" => Expr::Trim(cbox(0)),
        "u.str_len" => Expr::StrLen(cbox(0)),

        // --- type conversion ---
        "u.to_int" => Expr::ToInt(cbox(0)),
        "u.to_float" => Expr::ToFloat(cbox(0)),
        "u.to_string" => Expr::ToString(cbox(0)),

        // --- collections ---
        "len" => Expr::Len(cbox(0)),
        "push" => Expr::Push(cbox(0), cbox(1)),
        "get" => Expr::Get(cbox(0), cbox(1)),

        // --- control flow ---
        "do" | "pipe" => Expr::Do(call_args()),
        "match" => {
            let value_expr = cbox(0);
            let mut cases = Vec::new();
            let mut i = 1;
            while i + 1 < args.len() {
                let pat = compile_expr(&args[i], iter_ctx, reduce_ctx);
                let result = compile_expr(&args[i + 1], iter_ctx, reduce_ctx);
                cases.push((pat, result));
                i += 2;
            }
            Expr::Match(value_expr, cases)
        }

        // --- iteration (tree-level) ---
        "map" => Expr::Map(cbox(0), Box::new(compile_expr(args.get(1).unwrap_or(&Value::Null), true, false))),
        "filter" => Expr::Filter(cbox(0), Box::new(compile_expr(args.get(1).unwrap_or(&Value::Null), true, false))),

        // --- events ---
        "w.event.value" => Expr::EventValue,
        "w.event.key" => Expr::EventKey,
        "w.event.prevent" => Expr::EventPrevent,

        // --- side effects ---
        "log" => Expr::Log(call_args()),
        "effect" => Expr::Effect(call_args()),

        // --- fallback ---
        unknown => Expr::Call(Rc::from(unknown), call_args()),
    }
}

fn compile_var(args: &[Value], iter_ctx: bool, reduce_ctx: bool) -> Expr {
    let Some(first) = args.first() else {
        return Expr::Literal(JVal::Null);
    };

    if let Some(name) = first.as_str() {
        if iter_ctx && name == "item" {
            return Expr::LocalItem;
        }
        if iter_ctx && name == "index" {
            return Expr::LocalIndex;
        }
        if reduce_ctx && name == "acc" {
            return Expr::LocalAcc;
        }
        return Expr::Var(Rc::from(name));
    }

    Expr::VarDynamic(Box::new(compile_expr(first, iter_ctx, reduce_ctx)))
}

fn try_fold_add(args: Vec<Expr>) -> Expr {
    if args.iter().all(|e| matches!(e, Expr::Literal(JVal::Int(_)))) {
        let sum: i64 = args
            .iter()
            .map(|e| match e {
                Expr::Literal(JVal::Int(n)) => *n,
                _ => 0,
            })
            .sum();
        return Expr::Literal(JVal::Int(sum));
    }
    if args
        .iter()
        .all(|e| matches!(e, Expr::Literal(JVal::Int(_) | JVal::Float(_))))
    {
        let sum: f64 = args
            .iter()
            .map(|e| match e {
                Expr::Literal(JVal::Int(n)) => *n as f64,
                Expr::Literal(JVal::Float(f)) => *f,
                _ => 0.0,
            })
            .sum();
        return Expr::Literal(JVal::Float(sum));
    }
    Expr::Add(args)
}

fn try_fold_mul(args: Vec<Expr>) -> Expr {
    if args.iter().all(|e| matches!(e, Expr::Literal(JVal::Int(_)))) {
        let prod: i64 = args
            .iter()
            .map(|e| match e {
                Expr::Literal(JVal::Int(n)) => *n,
                _ => 1,
            })
            .product();
        return Expr::Literal(JVal::Int(prod));
    }
    Expr::Mul(args)
}

fn try_fold_mod(a: Box<Expr>, b: Box<Expr>) -> Expr {
    if let (Expr::Literal(JVal::Int(av)), Expr::Literal(JVal::Int(bv))) =
        (a.as_ref(), b.as_ref())
    {
        if *bv != 0 {
            return Expr::Literal(JVal::Int(av.rem_euclid(*bv)));
        }
    }
    Expr::Mod(a, b)
}
