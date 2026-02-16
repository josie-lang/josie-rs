# josie-core

Core runtime for **JOSIE** (JSON Omni Safe Interactive Expressions).

`josie-core` provides:
- Tree expression evaluation (`["op", ...args]`)
- Pipeline document parsing/validation/compilation
- Fast execution tiers with semantic fallback guarantees
- Host extension points (custom operators and typed host callbacks)

## Operator Surface (Core)

- State: `var`, `set`, `get`
- Sequence: `do`, `pipe`
- Math: `+`, `-`, `*`, `/`, `%`
- Compare: `==`, `!=`, `>`, `<`, `>=`, `<=`
- Boolean: `&&`, `||`, `!`
- Flow: `if`, `match`
- Collections: `map`, `filter`, `len`, `push`
- Functions: `def`, `call`
- Utils: `u.concat`, `u.lower`, `u.upper`, `u.contains`, `u.template`, `u.to_int`, `u.to_float`, `u.to_string`, `u.trim`, `u.str_len`
- Event helpers: `w.event.value`, `w.event.key`, `w.event.prevent`
- Extension namespace: `x.*` (host-defined)

## Hello World

```rust
use josie_core::{evaluate, Context, Operators, State};
use serde_json::json;

let mut state = State::new();
let operators = Operators::new();
let expr = json!(["u.concat", "Hello", ", ", "World"]);

let mut ctx = Context {
    state: &mut state,
    operators: &operators,
    event: None,
};

let out = evaluate(&expr, &mut ctx)?;
assert_eq!(out, json!("Hello, World"));
# Ok::<(), josie_core::EvalError>(())
```

## Inject Custom `x.*` Operator

```rust
use josie_core::{Context, EvalResult, Operator, Operators, State, evaluate};
use serde_json::{json, Value};

fn op_x_echo(args: &[Value], _ctx: &mut Context) -> EvalResult {
    Ok(args.first().cloned().unwrap_or(Value::Null))
}

let mut operators = Operators::new();
operators.register("x.echo", op_x_echo as Operator);

let mut state = State::new();
let expr = json!(["x.echo", {"ok": true}]);
let mut ctx = Context { state: &mut state, operators: &operators, event: None };
let out = evaluate(&expr, &mut ctx)?;
assert_eq!(out, json!({"ok": true}));
# Ok::<(), josie_core::EvalError>(())
```

## Pipeline Execution (Compile Once, Execute Many)

```rust
use josie_core::program::{compile_program, execute_compiled_program, parse_program};
use josie_core::{Operators, State};
use serde_json::json;

let doc = json!({
  "state": {"client": {}, "server": {}},
  "program": {
    "type": "pipeline",
    "steps": [
      {"op":"set", "into":"nums", "args":[[1,2,3,4]]},
      {"op":"map", "from":"nums", "into":"mapped", "do":["+", ["var", "item"], 1]},
      {"op":"reduce", "from":"mapped", "into":"score", "args":[0], "do":["+", ["var", "acc"], ["var", "item"]]}
    ]
  }
});

let parsed = parse_program(&doc)?;
let compiled = compile_program(&parsed)?;
let mut state = State::new();
let operators = Operators::new();
let out = execute_compiled_program(&compiled, &mut state, &operators)?;
assert_eq!(out, json!(14));
# Ok::<(), Box<dyn std::error::Error>>(())
```

## Crate Docs

- API docs: <https://docs.rs/josie-core>
- Project site: <https://josie-lang.org>
