# josie-core

Core runtime for **JOSIE** (JSON Omni Safe Interactive Expressions).

New agreement:
- Pipeline as single entrance, each operation is tree
- Std library = data.* and util.*
- TS contract snapshot: [`contracts/josie-core/Program.ts`](../../contracts/josie-core/Program.ts)

`josie-core` provides:
- Tree expression evaluation (`["op", ...args]`)
- Pipeline document parsing/validation/compilation
- Fast execution tiers with semantic fallback guarantees
- Host extension points (custom operators and typed host callbacks)

## Operator Surface (Core)

### Language Tiers

- `kernel` (no prefix, always-on): control flow, state, math, base collection ops
- `util.*` stdlib (native included): scalar/path/string/normalization helpers
- `data.*` stdlib (native included): row/list/tabular transforms for ETL-style workloads
- `x.*` extension namespace: host/injected operators only

### Core Keyword Status Table

| tier | keyword | status | notes |
|---|---|---|---|
| kernel | `var`, `set`, `get` | implemented | state access/update |
| kernel | `if`, `match`, `do`, `pipe` | implemented | control flow/sequence |
| kernel | `def`, `call` | implemented | function definition/call |
| kernel | `+`, `-`, `*`, `/`, `%` | implemented | math ops |
| kernel | `==`, `!=`, `>`, `<`, `>=`, `<=` | implemented | compare ops |
| kernel | `&&`, `||`, `!` | implemented | boolean ops |
| kernel | `map`, `filter`, `len`, `push` | implemented | base collection ops |
| pipeline step | `call`, `set`, `get`, `map`, `filter`, `for_each`, `reduce`, `if`, `match`, `do`, `pipe`, `return` | implemented | validated in pipeline compiler/runtime |
| pipeline policy | `run_hint`, `on_error`, `timeout_ms`, `max_retries`, `idempotency_key` | implemented | execution control hints/policies (planner + runtime) |
| stdlib `util.*` | `util.concat`, `util.lower`, `util.upper`, `util.contains`, `util.template`, `util.to_int`, `util.to_float`, `util.to_string`, `util.trim`, `util.str_len` | implemented | included native stdlib |
| event helper | `w.event.value`, `w.event.key`, `w.event.prevent` | implemented | web/event integration helpers |
| extension | `x.*` | implemented | host/injected operators |
| stdlib `util.*` | `util.get_path`, `util.one_of`, `util.pick`, `util.exists`, `util.omit` | implemented | path extraction/projection |
| stdlib `util.*` | `util.as_array`, `util.to_bool` | implemented | normalization helpers |
| stdlib `util.*` | `util.replace`, `util.replace_all`, `util.split`, `util.join` | implemented | text helpers |
| stdlib `util.*` | `util.regex_match`, `util.regex_find`, `util.regex_find_all`, `util.regex_replace` | implemented | regex helpers |
| stdlib `data.*` | `data.select`, `data.rename`, `data.cast`, `data.chunk`, `data.flat_map`, `data.distinct`, `data.sort_by`, `data.group_by`, `data.aggregate`, `data.join` | implemented | current ETL primitives |

### Priority Now (Phase 1)

Current focus after this landing:
- optional optimization path for heavy `data.*` workloads (parallel backend selection)

## Hello World

```rust
use josie_core::eval;
use serde_json::json;

let out = eval(&json!(["util.concat", "Hello", ", ", "World"]))?;
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

### Pipeline Step Policy Fields

`PipelineStep` supports optional policy fields:

- `run_hint`: `inline | worker` (planner hint, not business logic)
- `on_error`: `retry | fallback | halt | compensate`
- `timeout_ms`: step-level timeout budget
- `max_retries`: retry limit used with `on_error=retry`
- `idempotency_key`: dedupe key in runtime execution scope

## Main Interfaces (Full List)

### Expression Interfaces

```rust
use josie_core::{eval, eval_with_operators, eval_with_state, Operators, State};
use serde_json::json;

// 1) Fast single expression with default empty state + default operators.
let v1 = eval(&json!(["+", 1, 2]))?;
assert_eq!(v1, json!(3.0));

// 2) Single expression with caller-managed state.
let mut state = State::new();
state.client.insert("n".into(), json!(7));
let v2 = eval_with_state(&json!(["+", ["var", "n"], 1]), &mut state)?;
assert_eq!(v2, json!(8.0));

// 3) Single expression with custom operator registry.
let mut operators = Operators::new();
let v3 = eval_with_operators(&json!(["util.upper", "josie"]), &operators)?;
assert_eq!(v3, json!("JOSIE"));
# Ok::<(), josie_core::EvalError>(())
```

### Program/Pipeline Interfaces

```rust
use josie_core::program::{compile_program, execute_compiled_program, parse_program};
use josie_core::{Operators, State};
use serde_json::json;

let doc = json!({
  "state": {"client": {}, "server": {}},
  "program": {
    "type": "pipeline",
    "steps": [
      {"op":"set", "into":"nums", "args":[[1,2,3]]},
      {"op":"map", "from":"nums", "into":"mapped", "do":["+", ["var", "item"], 1]},
      {"op":"reduce", "from":"mapped", "into":"sum", "args":[0], "do":["+", ["var", "acc"], ["var", "item"]]}
    ]
  }
});

let parsed = parse_program(&doc)?;
let compiled = compile_program(&parsed)?;
let mut state = State::new();
let operators = Operators::new();
let out = execute_compiled_program(&compiled, &mut state, &operators)?;
assert_eq!(out, json!(9));
# Ok::<(), Box<dyn std::error::Error>>(())
```

### One-Shot Program Execute (Compile Included)

```rust
use josie_core::program::{execute_program, parse_program};
use josie_core::Operators;
use serde_json::json;

let doc = json!({
  "state": {"client": {}, "server": {}},
  "program": ["util.concat", "Hi", ", ", "Josie"]
});
let parsed = parse_program(&doc)?;
let out = execute_program(&parsed, &Operators::new())?;
assert_eq!(out.value, json!("Hi, Josie"));
# Ok::<(), Box<dyn std::error::Error>>(())
```

### Execute With Host Functions

```rust
use josie_core::program::{HostFunctions, execute_program_with_hosts, parse_program};
use josie_core::{Operators};
use serde_json::{json, Value};

fn host_sum(args: &[Value]) -> Result<Value, josie_core::program::RuntimeError> {
    let a = args.first().and_then(Value::as_i64).unwrap_or(0);
    let b = args.get(1).and_then(Value::as_i64).unwrap_or(0);
    Ok(json!(a + b))
}

let doc = json!({
  "state": {"client": {}, "server": {}},
  "program": {
    "type": "pipeline",
    "steps": [
      {"op":"call", "fn":"x.sum", "args":[4,5], "into":"total"},
      {"op":"get", "from":"total"}
    ]
  }
});

let parsed = parse_program(&doc)?;
let mut hosts = HostFunctions::new();
hosts.register_call("x.sum", host_sum);
let out = execute_program_with_hosts(&parsed, &Operators::new(), &hosts)?;
assert_eq!(out.value, json!(9));
# Ok::<(), Box<dyn std::error::Error>>(())
```

### Reuse Compiled Program + Hosts

```rust
use josie_core::program::{
    HostFunctions, compile_program, execute_compiled_program_with_hosts, parse_program
};
use josie_core::{Operators, State};
use serde_json::{json, Value};

fn host_inc(args: &[Value]) -> Result<Value, josie_core::program::RuntimeError> {
    Ok(json!(args.first().and_then(Value::as_i64).unwrap_or(0) + 1))
}

let doc = json!({
  "state": {"client": {}, "server": {}},
  "program": {
    "type": "pipeline",
    "steps": [
      {"op":"call", "fn":"x.inc", "args":[41], "into":"v"},
      {"op":"get", "from":"v"}
    ]
  }
});

let parsed = parse_program(&doc)?;
let compiled = compile_program(&parsed)?;
let mut hosts = HostFunctions::new();
hosts.register_call("x.inc", host_inc);
let operators = Operators::new();

let mut run_state_1 = State::new();
let v1 = execute_compiled_program_with_hosts(&compiled, &mut run_state_1, &operators, &hosts)?;
assert_eq!(v1, json!(42));

let mut run_state_2 = State::new();
let v2 = execute_compiled_program_with_hosts(&compiled, &mut run_state_2, &operators, &hosts)?;
assert_eq!(v2, json!(42));
# Ok::<(), Box<dyn std::error::Error>>(())
```

### External Metrics Helper (Typed Fast Path)

```rust
use josie_core::program::{
    HostFunctions, compile_program, execute_compiled_program_external_metrics, parse_program
};
use josie_core::{Operators, State};
use serde_json::json;

fn host_gen(p1: i64, p2: i64, size: usize) -> Vec<i64> {
    (0..size).map(|i| ((i as i64 + p1) * p2) % 97).collect()
}
fn host_map(item: i64) -> i64 { (item * 3 + 1) % 101 }

let doc = json!({
  "state": {"client": {}, "server": {}},
  "program": {
    "type": "pipeline",
    "steps": [
      {"op":"call", "fn":"x.a", "args":[2,3,10], "into":"result_a"},
      {"op":"for_each", "from":"result_a", "do":{"op":"call", "fn":"x.b", "args":[["var","item"]]}, "into":"result_b"}
    ]
  }
});

let parsed = parse_program(&doc)?;
let compiled = compile_program(&parsed)?;
let mut hosts = HostFunctions::new();
hosts.register_generate_i64("x.a", host_gen);
hosts.register_map_i64("x.b", host_map);

let mut state = State::new();
let (checksum, len) = execute_compiled_program_external_metrics(
    &compiled,
    &mut state,
    &Operators::new(),
    &hosts,
)?;
assert_eq!(len, 10);
let _ = checksum;
# Ok::<(), Box<dyn std::error::Error>>(())
```

## Crate Docs

- API docs: <https://docs.rs/josie-core>
- Project site: <https://josie-lang.org>
