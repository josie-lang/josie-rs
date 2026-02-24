# josie-core

Core runtime for **JOSIE**: **Josie Omni Secure Interactive Expressions**.

`josie-core` is the execution engine used by `josie-web`, `josie-cli`, and host apps that embed JOSIE.

## What this crate gives you

- Deterministic expression evaluation
- Program/pipeline parsing and execution
- Lisp-style JOSIE input (recommended authoring format)
- JSON tree/program input (canonical machine format)
- Built-in kernel operators + `util.*` + `data.*`
- Host extension points via custom operators

## Recommended authoring style

Use **JOSIE Lisp** (`.josie`) for writing by hand.
Use JSON trees/program docs for generated or interoperable payloads.

---

## Quickstart (Lisp-first)

```rust
use josie_core::program::{execute_program, parse_program};
use josie_core::{read_program, Operators};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let src = r#"
    (set client.name "JOSIE")
    (set client.msg (util.concat "Hello, " client.name))
    client.msg
    "#;

    let doc = read_program(src)?;            // Lisp -> Program JSON doc
    let program = parse_program(&doc)?;      // Validate program contract
    let out = execute_program(&program, &Operators::new())?;

    println!("{}", out.value);               // "Hello, JOSIE"
    Ok(())
}
```

---

## JSON program format (canonical)

```rust
use josie_core::program::{execute_program, parse_program};
use josie_core::Operators;
use serde_json::json;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let doc = json!({
        "state": { "client": {}, "server": {} },
        "program": ["do",
            ["set", "client.a", 10],
            ["set", "client.b", 20],
            ["+", ["var", "client.a"], ["var", "client.b"]]
        ]
    });

    let program = parse_program(&doc)?;
    let out = execute_program(&program, &Operators::new())?;
    assert_eq!(out.value, json!(30.0));
    Ok(())
}
```

---

## Example: function + recursion (Lisp)

```lisp
(def fib (n)
  (if (<= n 1)
    n
    (+ (call fib (- n 1))
       (call fib (- n 2)))))

(call fib 10)
```

You can run this through `read_program -> parse_program -> execute_program`.

---

## Example: data wrangling (Lisp)

```lisp
(set server.rows [
  {region: "APAC" amount: 120}
  {region: "EU"   amount:  80}
  {region: "APAC" amount:  30}
])

(set server.grouped
  (data.group_by server.rows ["region"]))

(set server.summary
  (data.aggregate server.grouped {
    n: "count(*)"
    total: "sum(amount)"
  }))

(data.sort_by server.summary [{col: "total" dir: "desc"}])
```

---

## Example: backend response shaping (Lisp)

```lisp
(set server.response {
  name: "people.index"
  properties: {
    totalCount: (len server.people)
    people: (map server.people {
      id: item.id
      displayName: item.name
      tags: item.tags
    })
  }
})
```

---

## Host extension (`x.*`) example

```rust
use josie_core::{Context, EvalResult, Operator, Operators, State, evaluate};
use serde_json::{json, Value};

fn op_x_echo(args: &[Value], _ctx: &mut Context) -> EvalResult {
    Ok(args.first().cloned().unwrap_or(Value::Null))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut ops = Operators::new();
    ops.register("x.echo", op_x_echo as Operator);

    let expr = json!(["x.echo", {"ok": true}]);
    let mut state = State::new();
    let mut ctx = Context { state: &mut state, operators: &ops, event: None };
    let out = evaluate(&expr, &mut ctx)?;

    assert_eq!(out, json!({"ok": true}));
    Ok(())
}
```

---

## Program interfaces you will use most

- `read_program(&str)` for Lisp input
- `parse_program(&Value)` for validated program doc
- `execute_program(&Program, &Operators)` one-shot execute
- `compile_program(&Program)` + `execute_compiled_program(...)` for reuse
- `execute_program_with_hosts(...)` when host callbacks are involved

---

## Operator families

- **kernel**: `var`, `set`, `if`, `do`, `pipe`, `def`, `call`, arithmetic/comparison/boolean ops
- **collection**: `map`, `filter`, `len`, `push`
- **util.***: strings, normalization, paths, regex, casting helpers
- **data.***: select/rename/cast/chunk/flat_map/distinct/sort/group/aggregate/join

---

## Safety model

- No implicit filesystem/network access in core runtime
- Side effects only through explicit operators/host integrations
- Host app controls what custom operators are exposed

---

## Publish (crates.io)

Run from this crate folder:

```bash
cd crates/josie-core
cargo publish
```

Or from workspace root:

```bash
cargo publish -p josie-core
```

---

## Links

- API docs: <https://docs.rs/josie-core>
- Project site: <https://josie-lang.org>
