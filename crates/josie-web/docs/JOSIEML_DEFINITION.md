# JOSIEML Definition

## Core Identity

JOSIEML is full HTML authoring format for interactive web pages powered by Josie.

- `.josieml` contains full page/component markup.
- `<script type="application/josie+json">` inside `.josieml` carries the executable Program payload.
- Josie language stays single: executable atoms are Josie tree expressions.

## One Language Rule

Executable logic must be the same Josie semantics as `josie-core`:

- core ops: `set`, `get`, `call`, `map`, `filter`, `for_each`, `reduce`, `if`, `match`, `do`, `return`
- stdlib: `util.*`, `data.*`
- extension/injected: `x.*`

No second mini-language for web logic.

## Program Payload in JOSIEML

The payload inside `application/josie+json` is canonical Program-shaped data:

```json
{
  "kind": "program",
  "version": "0.1.0",
  "state": {},
  "actions": {},
  "resources": [],
  "steps": []
}
```

## Interaction Contract

- template event binding uses action name (`@click="counter.inc"`)
- action name resolves to step execution entry (`runStep` mapping)
- `steps` are executable step definitions (not separate language)
- runtime uses delegated event listeners per event type (`@click`, `@input`, ...)
- list rendering supports keyed reconciliation via `data-josie-map` config (`source`, optional `key`)

## Runtime Contract

- default mode is `compiled`
- compiled mode exports and loads:
`josie-runtime.js` + `program.compiled.js`
- interpreted mode exports and loads:
`josie-runtime-dev.js`
- both modes keep the same `.josieml` and Program payload shape

## Safety Contract

- no inline JavaScript execution in template
- external JS/CSS/URL must pass allowlist policy
- all mutation must be explicit in Program operations

## Responsibility Boundary

- `josie-core`: language semantics and execution engine
- `josie-web`: template compile/render, component stitching, hydration/runtime binding
- framework adapter (axum/actix/etc.): transport and route ownership
