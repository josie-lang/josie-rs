# JOSIE Runtime Notes

## Why Hydration Now Works

`map` rows are hydrated with their own lexical scope (`item`) immediately after row render.
This prevents global-scope hydration from clobbering per-row bindings.

Key points:
- Boot uses `discover(document.body)` with global scope.
- `bindMap()` rerenders rows and calls `hydrateScopeTree(row, itemScope)`.
- `hydrateScopeTree()` avoids crossing into nested foreign map roots.

## Why This Is Stable

- SSR HTML stays authoritative.
- Runtime attaches bindings/events instead of re-parsing whole app state into a VDOM.
- Scoped hydration is close to Alpine-style behavior but with JOSIE expressions.

## Canonical Operator Names

- Core math/compare/boolean in web runtime follow canonical symbols:
  - `+ - * / %`
  - `== != > < >= <=`
  - `&& || !`
- Utility string/transform helpers use `u.*`:
  - `u.concat`, `u.lower`, `u.upper`, `u.contains`, `u.template`
  - `u.to_int`, `u.to_float`, `u.to_string`, `u.trim`, `u.str_len`
- Function and effect helpers:
  - `def`, `call`, `effect`
- Event helpers use `w.event.*`:
  - `w.event.value`, `w.event.key`, `w.event.prevent`
- Pipeline operator is `pipe` (not `$pipe`).

## NPM Packaging Decision

Runtime can stay inline for now (zero dependency, easy SSR ship).
If split as NPM package later:
- Pros: versioning, reuse across runtimes, CDN cache.
- Cons: bundling/version skew risk with server renderer, extra distribution complexity.

Recommended now: keep embedded, then extract when API surface stabilizes.

## Web Logic Runtime Notes

- `http.get|post|put|delete|request` now performs real outbound HTTP (via `ureq`)
  with policy enforcement (`allowPatterns`, `allowMethods`, `timeoutMs`).
- `http_mocks` still has priority for deterministic tests/dev responses.
- Axum adapter resolves merged route policy from `matchRoute()` automatically
  when explicit policy is not passed to `execute_logic`.
- `x.*` operators are registered through the runtime extension registry:
  - `register_x_operator("x.name", op)`
  - `unregister_x_operator("x.name")`
  - `clear_x_operator_registry()`
