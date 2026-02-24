# Josie Reactive Primitives (v0.1.0)

This document defines the current reactive contract used by `josie-web` runtime bundles:

- `josie-runtime.js` (compiled mode)
- `josie-runtime-dev.js` (interpreted mode)

The goal is React-grade behavior with deterministic, compile-validated contracts.

## 1. Concept Mapping

| React concept | Josie equivalent | Contract shape |
|---|---|---|
| `useRef` | `refs` metadata + stable state paths | `refs: { ... }` |
| `useMemo` | `memos[]` | `{ id, deps, runStep, into }` |
| `useCallback` | stable action names in `actions` | `actions: { "ui.submit": { runStep: "..." } }` |
| `useEffect` | `effects[]` | `{ id, deps, runStep, immediate?, once? }` |

Notes:
- `refs` is currently a compile-time shape check and naming contract, not a dedicated runtime DOM-ref API.
- `actions` are stable entry points; runtime dispatches them to steps.

## 2. Canonical Program Fragment

```json
{
  "kind": "program",
  "version": "0.1.0",
  "state": {
    "client": {
      "items": [],
      "filter": "all",
      "memo": {
        "visibleItems": []
      }
    }
  },
  "refs": {
    "scroll.container": { "kind": "dom" }
  },
  "actions": {
    "ui.refresh": { "runStep": "items.fetch" },
    "ui.submit": { "runStep": "form.submit" }
  },
  "memos": [
    {
      "id": "memo.visible_items",
      "deps": ["client.items", "client.filter"],
      "runStep": "memo.visible_items.compute",
      "into": "client.memo.visibleItems"
    }
  ],
  "effects": [
    {
      "id": "fx.persist_items",
      "deps": ["client.items"],
      "runStep": "fx.persist_items.run",
      "immediate": true,
      "once": false
    }
  ],
  "steps": [
    { "id": "items.fetch", "op": "return", "args": [true] },
    { "id": "form.submit", "op": "return", "args": [true] },
    { "id": "memo.visible_items.compute", "op": "return", "args": [["var", "client.items"]] },
    { "id": "fx.persist_items.run", "op": "return", "args": [null] }
  ]
}
```

## 3. Compile-Time Rules (Enforced)

These checks are enforced in `josie-web` compile validation:

1. `program.refs` must be an object when present.
2. `program.memos` must be an array when present.
3. `memos[].id` required, non-empty, unique.
4. `memos[].deps[]` entries must be non-empty strings.
5. `memos[].runStep` required and must reference an existing step.
6. `memos[].into` required, non-empty.
7. `program.effects` must be an array when present.
8. `effects[].id` required, non-empty, unique.
9. `effects[].deps[]` entries must be non-empty strings.
10. `effects[].runStep` required and must reference an existing step.
11. `effects[].immediate` and `effects[].once` must be booleans when present.

Additional behavior:
- `memos[].into` not starting with `client.` produces warning (not hard fail).

## 4. Runtime Execution Model

For both runtime bundles:

1. Runtime initializes program/state/actions/steps.
2. `initReactiveContracts()` normalizes memos/effects.
3. Initial run:
- all memos compute once and write into their `into` path,
- effects with `immediate: true` run once at init,
- `once: true` effects are tracked and skipped after first run.
4. State changes call `set(path, value)`.
5. `set` enqueues dirty paths (`enqueueReactivePath`) and does not directly flush DOM.
6. Microtask flush (`flushReactiveLoop`) runs reactive recomputation with a 64-pass guard.
7. After reactive settle, runtime calls `notify(path)` for each changed path.
8. `notify` triggers only subscribed bindings (exact/prefix/wildcard), not full tree updates.

## 5. Dependency Matching Semantics

Path dependency match uses these rules:

- exact match: `client.a` matches `client.a`
- parent -> child: `client.a` matches `client.a.b`
- child -> parent: `client.a.b` matches `client.a`
- wildcard dep `*` matches all

This applies to memo/effect dependency checks and binding subscriptions.

## 6. Binding-Level Reactivity

Compile step extracts static bindings and stamps `data-jid` on elements for:

- `j-text`
- `j-model`
- `j-attr:*`
- `data-josie-map` source

Runtime installs subscribers from compile metadata:

- exact index: `_subs`
- descendant index: `_subsDesc`
- wildcard bucket: `_subsWildcard`

Result:
- updates are path-driven and fine-grained,
- full `renderTree(document.body)` is only used as fallback when no bindings exist.

## 7. Practical Patterns

### 7.1 Derived state (memo style)

- Keep expensive list shaping in `memos`.
- Write result to `client.memo.*`.
- Bind UI to the memo path, not raw source paths.

### 7.2 Side effects (effect style)

- Use `effects` for persistence, analytics, sync jobs.
- Use `once: true` for one-time startup side effects.
- Use `immediate: true` only when startup execution is required.

### 7.3 Stable action identity

- Keep action names stable (`ui.submit`, `todo.toggle`) to avoid brittle templates.
- Remap internals by changing `runStep` targets, not template event names.

## 8. Current Limits

1. `refs` does not yet expose a dedicated runtime API for direct element handles.
2. Memo/effect contracts are program-level; no isolated component-local lifecycle boundary yet.
3. Reactive scope is path-based; avoid unstable/random path naming to keep updates predictable.

## 9. Authoring Guidance

1. Keep `deps` minimal and explicit.
2. Keep memos pure (derive data only).
3. Keep effects impure (IO/persistence/side-effects only).
4. Avoid circular state writes between memos/effects; runtime guard stops at 64 passes.
5. Prefer writing into `client.memo.*` and `client.ui.*` namespaces for clarity.
