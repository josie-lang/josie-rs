# JOSIEML Spec v0.1.0

## 1. File Shape

### 1.1 `.josieml`

- full HTML document or component fragment
- supports reactive attrs (`j-text`, `j-model`, `j-attr:*`, `@event`)
- may contain one `application/josie+json` block as executable payload

### 1.2 `application/josie+json` payload

```json
{
  "kind": "program",
  "version": "0.1.0",
  "state": {
    "client": {},
    "server": {}
  },
  "actions": {
    "action.name": { "runStep": "step.id" }
  },
  "resources": ["josie-runtime-js", "app-css"],
  "loadScripts": [
    "https://cdn.jsdelivr.net/npm/three@0.161.0/build/three.min.js"
  ],
  "refs": {},
  "memos": [],
  "effects": [],
  "allowList": [
    { "type": "js", "value": "/assets/runtime.js" },
    { "type": "css", "value": "/assets/app.css" },
    { "type": "url", "value": "https://api.example.com" },
    { "type": "feature", "value": "localStorage" }
  ],
  "steps": []
}
```

Reactive primitives (`refs`, `memos`, `effects`) are documented in:
`docs/JOSIE_REACTIVE_PRIMITIVES.md`.

Compile API also accepts:

```json
{
  "runtimeMode": "compiled | interpreted",
  "loadScripts": [
    "https://cdn.jsdelivr.net/npm/three@0.161.0/build/three.min.js",
    "https://cdn.jsdelivr.net/npm/three@0.161.0/build/three.min.js|sha384-..."
  ],
  "cachePolicy": "ram | disk | auto"
}
```

## 2. Step Contract

Each step is a definition unit. `args`, `do`, and `when` contain Josie tree expressions.

```json
{
  "id": "counter.inc",
  "type": "action",
  "op": "do",
  "args": [
    ["set", "client.count", ["+", ["var", "client.count"], 1]]
  ],
  "timeout_ms": 1000,
  "max_retries": 0,
  "on_error": "halt"
}
```

## 3. Compile-First Runtime

Compile pipeline:

1. parse `.josieml`
2. stitch components
3. extract Program payload
4. run processors in configured order (`markdown`, `tailwind`, `security`, `minify`)
5. validate Program + bindings
6. enforce security walls (script/css/url)
7. extract reactive bindings (`j-text`, `j-model`, `j-attr:*`, map source) and inject stable `data-jid`
  - if `data-josie-map.source` is not a static string, compiler falls back to wildcard map subscription
8. compile `steps` expressions to native JS (`program.compiled.js`) in compiled mode
9. emit compiled artifact (for RAM/Disk cache)

Runtime pipeline:

1. load compiled artifact
2. hydrate state
3. load runtime bundle:
`josie-runtime.js` for compiled mode,
`josie-runtime-dev.js` for interpreted mode
4. index `data-jid` elements + install compile-time subscription map (`path -> updater[]`)
5. resolve event action -> step id (delegated listener per event type)
6. execute compiled step plan (compiled mode) or interpreter plan (interpreted mode)
7. run reactive primitives (`memos`, `effects`) by dependency path
8. flush settled changes, then notify bindings once (batched)
9. patch affected bindings/components only (no full-body render on each action)
  - map/list nodes use keyed reconciliation (`data-josie-map.key`) when provided

Subscription internals:
- exact-path index + descendant-prefix index + wildcard bucket (`*`)
- avoids full subscription-map scan for each state write
- DOM updates happen after reactive settle, not immediately inside each `set`
- event listener count is bounded by distinct `@event` names, not element count

## 4. Components

- source remains `.josieml`
- support props and nested components
- compiler resolves component tree and emits reusable compiled units
- runtime instantiates component units with scoped props and shared app state
- list/map components use `data-josie-map`:
  - `source`: state path or expression returning array
  - `key` (optional): stable key expression/path for node reuse; falls back to `item.id` then index

## 4.1 Processor Behavior

- `markdown`: transforms `<j-md>...</j-md>` blocks into HTML at compile time, and resolves `j-md-var="path.to.markdown"` at render time using backend vars
- `tailwind`: scans static class tokens (template + class-like strings in Program state), compiles utility rules, injects `<style data-josie-tw>`
- `security`: script wall enforcement. only external scripts listed in `loadScripts` are kept; inline/unlisted scripts are removed
- `minify`: applied during export
- css wall: external stylesheet links (`<link rel="stylesheet" href="...">`) are filtered by `allowList` (`type: css` + `type: url`)
- url wall: disallowed URL attrs (`href/src/action/formaction/poster/data`) are stripped and inline `on*` handlers are stripped
- runtime url wall: dynamic URL requests (`fetch`, `XMLHttpRequest`, `WebSocket`) are blocked by `allowList type: url`
- runtime feature wall: `allowList type: feature` gates `localStorage`, `indexedDB`, `gps`, `camera`

Markdown link policy:
- link output from markdown (`[label](url)`) follows `allowList` rules with `type: "url"`
- `{"type":"url","value":"*"}` allows all URLs
- when URL is not allowed, the anchor is omitted and label text is kept

Script policy:
- script usage is deny-by-default
- only `<script src="...">` entries present in `loadScripts` are allowed
- if `allowList` includes `type: "js"` or `type: "url"` rules, script src must also satisfy those rules
- remote scripts (`http://`, `https://`, `//`) require `integrity` on the tag
- optional integrity pin syntax in `loadScripts`: `url|sha384-...` (or `url#sha384-...`)

Reactive primitives:
- `memos`: recomputed at init and when deps change
- `effects.immediate=true`: runs at init
- `effects.once=true`: runs once then skipped

## 5. Cache Policy

Supported policies:

- `ram`
- `disk`
- `auto` (L1 ram, L2 disk fallback)

Cache key must include:

- template hash
- component graph hash
- compiler/runtime version
- processors config (`tailwind`, `markdown`, `security`, `minify`)
- allowlist/security policy hash

## 6. Export

Export emits static deployable output:

- `index.html`
- `program.josie`
- `manifest.json`
- runtime bundle:
`josie-runtime.js` (compiled mode) or `josie-runtime-dev.js` (interpreted mode)
- `program.compiled.js` (compiled mode only)
- deterministic fingerprint for cache/versioning

Hierarchical static-site build is supported via engine API:
- default folders:
  - `pages/` (`.josieml`)
  - `components/` (`.josieml`)
  - `public/` (copied as-is)
- route mapping:
  - `pages/index.josieml` -> `/index.html`
  - `pages/about.josieml` -> `/about/index.html`
  - `pages/blog/index.josieml` -> `/blog/index.html`
  - `pages/blog/post.josieml` -> `/blog/post/index.html`

## 7. Adapter Inputs

Adapters inject request context at runtime:

- route params
- query
- headers/cookies
- backend vars for SSR

No route ownership is moved from web framework into josie-web.
