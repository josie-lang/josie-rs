# Josie Web Feature Checklist

| Feature | Status | Notes |
|---|---|---|
| Full `.josieml` HTML authoring | implemented | Full page or component fragment |
| Program payload in `application/josie+json` | implemented | `kind/version/state/actions/resources/steps` |
| Reactive attrs (`j-text`, `j-model`, `j-attr:*`, `@event`) | implemented | Core interactive contract |
| Action -> step mapping (`runStep`) | implemented | Single-language execution path |
| Compile-first runtime | implemented | AOT `program.compiled.js` for compiled mode |
| Runtime mode switch (`compiled` / `interpreted`) | implemented | `CompileInput.runtimeMode` |
| Runtime bundle split | implemented | `josie-runtime.js` (kernel), `josie-runtime-dev.js` (interpreter) |
| Reusable components + nested components | implemented | Component stitch + cycle detection |
| Tailwind processor | implemented | Utility token compile + CSS injection (`<style data-josie-tw>`) |
| Tailwind CSS minify | implemented | Generated Tailwind CSS is minified before injection |
| Tailwind parity roadmap | documented | See `docs/TAILWIND_PARITY_PLAN.md` for v4.2 gap + priority plan |
| Markdown processor | implemented | Converts `<j-md>...</j-md>` at compile and resolves `j-md-var` from render vars |
| Markdown URL allow-list | implemented | Markdown links filtered by `allowList` url rules (`*` supported) |
| Script security wall | implemented | Inline/unlisted scripts removed; only `loadScripts` are allowed |
| Script integrity enforcement | implemented | Remote scripts in `loadScripts` require `integrity`; optional pinned syntax `url|sha384-...` |
| CSS security wall | implemented | External stylesheet `<link rel="stylesheet">` filtered by `allowList` (`css` + `url`) |
| URL attribute wall | implemented | Disallowed URL attrs (`href/src/action/...`) removed from HTML and dynamic `j-attr:*` |
| Runtime feature wall | implemented | `allowList` feature rules gate `localStorage/indexedDB/gps/camera` at runtime |
| Runtime request wall | implemented | `fetch/XHR/WebSocket` blocked when URL violates allow-list rules |
| Minify processor | implemented | HTML/JS/manifest/program minify on export |
| Cache policy (`ram`/`disk`/`auto`) | implemented | L1/L2 cache behavior |
| Export static output | implemented | `index.html`, runtime bundle, `program.josie`, manifest |
| Hierarchical static-site build | implemented | Build from `pages/`, `components/`, `public/` folders with route mapping |
| Action-to-step contract validation | implemented | Compile fails on unknown step refs and duplicate/missing step IDs |
| Reactive primitives (`refs/memos/effects`) | implemented | Compile-time contract validation + runtime dependency-driven memo/effect execution |
| Compile-time binding extraction (`data-jid`) | implemented | Rust compiler emits stable reactive binding metadata for runtime subscriptions |
| Runtime subscription map updates | implemented | Runtime updates affected bindings by path; avoids full-body render on each action |
| Delegated event handling | implemented | One listener per event type; avoids per-element listener growth |
| Keyed map reconciliation | implemented | `data-josie-map.key` reuses DOM nodes; no full list teardown each update |
| Adapter-injected backend context | planned | Params/query/headers/cookies/vars |
| SSR route-level rendering | implemented | Render vars into compiled template |
| Diagnostics/trace | partial | Compile/render diagnostics exist; deep tracing TBD |
