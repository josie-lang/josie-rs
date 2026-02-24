# josie-web

`josie-web` is a library crate for compile-first interactive web pages with Josie semantics.

## Public Interfaces

### Constants
- `JOSIE_WEB_KIND`
- `JOSIE_WEB_VERSION`
- `JOSIE_RUNTIME_JS`
- `JOSIE_RUNTIME_DEV_JS`

### Public Types
- `RuntimeMode` (`compiled` | `interpreted`)
- `CachePolicy` (`ram` | `disk` | `auto`)
- `CacheRef`
- `AllowListRule`
- `CompileInput`
- `DiagnosticLevel`
- `Diagnostic`
- `CompiledPage`
- `RenderInput`
- `RenderOutput`
- `StaticSiteBuildInput`
- `StaticSiteBuildOutput`
- `DevHmrConfig`
- `StaticSiteWatchInput`
- `JosieWebEngine`

### Public Methods (`JosieWebEngine`)
- `new()`
- `with_disk_root(root)`
- `compile(input)`
- `render(input)`
- `cache_get(cache_ref)`
- `cache_put(cache_ref, compiled)`
- `cache_invalidate(namespace, key)`
- `export_compiled(compiled, output_dir)`
- `build_static_site(input)`
- `watch_static_site(input)`

## Mechanism Flow

### Compile-first
1. Stitch component templates (`<j-component .../>`).
2. Extract/normalize `application/josie+json` program.
3. Apply processors (`tailwind`, `markdown`, `security`, `minify`).
4. Apply script/CSS/URL security walls from allow-list + load-scripts.
5. Extract reactive bindings and stamp `data-jid`.
6. Store compiled artifact by deterministic fingerprint.

### Render-fast
1. Resolve compiled artifact by fingerprint (or cache ref path).
2. Inject backend vars.
3. Apply markdown var rendering if enabled.
4. Re-apply URL wall post-injection.
5. Return final HTML with diagnostics.

### Export
`export_compiled(...)` writes:
- `index.html`
- runtime bundle (`josie-runtime.js` or `josie-runtime-dev.js`)
- `program.josie`
- `manifest.json`
- `program.compiled.js` (compiled mode)

### Static site build/watch
- `build_static_site(...)` maps hierarchical `pages/**/*.josieml` to route folders and exports each page.
- `watch_static_site(...)` does polling fingerprint watch and rebuild on change.
- Optional `dev_hmr` injects lightweight reload script + version file.

## Related Docs
- `crates/josie-web/docs/JOSIEML_SPEC.md`
- `crates/josie-web/docs/JOSIE_REACTIVE_PRIMITIVES.md`
- `crates/josie-web/docs/FEATURE_CHECKLIST.md`
- `crates/josie-web/docs/contracts/WebInterface.ts`
