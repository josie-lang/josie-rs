//! `josie-web` compile-first web runtime bridge.
//!
//! # Core Strategy
//! `josie-web` is intentionally split into two phases:
//!
//! 1. **Compile phase (`compile`)**
//!    - stitches component fragments,
//!    - extracts and normalizes `application/josie+json` program payload,
//!    - applies processors (`tailwind`, `markdown`, `security`, `minify`),
//!    - validates contract integrity (actions -> step refs),
//!    - snapshots a deterministic artifact keyed by fingerprint.
//!
//! 2. **Render phase (`render`)**
//!    - injects backend vars into compiled HTML,
//!    - applies render-time markdown bindings,
//!    - applies URL security wall again (post-injection hardening),
//!    - returns final HTML quickly from cached artifacts.
//!
//! This split keeps runtime hot-path tiny and deterministic:
//! expensive work is done once at compile time, while render stays close
//! to "string interpolation + security recheck".
//!
//! # Security Model (Frontend Boundary)
//! Security here is **deny-by-default** and layered:
//!
//! - Script wall: only `loadScripts` entries survive.
//! - Remote script integrity: external scripts require `integrity`;
//!   optional pin rule (`url|sha384-...`) is supported.
//! - CSS wall: stylesheet links filtered through allow-list rules.
//! - URL wall: disallowed URL attributes and inline `on*` handlers are removed.
//! - Runtime wall (JS runtime files): request APIs and selected browser features
//!   are guarded by the same allow-list policy.
//!
//! # Hyperspeed Direction
//! The current architecture is prepared for fine-grained reactivity:
//! compile-time binding extraction, path-based subscription maps, and batched flush
//! can be layered without changing public compile/render contracts.
//!
//! Current implementation:
//! - Rust compile pass extracts static bindings and stamps `data-jid`.
//! - Exported runtime receives binding metadata (`__JOSIE_BINDINGS__`).
//! - Runtime uses exact/prefix/wildcard subscription indexes.
//! - DOM notify runs after reactive settle flush, not on every `set`.
//! - Event handlers are delegated per event type (no per-node listener explosion).
//! - `data-josie-map` supports keyed reconciliation to avoid full list teardown.
//!
//! # Josie Equivalents (React-ish ergonomics)
//! See crate docs for canonical contracts:
//! - `useRef`-like: stable ref paths in program/state conventions.
//! - `useMemo`-like: dependency-scoped derived nodes/memos.
//! - `useCallback`-like: stable action names (action map).
//! - `useEffect`-like: dependency-triggered effect entries.
//! The engine validates structural integrity so these patterns remain safe.
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value, json};
use std::collections::{HashMap, HashSet};
use std::fs;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
#[allow(dead_code)]
mod tw;
pub mod jsminify;
use tw::token_css_rule;

pub const JOSIE_WEB_KIND: &str = "josie-web";
pub const JOSIE_WEB_VERSION: &str = "0.1.0";
pub const JOSIE_RUNTIME_JS: &str = include_str!("josie-runtime.js");
pub const JOSIE_RUNTIME_DEV_JS: &str = include_str!("josie-runtime-dev.js");
const TAILWIND_PREFLIGHT_CSS: &str = "*,:before,:after{box-sizing:border-box;border:0 solid #e5e7eb;}html{line-height:1.5;-webkit-text-size-adjust:100%;tab-size:4;font-family:ui-sans-serif,system-ui,sans-serif;}body{margin:0;line-height:inherit;}button,input,select,textarea{font:inherit;color:inherit;margin:0;padding:0;}h1,h2,h3,h4,h5,h6,p,ul,ol{margin:0;padding:0;}ul,ol{list-style:none;}";

/// Execution mode for client runtime assets.
///
/// - `Compiled`: loads `josie-runtime.js` kernel + generated `program.compiled.js`
/// - `Interpreted`: loads `josie-runtime-dev.js` and evaluates program at runtime
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "lowercase")]
pub enum RuntimeMode {
    Interpreted,
    Compiled,
}

impl Default for RuntimeMode {
    fn default() -> Self {
        Self::Compiled
    }
}

/// Compile artifact cache strategy.
///
/// - `Ram`: in-memory only
/// - `Disk`: on-disk envelope only
/// - `Auto`: L1 RAM + L2 disk
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum CachePolicy {
    Ram,
    Disk,
    Auto,
}

impl Default for CachePolicy {
    fn default() -> Self {
        Self::Auto
    }
}

/// Stable cache coordinate used by callers to control cache scope/versioning.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct CacheRef {
    pub namespace: String,
    pub key: String,
    pub version: Option<String>,
}

/// Allow-list rule used by compile/runtime security walls.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct AllowListRule {
    #[serde(rename = "type")]
    pub rule_type: String,
    pub value: String,
}

/// Compile input for `JosieWebEngine::compile`.
///
/// The compile path is designed to be deterministic:
/// identical input + processor/security config should produce the same fingerprint.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct CompileInput {
    pub template: String,
    #[serde(default)]
    pub components: HashMap<String, String>,
    #[serde(default)]
    pub processors: Vec<String>,
    #[serde(default)]
    pub allow_list: Vec<AllowListRule>,
    #[serde(default)]
    pub load_scripts: Vec<String>,
    pub runtime_mode: Option<RuntimeMode>,
    pub cache: Option<CacheRef>,
    pub cache_policy: Option<CachePolicy>,
}

/// Diagnostic severity level.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum DiagnosticLevel {
    Info,
    Warn,
    Error,
}

/// Compiler/render diagnostic entry.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Diagnostic {
    pub level: DiagnosticLevel,
    pub code: String,
    pub message: String,
}

/// Opaque compiled page handle returned from `compile`.
///
/// `fingerprint` is the primary identity and lookup key.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompiledPage {
    pub kind: String,
    pub version: String,
    pub fingerprint: String,
    pub cache: Option<CacheRef>,
    #[serde(default)]
    pub diagnostics: Vec<Diagnostic>,
}

/// Input for `render` using a previously compiled page.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RenderInput {
    pub compiled: CompiledPage,
    #[serde(default)]
    pub vars: Map<String, Value>,
}

/// Render output (HTML + diagnostics).
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct RenderOutput {
    pub html: String,
    #[serde(default)]
    pub diagnostics: Vec<Diagnostic>,
}

/// Input for static-site export from hierarchical folders.
///
/// Defaults:
/// - `pages_dir`: `"pages"`
/// - `components_dir`: `"components"`
/// - `public_dir`: `"public"`
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct StaticSiteBuildInput {
    pub source_dir: String,
    pub output_dir: String,
    pub pages_dir: Option<String>,
    pub components_dir: Option<String>,
    pub public_dir: Option<String>,
    #[serde(default)]
    pub processors: Vec<String>,
    #[serde(default)]
    pub allow_list: Vec<AllowListRule>,
    #[serde(default)]
    pub load_scripts: Vec<String>,
    pub runtime_mode: Option<RuntimeMode>,
    pub cache_policy: Option<CachePolicy>,
    pub dev_hmr: Option<DevHmrConfig>,
}

/// Static-site build summary.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct StaticSiteBuildOutput {
    pub pages_built: usize,
    pub assets_copied: usize,
    #[serde(default)]
    pub generated_routes: Vec<String>,
}

/// Dev hot-reload behavior for static-site builds.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DevHmrConfig {
    #[serde(default = "default_hmr_enabled")]
    pub enabled: bool,
    pub websocket_url: Option<String>,
    pub version_path: Option<String>,
    pub poll_interval_ms: Option<u64>,
}

fn default_hmr_enabled() -> bool {
    true
}

/// Input for file-watch static-site rebuild loop.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct StaticSiteWatchInput {
    pub build: StaticSiteBuildInput,
    pub watch_paths: Option<Vec<String>>,
    pub poll_interval_ms: Option<u64>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct CompiledArtifact {
    html: String,
    program: Value,
    #[serde(default)]
    allow_list: Vec<AllowListRule>,
    #[serde(default)]
    load_scripts: Vec<String>,
    #[serde(default)]
    bindings: Vec<ReactiveBinding>,
    runtime_mode: RuntimeMode,
    components_used: Vec<String>,
    processors: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
/// Compile-time extracted reactive binding used by runtime fine-grained updates.
///
/// - `jid`: stable DOM target (`data-jid`)
/// - `kind`: `text | model | attr | map`
/// - `path`: dependency path in client state (or `*` wildcard fallback)
/// - `attr`: only used by `kind = attr`
struct ReactiveBinding {
    jid: String,
    kind: String,
    path: String,
    #[serde(default)]
    attr: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct CacheEnvelope {
    page: CompiledPage,
    artifact: CompiledArtifact,
}

/// Main `josie-web` engine.
///
/// The engine keeps:
/// - compiled artifacts by fingerprint,
/// - optional cache envelopes by caller-controlled cache refs.
#[derive(Debug, Clone, Default)]
pub struct JosieWebEngine {
    disk_root: PathBuf,
    cache_by_ref: HashMap<String, CacheEnvelope>,
    artifact_by_fingerprint: HashMap<String, CompiledArtifact>,
}

impl JosieWebEngine {
    /// Create a new engine using default disk cache root (`.josie-web-cache`).
    pub fn new() -> Self {
        Self {
            disk_root: PathBuf::from(".josie-web-cache"),
            cache_by_ref: HashMap::new(),
            artifact_by_fingerprint: HashMap::new(),
        }
    }

    /// Override disk cache root.
    pub fn with_disk_root(mut self, root: impl AsRef<Path>) -> Self {
        self.disk_root = root.as_ref().to_path_buf();
        self
    }

    /// Compile template + program payload into a reusable compiled page.
    ///
    /// This performs structure normalization, processor execution, security wall
    /// hardening, contract validation, compile-time binding extraction, and
    /// deterministic fingerprinting.
    pub fn compile(&mut self, input: CompileInput) -> Result<CompiledPage, String> {
        let policy = input.cache_policy.clone().unwrap_or_default();
        let runtime_mode = input.runtime_mode.clone().unwrap_or_default();
        if let Some(cache_ref) = input.cache.as_ref() {
            if let Some(hit) = self.cache_get_with_policy(cache_ref, &policy)? {
                if let Some(hit_artifact) = self.artifact_by_fingerprint.get(&hit.fingerprint) {
                    if hit_artifact.runtime_mode == runtime_mode {
                        return Ok(hit);
                    }
                } else {
                    return Ok(hit);
                }
            }
        }

        let mut diagnostics = Vec::new();
        for processor in &input.processors {
            if processor != "tailwind"
                && processor != "markdown"
                && processor != "security"
                && processor != "minify"
            {
                diagnostics.push(Diagnostic {
                    level: DiagnosticLevel::Warn,
                    code: "unknown_processor".to_string(),
                    message: format!("processor '{processor}' is not implemented; skipped"),
                });
            }
        }

        let stitched = stitch_components(&input.template, &input.components)?;
        let (template_without_program, maybe_program) = extract_program_payload(&stitched.source)?;
        let mut program = maybe_program.unwrap_or_else(|| {
            diagnostics.push(Diagnostic {
                level: DiagnosticLevel::Warn,
                code: "missing_program_payload".to_string(),
                message: "missing application/josie+json program payload; default inserted"
                    .to_string(),
            });
            json!({
                "kind": "program",
                "version": "0.1.0",
                "state": {},
                "actions": {},
                "resources": [],
                "steps": []
            })
        });

        normalize_program_payload(&mut program, &mut diagnostics)?;
        validate_program_contract(&program, &mut diagnostics)?;
        let effective_allow_list = merged_allow_list(&input.allow_list, &program, &mut diagnostics);
        let effective_load_scripts =
            merged_load_scripts(&input.load_scripts, &program, &mut diagnostics);

        // Global cleanup of any remaining {{p. tokens in the template
        let mut final_template = template_without_program;
        while let Some(start) = final_template.find("{{p.") {
            if let Some(rel_end) = final_template[start..].find("}}") {
                let end = start + rel_end + 2;
                final_template.replace_range(start..end, "");
            } else {
                break;
            }
        }

        let mut processed_html = apply_processors(
            &final_template,
            &program,
            &input.processors,
            &effective_allow_list,
            &mut diagnostics,
        );

        processed_html = apply_script_security_wall(
            &processed_html,
            &effective_load_scripts,
            &effective_allow_list,
            &mut diagnostics,
        );
        processed_html = apply_css_security_wall(&processed_html, &effective_allow_list, &mut diagnostics);
        processed_html =
            apply_url_attribute_security_wall(&processed_html, &effective_allow_list, &mut diagnostics);
        let (processed_html, bindings) =
            extract_reactive_bindings_and_annotate_html(&processed_html, &mut diagnostics);

        let fingerprint = fingerprint_for(
            &processed_html,
            &program,
            &effective_allow_list,
            &effective_load_scripts,
            &runtime_mode,
            &stitched.components_used,
            &input.processors,
        );

        let artifact = CompiledArtifact {
            html: processed_html,
            program,
            allow_list: effective_allow_list,
            load_scripts: effective_load_scripts,
            bindings,
            runtime_mode,
            components_used: stitched.components_used,
            processors: input.processors,
        };
        self.artifact_by_fingerprint
            .insert(fingerprint.clone(), artifact.clone());

        let page = CompiledPage {
            kind: JOSIE_WEB_KIND.to_string(),
            version: JOSIE_WEB_VERSION.to_string(),
            fingerprint,
            cache: input.cache.clone(),
            diagnostics,
        };

        if let Some(cache_ref) = input.cache.as_ref() {
            self.cache_put_with_policy(cache_ref, page.clone(), artifact, &policy)?;
        }

        Ok(page)
    }

    /// Render HTML from a previously compiled page with backend vars.
    ///
    /// Render is intentionally lightweight and re-applies URL security filtering
    /// after variable injection.
    pub fn render(&mut self, input: RenderInput) -> Result<RenderOutput, String> {
        let mut diagnostics = input.compiled.diagnostics.clone();

        if !self
            .artifact_by_fingerprint
            .contains_key(&input.compiled.fingerprint)
        {
            if let Some(cache_ref) = input.compiled.cache.as_ref() {
                let _ = self.cache_get(cache_ref);
            }
        }

        let artifact = self
            .artifact_by_fingerprint
            .get(&input.compiled.fingerprint)
            .ok_or_else(|| {
                format!(
                    "compiled artifact not found for fingerprint {}",
                    input.compiled.fingerprint
                )
            })?;

        let mut html = inject_vars(&artifact.html, &input.vars);
        if has_processor(&artifact.processors, "markdown") {
            html = apply_markdown_bindings_from_vars(&html, &input.vars, &artifact.allow_list);
        }
        html = apply_url_attribute_security_wall(&html, &artifact.allow_list, &mut diagnostics);
        diagnostics.push(Diagnostic {
            level: DiagnosticLevel::Info,
            code: "render_ok".to_string(),
            message: "rendered from compiled artifact".to_string(),
        });

        Ok(RenderOutput { html, diagnostics })
    }

    /// Render HTML and inject runtime bootstrap scripts.
    ///
    /// This is the right method for server endpoints that return a complete
    /// interactive page in one response.
    ///
    /// - `inline_assets = false`: keeps external runtime references
    ///   (`./josie-runtime*.js`, `./program.compiled.js`).
    /// - `inline_assets = true`: inlines runtime JS payloads into HTML, so no
    ///   extra static asset route is required.
    pub fn render_with_runtime(
        &mut self,
        input: RenderInput,
        inline_assets: bool,
    ) -> Result<RenderOutput, String> {
        let mut rendered = self.render(input.clone())?;
        let artifact = self
            .artifact_by_fingerprint
            .get(&input.compiled.fingerprint)
            .ok_or_else(|| {
                format!(
                    "compiled artifact not found for fingerprint {}",
                    input.compiled.fingerprint
                )
            })?;

        let mut html = inject_runtime_bootstrap(
            &rendered.html,
            &artifact.program,
            &artifact.allow_list,
            &artifact.load_scripts,
            &artifact.bindings,
            &artifact.runtime_mode,
            &input.vars,
        )?;

        if inline_assets {
            let use_minify = has_processor(&artifact.processors, "minify");
            let runtime_asset = runtime_asset_name(&artifact.runtime_mode);
            let mut runtime_js = runtime_asset_source(&artifact.runtime_mode).to_string();
            if use_minify {
                runtime_js = minify_js_light(&runtime_js);
            }
            let mut compiled_js = None;
            if artifact.runtime_mode == RuntimeMode::Compiled {
                let mut js = compile_program_to_js(
                    &artifact.program,
                    &artifact.allow_list,
                    &artifact.load_scripts,
                    &artifact.bindings,
                )?;
                if use_minify {
                    js = minify_js_light(&js);
                }
                compiled_js = Some(js);
            }
            html = inline_runtime_assets(&html, runtime_asset, &runtime_js, compiled_js.as_deref());
        }

        rendered.html = html;
        rendered.diagnostics.push(Diagnostic {
            level: DiagnosticLevel::Info,
            code: "runtime_bootstrap_injected".to_string(),
            message: if inline_assets {
                "runtime bootstrap injected with inline assets".to_string()
            } else {
                "runtime bootstrap injected with external assets".to_string()
            },
        });
        Ok(rendered)
    }

    /// Fetch cached compiled page by cache ref (`Auto` policy path).
    pub fn cache_get(&mut self, cache_ref: &CacheRef) -> Option<CompiledPage> {
        self.cache_get_with_policy(cache_ref, &CachePolicy::Auto)
            .ok()
            .flatten()
    }

    /// Persist compiled page into cache (`Auto` policy path).
    pub fn cache_put(
        &mut self,
        cache_ref: &CacheRef,
        compiled: CompiledPage,
    ) -> Result<(), String> {
        let artifact = self
            .artifact_by_fingerprint
            .get(&compiled.fingerprint)
            .ok_or_else(|| {
                format!(
                    "cannot cache compiled page without artifact for fingerprint {}",
                    compiled.fingerprint
                )
            })?
            .clone();

        self.cache_put_with_policy(cache_ref, compiled, artifact, &CachePolicy::Auto)
    }

    /// Invalidate cache entries by namespace and optional key prefix.
    ///
    /// Returns removed entry count.
    pub fn cache_invalidate(&mut self, namespace: &str, key: Option<&str>) -> usize {
        let prefix = if let Some(key) = key {
            format!("{}/{}", sanitize_segment(namespace), sanitize_segment(key))
        } else {
            format!("{}/", sanitize_segment(namespace))
        };

        let keys: Vec<String> = self
            .cache_by_ref
            .keys()
            .filter(|k| k.starts_with(&prefix))
            .cloned()
            .collect();
        let removed = keys.len();
        for k in keys {
            self.cache_by_ref.remove(&k);
            let path = self.disk_root.join(format!("{k}.json"));
            let _ = fs::remove_file(path);
        }
        removed
    }

    /// Export static deployable artifacts for a compiled page.
    ///
    /// Emits:
    /// - `index.html`
    /// - runtime asset (`josie-runtime.js` or `josie-runtime-dev.js`)
    /// - `program.josie`
    /// - `manifest.json`
    /// - `program.compiled.js` (compiled mode only)
    pub fn export_compiled(
        &self,
        compiled: &CompiledPage,
        output_dir: impl AsRef<Path>,
    ) -> Result<(), String> {
        let artifact = self
            .artifact_by_fingerprint
            .get(&compiled.fingerprint)
            .ok_or_else(|| {
                format!(
                    "cannot export missing artifact for fingerprint {}",
                    compiled.fingerprint
                )
            })?;

        let out = output_dir.as_ref();
        fs::create_dir_all(out).map_err(|e| format!("failed to create export dir: {e}"))?;

        let use_minify = has_processor(&artifact.processors, "minify");
        let runtime_asset = runtime_asset_name(&artifact.runtime_mode);
        let runtime_source = runtime_asset_source(&artifact.runtime_mode);
        let mut html = inject_runtime_bootstrap(
            &artifact.html,
            &artifact.program,
            &artifact.allow_list,
            &artifact.load_scripts,
            &artifact.bindings,
            &artifact.runtime_mode,
            &Map::new(),
        )?;
        if use_minify {
            html = minify_html_light(&html);
        }
        fs::write(out.join("index.html"), html)
            .map_err(|e| format!("failed to write index.html: {e}"))?;

        let mut runtime_js = runtime_source.to_string();
        if use_minify {
            runtime_js = minify_js_light(&runtime_js);
        }
        fs::write(out.join(runtime_asset), runtime_js)
            .map_err(|e| format!("failed to write {runtime_asset}: {e}"))?;
        let stale_runtime = if runtime_asset == "josie-runtime.js" {
            "josie-runtime-dev.js"
        } else {
            "josie-runtime.js"
        };
        let _ = fs::remove_file(out.join(stale_runtime));

        if artifact.runtime_mode == RuntimeMode::Compiled {
            let mut compiled_js = compile_program_to_js(
                &artifact.program,
                &artifact.allow_list,
                &artifact.load_scripts,
                &artifact.bindings,
            )?;
            if use_minify {
                compiled_js = minify_js_light(&compiled_js);
            }
            fs::write(out.join("program.compiled.js"), compiled_js)
                .map_err(|e| format!("failed to write program.compiled.js: {e}"))?;
        } else {
            let _ = fs::remove_file(out.join("program.compiled.js"));
        }

        let program_text = if use_minify {
            serde_json::to_string(&artifact.program)
                .map_err(|e| format!("failed to encode minified program.josie: {e}"))?
        } else {
            serde_json::to_string_pretty(&artifact.program)
                .map_err(|e| format!("failed to encode program.josie: {e}"))?
        };
        fs::write(
            out.join("program.josie"),
            program_text,
        )
        .map_err(|e| format!("failed to write program.josie: {e}"))?;

        let manifest = json!({
            "kind": compiled.kind,
            "version": compiled.version,
            "fingerprint": compiled.fingerprint,
            "runtime_mode": artifact.runtime_mode,
            "runtime_asset": runtime_asset,
            "components": artifact.components_used,
            "processors": artifact.processors,
        });
        let manifest_text = if use_minify {
            serde_json::to_string(&manifest)
                .map_err(|e| format!("failed to encode minified manifest.json: {e}"))?
        } else {
            serde_json::to_string_pretty(&manifest)
                .map_err(|e| format!("failed to encode manifest.json: {e}"))?
        };
        fs::write(
            out.join("manifest.json"),
            manifest_text,
        )
        .map_err(|e| format!("failed to write manifest.json: {e}"))?;

        Ok(())
    }

    /// Build a static website from hierarchical folders.
    ///
    /// Folder layout defaults:
    /// - `pages/` for route templates (`.josieml`)
    /// - `components/` for reusable component templates (`.josieml`)
    /// - `public/` for passthrough static assets
    ///
    /// Route mapping:
    /// - `pages/index.josieml` -> `/index.html`
    /// - `pages/about.josieml` -> `/about/index.html`
    /// - `pages/blog/index.josieml` -> `/blog/index.html`
    /// - `pages/blog/post.josieml` -> `/blog/post/index.html`
    pub fn build_static_site(
        &mut self,
        input: StaticSiteBuildInput,
    ) -> Result<StaticSiteBuildOutput, String> {
        let source_root = PathBuf::from(&input.source_dir);
        let output_root = PathBuf::from(&input.output_dir);
        let pages_dir = input.pages_dir.as_deref().unwrap_or("pages");
        let components_dir = input.components_dir.as_deref().unwrap_or("components");
        let public_dir = input.public_dir.as_deref().unwrap_or("public");

        let pages_root = source_root.join(pages_dir);
        if !pages_root.exists() {
            return Err(format!("pages directory not found: {}", pages_root.display()));
        }
        if !pages_root.is_dir() {
            return Err(format!(
                "pages path is not a directory: {}",
                pages_root.display()
            ));
        }

        fs::create_dir_all(&output_root)
            .map_err(|e| format!("failed to create output dir {}: {e}", output_root.display()))?;

        let components_root = source_root.join(components_dir);
        let components = load_components_from_dir(&components_root)?;

        let mut page_files = Vec::<PathBuf>::new();
        collect_josieml_files(&pages_root, &mut page_files)?;
        page_files.sort();
        if page_files.is_empty() {
            return Err(format!(
                "no .josieml pages found under {}",
                pages_root.display()
            ));
        }

        let mut generated_routes = Vec::<String>::new();
        let mut generated_page_dirs = Vec::<PathBuf>::new();
        for page_path in page_files {
            let template = fs::read_to_string(&page_path)
                .map_err(|e| format!("failed to read page {}: {e}", page_path.display()))?;
            let rel = page_path
                .strip_prefix(&pages_root)
                .map_err(|e| format!("failed to resolve page path {}: {e}", page_path.display()))?;
            let route = route_from_page_rel(rel)?;
            let page_output_dir = output_dir_from_page_rel(&output_root, rel)?;

            let compiled = self.compile(CompileInput {
                template,
                components: components.clone(),
                processors: input.processors.clone(),
                allow_list: input.allow_list.clone(),
                load_scripts: input.load_scripts.clone(),
                runtime_mode: input.runtime_mode.clone(),
                cache: None,
                cache_policy: input.cache_policy.clone(),
            })?;
            self.export_compiled(&compiled, &page_output_dir)?;
            generated_routes.push(route);
            generated_page_dirs.push(page_output_dir);
        }

        let public_root = source_root.join(public_dir);
        let assets_copied = if public_root.exists() && public_root.is_dir() {
            copy_dir_recursive(&public_root, &output_root)?
        } else {
            0usize
        };

        if let Some(dev_hmr) = input.dev_hmr.as_ref() {
            if dev_hmr.enabled {
                let ws_url = dev_hmr
                    .websocket_url
                    .clone()
                    .unwrap_or_else(|| "ws://127.0.0.1:35729/josie-hmr".to_string());
                let version_path = dev_hmr
                    .version_path
                    .clone()
                    .unwrap_or_else(|| "/__josie_hmr_version.txt".to_string());
                let poll_ms = dev_hmr.poll_interval_ms.unwrap_or(1000).max(100);

                write_hmr_version_file(&output_root, &version_path)?;
                for page_dir in &generated_page_dirs {
                    let index_path = page_dir.join("index.html");
                    if !index_path.exists() {
                        continue;
                    }
                    let html = fs::read_to_string(&index_path).map_err(|e| {
                        format!("failed to read page for hmr injection {}: {e}", index_path.display())
                    })?;
                    let html =
                        inject_hmr_script_if_missing(&html, &ws_url, &version_path, poll_ms)?;
                    fs::write(&index_path, html).map_err(|e| {
                        format!(
                            "failed to write page after hmr injection {}: {e}",
                            index_path.display()
                        )
                    })?;
                }
            }
        }

        generated_routes.sort();
        Ok(StaticSiteBuildOutput {
            pages_built: generated_routes.len(),
            assets_copied,
            generated_routes,
        })
    }

    /// Watch hierarchical folders and rebuild static site on changes.
    ///
    /// This uses polling-based fingerprinting (no extra watcher dependency):
    /// - checks watched paths every `poll_interval_ms`,
    /// - rebuilds when fingerprint changes,
    /// - runs forever until process exit.
    pub fn watch_static_site(&mut self, input: StaticSiteWatchInput) -> Result<(), String> {
        let source_root = PathBuf::from(&input.build.source_dir);
        let default_watch = vec![
            input
                .build
                .pages_dir
                .clone()
                .unwrap_or_else(|| "pages".to_string()),
            input
                .build
                .components_dir
                .clone()
                .unwrap_or_else(|| "components".to_string()),
            input
                .build
                .public_dir
                .clone()
                .unwrap_or_else(|| "public".to_string()),
        ];
        let watch_rel = input.watch_paths.unwrap_or(default_watch);
        let watch_paths = watch_rel
            .into_iter()
            .map(|p| source_root.join(p))
            .collect::<Vec<_>>();

        let interval = std::time::Duration::from_millis(input.poll_interval_ms.unwrap_or(500).max(100));
        let mut last_fp = source_fingerprint(&watch_paths)?;
        let _ = self.build_static_site(input.build.clone())?;

        loop {
            std::thread::sleep(interval);
            let next_fp = source_fingerprint(&watch_paths)?;
            if next_fp == last_fp {
                continue;
            }
            last_fp = next_fp;
            let _ = self.build_static_site(input.build.clone())?;
        }
    }

    fn cache_get_with_policy(
        &mut self,
        cache_ref: &CacheRef,
        policy: &CachePolicy,
    ) -> Result<Option<CompiledPage>, String> {
        let key = cache_ref_key(cache_ref);
        match policy {
            CachePolicy::Ram => Ok(self.cache_by_ref.get(&key).map(|c| c.page.clone())),
            CachePolicy::Disk => {
                let loaded = self.load_from_disk(&key)?;
                Ok(loaded.map(|c| c.page))
            }
            CachePolicy::Auto => {
                if let Some(hit) = self.cache_by_ref.get(&key) {
                    return Ok(Some(hit.page.clone()));
                }
                let loaded = self.load_from_disk(&key)?;
                if let Some(env) = loaded {
                    self.artifact_by_fingerprint
                        .insert(env.page.fingerprint.clone(), env.artifact.clone());
                    self.cache_by_ref.insert(key, env.clone());
                    return Ok(Some(env.page));
                }
                Ok(None)
            }
        }
    }

    fn cache_put_with_policy(
        &mut self,
        cache_ref: &CacheRef,
        page: CompiledPage,
        artifact: CompiledArtifact,
        policy: &CachePolicy,
    ) -> Result<(), String> {
        let key = cache_ref_key(cache_ref);
        let envelope = CacheEnvelope { page, artifact };

        match policy {
            CachePolicy::Ram => {
                self.cache_by_ref.insert(key, envelope);
            }
            CachePolicy::Disk => {
                self.persist_to_disk(&key, &envelope)?;
            }
            CachePolicy::Auto => {
                self.cache_by_ref.insert(key.clone(), envelope.clone());
                self.persist_to_disk(&key, &envelope)?;
            }
        }
        Ok(())
    }

    fn persist_to_disk(&self, key: &str, env: &CacheEnvelope) -> Result<(), String> {
        let path = self.disk_root.join(format!("{key}.json"));
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)
                .map_err(|e| format!("failed to create cache dir {}: {e}", parent.display()))?;
        }
        fs::write(
            &path,
            serde_json::to_vec(env).map_err(|e| format!("failed to encode cache envelope: {e}"))?,
        )
        .map_err(|e| format!("failed to write cache file {}: {e}", path.display()))?;
        Ok(())
    }

    fn load_from_disk(&self, key: &str) -> Result<Option<CacheEnvelope>, String> {
        let path = self.disk_root.join(format!("{key}.json"));
        if !path.exists() {
            return Ok(None);
        }
        let bytes = fs::read(&path)
            .map_err(|e| format!("failed to read cache file {}: {e}", path.display()))?;
        let env: CacheEnvelope = serde_json::from_slice(&bytes)
            .map_err(|e| format!("invalid cache envelope {}: {e}", path.display()))?;
        Ok(Some(env))
    }
}

fn is_josieml_file(path: &Path) -> bool {
    path
        .extension()
        .and_then(|s| s.to_str())
        .map(|s| s.eq_ignore_ascii_case("josieml"))
        .unwrap_or(false)
}

fn collect_josieml_files(dir: &Path, out: &mut Vec<PathBuf>) -> Result<(), String> {
    let mut entries = Vec::new();
    for entry in fs::read_dir(dir).map_err(|e| format!("failed to read dir {}: {e}", dir.display()))? {
        let entry = entry.map_err(|e| format!("failed to read dir entry in {}: {e}", dir.display()))?;
        entries.push(entry.path());
    }
    entries.sort();

    for path in entries {
        if path.is_dir() {
            collect_josieml_files(&path, out)?;
            continue;
        }
        if is_josieml_file(&path) {
            out.push(path);
        }
    }
    Ok(())
}

fn path_to_slash(path: &Path) -> String {
    path.to_string_lossy().replace('\\', "/")
}

fn load_components_from_dir(components_root: &Path) -> Result<HashMap<String, String>, String> {
    let mut components = HashMap::<String, String>::new();
    if !components_root.exists() || !components_root.is_dir() {
        return Ok(components);
    }

    let mut files = Vec::<PathBuf>::new();
    collect_josieml_files(components_root, &mut files)?;
    files.sort();

    for file in files {
        let rel = file
            .strip_prefix(components_root)
            .map_err(|e| format!("failed to resolve component path {}: {e}", file.display()))?;
        let key_path = rel.with_extension("");
        let key = path_to_slash(&key_path);
        let body = fs::read_to_string(&file)
            .map_err(|e| format!("failed to read component {}: {e}", file.display()))?;
        components.insert(key.clone(), body.clone());
        if let Some(stem) = key_path.file_name().and_then(|s| s.to_str()) {
            components.entry(stem.to_string()).or_insert(body);
        }
    }

    Ok(components)
}

fn output_dir_from_page_rel(output_root: &Path, rel_page: &Path) -> Result<PathBuf, String> {
    let rel_no_ext = rel_page.with_extension("");
    let Some(file_name) = rel_no_ext.file_name().and_then(|s| s.to_str()) else {
        return Err(format!("invalid page path: {}", rel_page.display()));
    };
    if file_name == "index" {
        if let Some(parent) = rel_no_ext.parent() {
            if parent.as_os_str().is_empty() {
                return Ok(output_root.to_path_buf());
            }
            return Ok(output_root.join(parent));
        }
        return Ok(output_root.to_path_buf());
    }
    Ok(output_root.join(rel_no_ext))
}

fn route_from_page_rel(rel_page: &Path) -> Result<String, String> {
    let rel_no_ext = rel_page.with_extension("");
    let Some(file_name) = rel_no_ext.file_name().and_then(|s| s.to_str()) else {
        return Err(format!("invalid page path: {}", rel_page.display()));
    };
    if file_name == "index" {
        let parent = rel_no_ext.parent().unwrap_or_else(|| Path::new(""));
        if parent.as_os_str().is_empty() {
            return Ok("/".to_string());
        }
        return Ok(format!("/{}", path_to_slash(parent)));
    }
    Ok(format!("/{}", path_to_slash(&rel_no_ext)))
}

fn copy_dir_recursive(src: &Path, dst: &Path) -> Result<usize, String> {
    let mut copied = 0usize;
    let mut entries = Vec::new();
    for entry in fs::read_dir(src).map_err(|e| format!("failed to read dir {}: {e}", src.display()))? {
        let entry = entry.map_err(|e| format!("failed to read dir entry in {}: {e}", src.display()))?;
        entries.push(entry.path());
    }
    entries.sort();

    for path in entries {
        let rel = path
            .strip_prefix(src)
            .map_err(|e| format!("failed to resolve asset path {}: {e}", path.display()))?;
        let out_path = dst.join(rel);
        if path.is_dir() {
            fs::create_dir_all(&out_path).map_err(|e| {
                format!(
                    "failed to create output asset dir {}: {e}",
                    out_path.display()
                )
            })?;
            copied += copy_dir_recursive(&path, &out_path)?;
        } else {
            if let Some(parent) = out_path.parent() {
                fs::create_dir_all(parent).map_err(|e| {
                    format!(
                        "failed to create parent dir for asset {}: {e}",
                        out_path.display()
                    )
                })?;
            }
            fs::copy(&path, &out_path).map_err(|e| {
                format!(
                    "failed to copy asset {} -> {}: {e}",
                    path.display(),
                    out_path.display()
                )
            })?;
            copied += 1;
        }
    }
    Ok(copied)
}

/// Writes a monotonically-changing dev version marker for static-site HMR.
///
/// The browser-side HMR helper can poll this file and trigger a reload when the
/// value changes. The file path is relative to output root; leading `/` is allowed.
fn write_hmr_version_file(output_root: &Path, version_path: &str) -> Result<(), String> {
    let rel = version_path.trim_start_matches('/');
    let target = output_root.join(rel);
    if let Some(parent) = target.parent() {
        fs::create_dir_all(parent).map_err(|e| {
            format!(
                "failed to create hmr version parent {}: {e}",
                parent.display()
            )
        })?;
    }
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map_err(|e| format!("failed to read system time for hmr version: {e}"))?
        .as_millis();
    fs::write(&target, format!("{now}\n"))
        .map_err(|e| format!("failed to write hmr version file {}: {e}", target.display()))?;
    Ok(())
}

/// Injects a tiny HMR client script into HTML only once.
///
/// Injection marker: `data-josie-hmr="1"`.
/// Placement priority: before `</body>`, otherwise append at end.
fn inject_hmr_script_if_missing(
    html: &str,
    websocket_url: &str,
    version_path: &str,
    poll_ms: u64,
) -> Result<String, String> {
    if html.contains("data-josie-hmr=\"1\"") {
        return Ok(html.to_string());
    }

    let ws_json = serde_json::to_string(websocket_url)
        .map_err(|e| format!("failed to encode hmr websocket url: {e}"))?;
    let version_json = serde_json::to_string(version_path)
        .map_err(|e| format!("failed to encode hmr version path: {e}"))?;

    let mut script = String::from("<script data-josie-hmr=\"1\">(function(){");
    script.push_str("const WS=");
    script.push_str(&ws_json);
    script.push(';');
    script.push_str("const VERSION=");
    script.push_str(&version_json);
    script.push(';');
    script.push_str("const POLL=");
    script.push_str(&poll_ms.to_string());
    script.push(';');
    script.push_str("let baseVer=null;");
    script.push_str("async function getVer(){try{const r=await fetch(VERSION,{cache:'no-store'});if(!r.ok)return null;return (await r.text()).trim();}catch(_e){return null;}}");
    script.push_str("function hardReload(){window.location.reload();}");
    script.push_str("function connect(){try{const w=new WebSocket(WS);w.onmessage=function(ev){if((ev&&ev.data)==='reload')hardReload();};w.onclose=function(){setTimeout(connect,500);};w.onerror=function(){try{w.close();}catch(_e){}};}catch(_e){}}");
    script.push_str("connect();");
    script.push_str("setInterval(async function(){const next=await getVer();if(next==null)return;if(baseVer==null){baseVer=next;return;}if(next!==baseVer)hardReload();},POLL);");
    script.push_str("})();</script>");

    if let Some(idx) = html.to_ascii_lowercase().rfind("</body>") {
        let mut out = String::with_capacity(html.len() + script.len());
        out.push_str(&html[..idx]);
        out.push_str(&script);
        out.push_str(&html[idx..]);
        return Ok(out);
    }

    let mut out = String::with_capacity(html.len() + script.len());
    out.push_str(html);
    out.push_str(&script);
    Ok(out)
}

/// Computes deterministic source fingerprint for dev watch mode.
///
/// Inputs:
/// - all watched roots
/// - each traversed file/dir relative path
/// - file length + modified timestamp (millis, best effort)
///
/// Output is a fixed FNV-1a hex string used to detect any change.
fn source_fingerprint(paths: &[PathBuf]) -> Result<String, String> {
    let mut hasher = Fnv1aHasher::new();
    for root in paths {
        let root_tag = format!("root:{}", path_to_slash(root));
        root_tag.hash(&mut hasher);
        fingerprint_walk(root, root, &mut hasher)?;
    }
    Ok(format!("{:016x}", hasher.finish()))
}

fn fingerprint_walk(root: &Path, path: &Path, hasher: &mut Fnv1aHasher) -> Result<(), String> {
    if !path.exists() {
        let marker = format!("missing:{}", path_to_slash(path));
        marker.hash(hasher);
        return Ok(());
    }

    let meta = fs::metadata(path)
        .map_err(|e| format!("failed to stat watched path {}: {e}", path.display()))?;
    let rel = path
        .strip_prefix(root)
        .unwrap_or(path)
        .to_string_lossy()
        .replace('\\', "/");

    if meta.is_file() {
        format!("file:{rel}").hash(hasher);
        meta.len().hash(hasher);
        let modified_ms = meta
            .modified()
            .ok()
            .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
            .map(|d| d.as_millis() as u64)
            .unwrap_or(0);
        modified_ms.hash(hasher);
        return Ok(());
    }

    if meta.is_dir() {
        format!("dir:{rel}").hash(hasher);
        let mut entries = Vec::<PathBuf>::new();
        for entry in fs::read_dir(path)
            .map_err(|e| format!("failed to read watched dir {}: {e}", path.display()))?
        {
            let entry = entry
                .map_err(|e| format!("failed to read watched entry in {}: {e}", path.display()))?;
            entries.push(entry.path());
        }
        entries.sort();
        for child in entries {
            fingerprint_walk(root, &child, hasher)?;
        }
    }

    Ok(())
}

#[derive(Debug, Clone)]
struct StitchResult {
    source: String,
    components_used: Vec<String>,
}

fn stitch_components(template: &str, components: &HashMap<String, String>) -> Result<StitchResult, String> {
    let mut used = HashSet::new();
    let mut stack = Vec::new();
    let source = stitch_components_inner(template, components, &mut used, &mut stack)?;
    let mut components_used: Vec<String> = used.into_iter().collect();
    components_used.sort();
    Ok(StitchResult {
        source,
        components_used,
    })
}

fn stitch_components_inner(
    source: &str,
    components: &HashMap<String, String>,
    used: &mut HashSet<String>,
    stack: &mut Vec<String>,
) -> Result<String, String> {
    let mut out = String::new();
    let mut cursor = 0usize;

    loop {
        let lower = source[cursor..].to_ascii_lowercase();
        let Some(rel) = lower.find("<j-component") else {
            out.push_str(&source[cursor..]);
            break;
        };
        let start = cursor + rel;
        out.push_str(&source[cursor..start]);

        let open_end = find_tag_end(source, start).ok_or_else(|| "unterminated <j-component> tag".to_string())?;
        let open = &source[start + 1..open_end];
        let self_closing = open.trim_end().ends_with('/');
        let name = extract_attr_value(open, "name")
            .or_else(|| extract_attr_value(open, "slug"))
            .ok_or_else(|| "j-component requires name or slug attribute".to_string())?;

        let props = extract_props(open);

        if stack.iter().any(|v| v == &name) {
            return Err(format!("cyclic j-component include detected: {}", name));
        }
        let comp_src = components
            .get(&name)
            .ok_or_else(|| format!("component '{}' not found", name))?;

        used.insert(name.clone());
        stack.push(name.clone());
        let mut expanded = stitch_components_inner(comp_src, components, used, stack)?;
        stack.pop();

        // Inject props into the FULL expanded component source
        for (k, v) in props {
            let target = format!("{{{{p.{k}}}}}");
            expanded = expanded.replace(&target, &v);
        }

        out.push_str(&expanded);

        if self_closing {
            cursor = open_end + 1;
            continue;
        }

        let close_tag = "</j-component>";
        let lower_rest = source[open_end + 1..].to_ascii_lowercase();
        if let Some(rel_close) = lower_rest.find(close_tag) {
            cursor = open_end + 1 + rel_close + close_tag.len();
        } else {
            return Err("missing </j-component> closing tag".to_string());
        }
    }

    Ok(out)
}

fn extract_program_payload(source: &str) -> Result<(String, Option<Value>), String> {
    let mut template = source.to_string();
    let mut merged_payload = json!({
        "kind": "program",
        "version": "0.1.0",
        "state": {},
        "actions": {},
        "resources": [],
        "steps": [],
        "memos": [],
        "effects": []
    });
    let mut found = false;
    let mut cursor = 0usize;

    while cursor < template.len() {
        let lower = template[cursor..].to_ascii_lowercase();
        let Some(rel_start) = lower.find("<script") else {
            break;
        };
        let start = cursor + rel_start;
        let Some(open_end) = find_tag_end(&template, start) else {
            cursor = start + 7;
            continue;
        };
        
        let open = &template[start..open_end];
        let script_type = extract_attr_value(open, "type")
            .unwrap_or_default()
            .to_ascii_lowercase();

        if script_type == "application/josie+json" {
            let Some(close_start) = find_close_tag(&template, open_end + 1, "script") else {
                cursor = open_end + 1;
                continue;
            };
            let close_end = close_start + "</script>".len();
            let payload_text = template[open_end + 1..close_start].trim();
            let payload = if payload_text.is_empty() {
                json!({})
            } else {
                serde_json::from_str::<Value>(payload_text)
                    .map_err(|e| format!("invalid application/josie+json payload: {e}"))?
            };

            merge_programs(&mut merged_payload, payload);
            found = true;

            template.replace_range(start..close_end, "");
            // No need to advance cursor as we removed characters, 
            // but we'll stay at 'start' to find the next tag that now moved here.
            cursor = start;
        } else {
            cursor = open_end + 1;
        }
    }

    Ok((template, if found { Some(merged_payload) } else { None }))
}

fn deep_merge(target: &mut Value, source: Value) {
    match (target, source) {
        (Value::Object(t_map), Value::Object(s_map)) => {
            for (k, v) in s_map {
                deep_merge(t_map.entry(k).or_insert(Value::Null), v);
            }
        }
        (t, s) => {
            *t = s;
        }
    }
}

fn merge_programs(target: &mut Value, source: Value) {
    let Some(t) = target.as_object_mut() else { return };
    let Some(s) = source.as_object() else { return };

    // Deep merge state
    if let Some(s_state) = s.get("state").cloned() {
        let t_state = t.entry("state").or_insert_with(|| json!({}));
        deep_merge(t_state, s_state);
    }

    // Deep merge actions
    if let Some(s_actions) = s.get("actions").cloned() {
        let t_actions = t.entry("actions").or_insert_with(|| json!({}));
        deep_merge(t_actions, s_actions);
    }

    // Merge steps (append)
    if let Some(s_steps) = s.get("steps").and_then(Value::as_array) {
        let t_steps = t.entry("steps").or_insert_with(|| json!([])).as_array_mut().unwrap();
        for step in s_steps {
            t_steps.push(step.clone());
        }
    }

    // Merge memos (append)
    if let Some(s_memos) = s.get("memos").and_then(Value::as_array) {
        let t_memos = t.entry("memos").or_insert_with(|| json!([])).as_array_mut().unwrap();
        for memo in s_memos {
            t_memos.push(memo.clone());
        }
    }

    // Merge effects (append)
    if let Some(s_effects) = s.get("effects").and_then(Value::as_array) {
        let t_effects = t.entry("effects").or_insert_with(|| json!([])).as_array_mut().unwrap();
        for effect in s_effects {
            t_effects.push(effect.clone());
        }
    }
}

/// Normalize program payload into canonical shape while preserving backward compatibility.
///
/// Legacy fields (`ui.resources`, `web.resources`) are migrated into top-level `resources`.
fn normalize_program_payload(program: &mut Value, diagnostics: &mut Vec<Diagnostic>) -> Result<(), String> {
    let Some(obj) = program.as_object_mut() else {
        return Err("program payload must be JSON object".to_string());
    };

    if obj.get("kind").and_then(Value::as_str) != Some("program") {
        diagnostics.push(Diagnostic {
            level: DiagnosticLevel::Warn,
            code: "program_kind_normalized".to_string(),
            message: "kind was missing/invalid and normalized to 'program'".to_string(),
        });
        obj.insert("kind".to_string(), Value::String("program".to_string()));
    }

    if obj.get("version").and_then(Value::as_str).is_none() {
        diagnostics.push(Diagnostic {
            level: DiagnosticLevel::Warn,
            code: "program_version_default".to_string(),
            message: "version missing; defaulted to 0.1.0".to_string(),
        });
        obj.insert("version".to_string(), Value::String("0.1.0".to_string()));
    }

    if !obj.get("state").map(Value::is_object).unwrap_or(false) {
        obj.insert("state".to_string(), Value::Object(Map::new()));
    }

    if !obj.get("steps").map(Value::is_array).unwrap_or(false) {
        obj.insert("steps".to_string(), Value::Array(Vec::new()));
    }

    if !obj.get("actions").map(Value::is_object).unwrap_or(false) {
        if let Some(legacy) = obj.get("ui").and_then(|v| v.get("actions")).cloned() {
            diagnostics.push(Diagnostic {
                level: DiagnosticLevel::Warn,
                code: "legacy_ui_actions".to_string(),
                message: "ui.actions detected; mirrored into top-level actions".to_string(),
            });
            obj.insert("actions".to_string(), legacy);
        } else if let Some(legacy) = obj.get("web").and_then(|v| v.get("actions")).cloned() {
            diagnostics.push(Diagnostic {
                level: DiagnosticLevel::Warn,
                code: "legacy_web_actions".to_string(),
                message: "web.actions detected; mirrored into top-level actions".to_string(),
            });
            obj.insert("actions".to_string(), legacy);
        } else {
            obj.insert("actions".to_string(), Value::Object(Map::new()));
        }
    }

    if !obj.get("resources").map(Value::is_array).unwrap_or(false) {
        if let Some(legacy) = obj.get("ui").and_then(|v| v.get("resources")).cloned() {
            diagnostics.push(Diagnostic {
                level: DiagnosticLevel::Warn,
                code: "legacy_ui_resources".to_string(),
                message: "ui.resources detected; mirrored into top-level resources".to_string(),
            });
            obj.insert("resources".to_string(), legacy);
        } else if let Some(legacy) = obj.get("web").and_then(|v| v.get("resources")).cloned() {
            diagnostics.push(Diagnostic {
                level: DiagnosticLevel::Warn,
                code: "legacy_web_resources".to_string(),
                message: "web.resources detected; mirrored into top-level resources".to_string(),
            });
            obj.insert("resources".to_string(), legacy);
        } else {
            obj.insert("resources".to_string(), Value::Array(Vec::new()));
        }
    }

    Ok(())
}

/// Validate structural program contracts required by `josie-web` runtime.
///
/// Guarantees:
/// - `steps` is an object array with unique non-empty `id`.
/// - `actions` references only existing step ids (`runStep`, `run[].step`, or string shortcut).
/// - emits `dead_step` warning for unreferenced steps.
fn validate_program_contract(program: &Value, diagnostics: &mut Vec<Diagnostic>) -> Result<(), String> {
    let Some(obj) = program.as_object() else {
        return Err("program payload must be a JSON object".to_string());
    };

    let steps = obj
        .get("steps")
        .and_then(Value::as_array)
        .ok_or_else(|| "program.steps must be an array".to_string())?;

    let mut step_ids = HashSet::<String>::new();
    for (idx, step) in steps.iter().enumerate() {
        let Some(step_obj) = step.as_object() else {
            return Err(format!("program.steps[{idx}] must be an object"));
        };
        let Some(step_id) = step_obj.get("id").and_then(Value::as_str) else {
            return Err(format!("program.steps[{idx}].id is required"));
        };
        let normalized = step_id.trim();
        if normalized.is_empty() {
            return Err(format!("program.steps[{idx}].id must not be empty"));
        }
        if !step_ids.insert(normalized.to_string()) {
            return Err(format!("duplicate step id detected: {normalized}"));
        }
    }

    let actions = obj
        .get("actions")
        .and_then(Value::as_object)
        .ok_or_else(|| "program.actions must be an object".to_string())?;
    let mut referenced = HashSet::<String>::new();

    for (name, action) in actions {
        if let Some(step_id) = action.as_str() {
            referenced.insert(step_id.to_string());
            continue;
        }
        let Some(action_obj) = action.as_object() else {
            continue;
        };
        if let Some(step_id) = action_obj.get("runStep").and_then(Value::as_str) {
            referenced.insert(step_id.to_string());
        }
        if let Some(run) = action_obj.get("run").and_then(Value::as_array) {
            for (run_idx, entry) in run.iter().enumerate() {
                let Some(entry_obj) = entry.as_object() else {
                    continue;
                };
                if let Some(step_id) = entry_obj.get("step").and_then(Value::as_str) {
                    referenced.insert(step_id.to_string());
                } else if entry_obj.contains_key("step") {
                    return Err(format!(
                        "program.actions.{name}.run[{run_idx}].step must be a string"
                    ));
                }
            }
        }
    }

    if let Some(refs) = obj.get("refs") {
        if !refs.is_object() {
            return Err("program.refs must be an object when present".to_string());
        }
    }

    if let Some(memos) = obj.get("memos") {
        let Some(list) = memos.as_array() else {
            return Err("program.memos must be an array when present".to_string());
        };
        let mut memo_ids = HashSet::<String>::new();
        for (idx, memo) in list.iter().enumerate() {
            let Some(memo_obj) = memo.as_object() else {
                return Err(format!("program.memos[{idx}] must be an object"));
            };
            let Some(memo_id) = memo_obj.get("id").and_then(Value::as_str) else {
                return Err(format!("program.memos[{idx}].id is required"));
            };
            let memo_id = memo_id.trim();
            if memo_id.is_empty() {
                return Err(format!("program.memos[{idx}].id must not be empty"));
            }
            if !memo_ids.insert(memo_id.to_string()) {
                return Err(format!("duplicate memo id detected: {memo_id}"));
            }

            if let Some(deps) = memo_obj.get("deps") {
                let Some(dep_list) = deps.as_array() else {
                    return Err(format!("program.memos[{idx}].deps must be an array"));
                };
                for (dep_idx, dep) in dep_list.iter().enumerate() {
                    let Some(dep_str) = dep.as_str() else {
                        return Err(format!(
                            "program.memos[{idx}].deps[{dep_idx}] must be a string"
                        ));
                    };
                    if dep_str.trim().is_empty() {
                        return Err(format!(
                            "program.memos[{idx}].deps[{dep_idx}] must not be empty"
                        ));
                    }
                }
            }

            let Some(run_step) = memo_obj.get("runStep").and_then(Value::as_str) else {
                return Err(format!("program.memos[{idx}].runStep is required"));
            };
            if run_step.trim().is_empty() {
                return Err(format!("program.memos[{idx}].runStep must not be empty"));
            }
            referenced.insert(run_step.to_string());

            let Some(into) = memo_obj.get("into").and_then(Value::as_str) else {
                return Err(format!("program.memos[{idx}].into is required"));
            };
            if into.trim().is_empty() {
                return Err(format!("program.memos[{idx}].into must not be empty"));
            }
            if !into.starts_with("client.") {
                diagnostics.push(Diagnostic {
                    level: DiagnosticLevel::Warn,
                    code: "memo_into_non_client".to_string(),
                    message: format!(
                        "program.memos[{idx}].into ('{into}') does not start with client."
                    ),
                });
            }
        }
    }

    if let Some(effects) = obj.get("effects") {
        let Some(list) = effects.as_array() else {
            return Err("program.effects must be an array when present".to_string());
        };
        let mut effect_ids = HashSet::<String>::new();
        for (idx, effect) in list.iter().enumerate() {
            let Some(effect_obj) = effect.as_object() else {
                return Err(format!("program.effects[{idx}] must be an object"));
            };
            let Some(effect_id) = effect_obj.get("id").and_then(Value::as_str) else {
                return Err(format!("program.effects[{idx}].id is required"));
            };
            let effect_id = effect_id.trim();
            if effect_id.is_empty() {
                return Err(format!("program.effects[{idx}].id must not be empty"));
            }
            if !effect_ids.insert(effect_id.to_string()) {
                return Err(format!("duplicate effect id detected: {effect_id}"));
            }

            if let Some(deps) = effect_obj.get("deps") {
                let Some(dep_list) = deps.as_array() else {
                    return Err(format!("program.effects[{idx}].deps must be an array"));
                };
                for (dep_idx, dep) in dep_list.iter().enumerate() {
                    let Some(dep_str) = dep.as_str() else {
                        return Err(format!(
                            "program.effects[{idx}].deps[{dep_idx}] must be a string"
                        ));
                    };
                    if dep_str.trim().is_empty() {
                        return Err(format!(
                            "program.effects[{idx}].deps[{dep_idx}] must not be empty"
                        ));
                    }
                }
            }

            let Some(run_step) = effect_obj.get("runStep").and_then(Value::as_str) else {
                return Err(format!("program.effects[{idx}].runStep is required"));
            };
            if run_step.trim().is_empty() {
                return Err(format!("program.effects[{idx}].runStep must not be empty"));
            }
            referenced.insert(run_step.to_string());

            if let Some(immediate) = effect_obj.get("immediate") {
                if !immediate.is_boolean() {
                    return Err(format!(
                        "program.effects[{idx}].immediate must be a boolean"
                    ));
                }
            }
            if let Some(once) = effect_obj.get("once") {
                if !once.is_boolean() {
                    return Err(format!("program.effects[{idx}].once must be a boolean"));
                }
            }
        }
    }

    for step_id in &referenced {
        if !step_ids.contains(step_id) {
            return Err(format!(
                "program references missing step id '{step_id}' in program.steps"
            ));
        }
    }

    for step_id in &step_ids {
        if !referenced.contains(step_id) {
            diagnostics.push(Diagnostic {
                level: DiagnosticLevel::Warn,
                code: "dead_step".to_string(),
                message: format!("step '{step_id}' is not referenced by any action"),
            });
        }
    }

    Ok(())
}

fn inject_vars(html: &str, vars: &Map<String, Value>) -> String {
    let mut flat = HashMap::<String, String>::new();
    for (k, v) in vars {
        flatten_vars(k, v, &mut flat);
    }
    if flat.is_empty() {
        return html.to_string();
    }

    // Walk the HTML and only replace {{key}} tokens OUTSIDE <script> blocks.
    // Inside script content, HTML-escaped values are wrong (JS doesn't decode &amp; etc.)
    // and template substitution in script bodies is an injection risk.
    // Developers should use server.* state to pass data to the JS runtime instead.
    let mut out = String::with_capacity(html.len());
    let mut cursor = 0usize;
    let lower = html.to_ascii_lowercase();

    while cursor < html.len() {
        // Find the next <script opening tag
        if let Some(script_rel) = lower[cursor..].find("<script") {
            let script_start = cursor + script_rel;
            // Flush the chunk before the script tag — apply substitutions here
            let before = &html[cursor..script_start];
            out.push_str(&replace_vars_in_chunk(before, &flat));

            // Find the end of the opening <script ...> tag
            let tag_end = html[script_start..].find('>').map(|i| script_start + i + 1);
            if tag_end.is_none() {
                // Malformed — flush rest as-is
                out.push_str(&html[script_start..]);
                return out;
            }
            let tag_end = tag_end.unwrap();

            // Find </script>
            let close = lower[tag_end..].find("</script").map(|i| tag_end + i);
            if let Some(close_pos) = close {
                let close_end = html[close_pos..].find('>').map(|i| close_pos + i + 1).unwrap_or(close_pos + 9);
                // Emit script tag + body verbatim (no substitution)
                out.push_str(&html[script_start..close_end]);
                cursor = close_end;
            } else {
                // No closing tag — emit rest verbatim
                out.push_str(&html[script_start..]);
                return out;
            }
        } else {
            // No more script tags — apply substitutions to the rest
            out.push_str(&replace_vars_in_chunk(&html[cursor..], &flat));
            return out;
        }
    }
    out
}

fn replace_vars_in_chunk(chunk: &str, flat: &HashMap<String, String>) -> String {
    let mut out = chunk.to_string();
    for (k, v) in flat {
        let plain = format!("{{{{{k}}}}}");
        let spaced = format!("{{{{ {k} }}}}");
        out = out.replace(&plain, v);
        out = out.replace(&spaced, v);
    }
    out
}

fn flatten_vars(prefix: &str, value: &Value, out: &mut HashMap<String, String>) {
    match value {
        Value::Object(obj) => {
            for (k, v) in obj {
                let next = if prefix.is_empty() {
                    k.to_string()
                } else {
                    format!("{prefix}.{k}")
                };
                flatten_vars(&next, v, out);
            }
        }
        _ => {
            let val = match value {
                Value::Null => String::new(),
                Value::String(s) => s.clone(),
                Value::Number(n) => n.to_string(),
                Value::Bool(b) => b.to_string(),
                _ => value.to_string(),
            };
            out.insert(prefix.to_string(), escape_html(&val));
        }
    }
}

/// Deterministic FNV-1a 64-bit hasher (stable across Rust versions/restarts).
/// Unlike `DefaultHasher`, the algorithm is fixed by spec — safe for disk cache keys.
struct Fnv1aHasher(u64);

impl Fnv1aHasher {
    fn new() -> Self {
        Self(14695981039346656037u64)
    }
}

impl Hasher for Fnv1aHasher {
    fn finish(&self) -> u64 {
        self.0
    }
    fn write(&mut self, bytes: &[u8]) {
        const PRIME: u64 = 1099511628211;
        for &b in bytes {
            self.0 ^= b as u64;
            self.0 = self.0.wrapping_mul(PRIME);
        }
    }
}

fn fingerprint_for(
    html: &str,
    program: &Value,
    allow_list: &[AllowListRule],
    load_scripts: &[String],
    runtime_mode: &RuntimeMode,
    components: &[String],
    processors: &[String],
) -> String {
    let mut hasher = Fnv1aHasher::new();
    JOSIE_WEB_KIND.hash(&mut hasher);
    JOSIE_WEB_VERSION.hash(&mut hasher);
    html.hash(&mut hasher);
    program.to_string().hash(&mut hasher);
    allow_list.hash(&mut hasher);
    load_scripts.hash(&mut hasher);
    runtime_mode.hash(&mut hasher);
    components.hash(&mut hasher);
    processors.hash(&mut hasher);
    format!("{:016x}", hasher.finish())
}

fn cache_ref_key(cache_ref: &CacheRef) -> String {
    let namespace = sanitize_segment(&cache_ref.namespace);
    let key = sanitize_segment(&cache_ref.key);
    let version = cache_ref
        .version
        .as_deref()
        .map(sanitize_segment)
        .unwrap_or_else(|| "latest".to_string());
    format!("{namespace}/{key}@{version}")
}

fn sanitize_segment(input: &str) -> String {
    input
        .chars()
        .map(|ch| {
            if ch.is_ascii_alphanumeric() || ch == '-' || ch == '_' || ch == '.' {
                ch
            } else {
                '_'
            }
        })
        .collect()
}

fn extract_attr_value(tag_inner: &str, key: &str) -> Option<String> {
    let mut cursor = 0usize;
    while cursor < tag_inner.len() {
        let rest = &tag_inner[cursor..];
        let Some(eq_rel) = rest.find('=') else { break; };
        let key_part = rest[..eq_rel].trim();
        let key_name = key_part.split_whitespace().last().unwrap_or_default();
        let after_eq = cursor + eq_rel + 1;
        
        if after_eq >= tag_inner.len() { break; }
        let Some(quote) = tag_inner[after_eq..].chars().next() else { break; };
        
        if quote == '"' || quote == '\'' {
            let mut end_idx = after_eq + quote.len_utf8();
            while end_idx < tag_inner.len() {
                let Some(ch) = tag_inner[end_idx..].chars().next() else { break; };
                if ch == quote {
                    let value = &tag_inner[after_eq + quote.len_utf8()..end_idx];
                    if key_name.eq_ignore_ascii_case(key) {
                        return Some(value.to_string());
                    }
                    break;
                }
                end_idx += ch.len_utf8();
            }
            cursor = end_idx.saturating_add(1);
        } else {
            // Unquoted attribute value
            let mut end_idx = after_eq;
            while end_idx < tag_inner.len() {
                let Some(ch) = tag_inner[end_idx..].chars().next() else { break; };
                if ch.is_whitespace() || ch == '>' {
                    break;
                }
                end_idx += ch.len_utf8();
            }
            let value = &tag_inner[after_eq..end_idx];
            if key_name.eq_ignore_ascii_case(key) {
                return Some(value.to_string());
            }
            cursor = end_idx;
        }
    }
    None
}

fn extract_props(tag_inner: &str) -> HashMap<String, String> {
    let mut props = HashMap::new();
    let mut cursor = 0usize;
    while cursor < tag_inner.len() {
        let rest = &tag_inner[cursor..];
        let Some(eq_rel) = rest.find('=') else { break };
        let key_part = rest[..eq_rel].trim();
        let key_name = key_part.split_whitespace().last().unwrap_or_default();
        let after_eq = cursor + eq_rel + 1;
        if after_eq >= tag_inner.len() { break };
        
        let Some(quote) = tag_inner[after_eq..].chars().next() else { break };
        if quote == '"' || quote == '\'' {
            let mut end_idx = after_eq + quote.len_utf8();
            while end_idx < tag_inner.len() {
                let Some(ch) = tag_inner[end_idx..].chars().next() else { break; };
                if ch == quote {
                    let value = &tag_inner[after_eq + quote.len_utf8()..end_idx];
                    if key_name.starts_with("p-") {
                        props.insert(key_name[2..].to_string(), value.to_string());
                    }
                    break;
                }
                end_idx += ch.len_utf8();
            }
            cursor = end_idx.saturating_add(1);
        } else {
            // Unquoted
            let mut end_idx = after_eq;
            while end_idx < tag_inner.len() {
                let Some(ch) = tag_inner[end_idx..].chars().next() else { break; };
                if ch.is_whitespace() || ch == '>' || ch == '/' {
                    break;
                }
                end_idx += ch.len_utf8();
            }
            let value = &tag_inner[after_eq..end_idx];
            if key_name.starts_with("p-") {
                props.insert(key_name[2..].to_string(), value.to_string());
            }
            cursor = end_idx;
        }
    }
    props
}

fn find_tag_end(source: &str, start: usize) -> Option<usize> {
    let mut i = start;
    let mut quote: Option<char> = None;
    while i < source.len() {
        let ch = source[i..].chars().next()?;
        if let Some(q) = quote {
            if ch == q {
                quote = None;
            }
            i += ch.len_utf8();
            continue;
        }
        if ch == '"' || ch == '\'' {
            quote = Some(ch);
            i += ch.len_utf8();
            continue;
        }
        if ch == '>' {
            return Some(i);
        }
        i += ch.len_utf8();
    }
    None
}

fn find_close_tag(source: &str, from: usize, tag: &str) -> Option<usize> {
    let close = format!("</{}>", tag.to_ascii_lowercase());
    let lower = source[from..].to_ascii_lowercase();
    lower.find(&close).map(|rel| from + rel)
}

fn apply_processors(
    html: &str,
    program: &Value,
    processors: &[String],
    allow_list: &[AllowListRule],
    diagnostics: &mut Vec<Diagnostic>,
) -> String {
    let mut out = html.to_string();
    for processor in processors {
        match processor.as_str() {
            "markdown" => {
                out = apply_markdown_processor(&out, allow_list);
            }
            "tailwind" => {
                out = apply_tailwind_processor(&out, program, diagnostics);
            }
            "security" | "minify" => {}
            _ => {}
        }
    }
    out
}

fn apply_markdown_processor(html: &str, allow_list: &[AllowListRule]) -> String {
    let mut out = String::new();
    let mut cursor = 0usize;
    let lower = html.to_ascii_lowercase();

    while let Some(rel_idx) = lower[cursor..].find("<j-md") {
        let open_start = cursor + rel_idx;
        out.push_str(&html[cursor..open_start]);
        let Some(open_end) = find_tag_end(html, open_start) else {
            out.push_str(&html[open_start..]);
            return out;
        };
        let content_start = open_end + 1;
        let Some(close_start) = find_close_tag(html, content_start, "j-md") else {
            out.push_str(&html[open_start..]);
            return out;
        };
        let close_end = close_start + "</j-md>".len();
        let markdown_raw = &html[content_start..close_start];
        out.push_str(&render_markdown_basic(markdown_raw, allow_list));
        cursor = close_end;
    }

    out.push_str(&html[cursor..]);
    out
}

fn render_markdown_basic(input: &str, allow_list: &[AllowListRule]) -> String {
    let mut out = String::new();
    let mut in_ul = false;
    let mut in_code = false;
    let mut paragraph = String::new();

    for raw_line in input.lines() {
        let line = raw_line.trim_end();
        let trimmed = line.trim();

        if trimmed.starts_with("```") {
            if !paragraph.is_empty() {
                out.push_str("<p>");
                out.push_str(&markdown_inline(&paragraph, allow_list));
                out.push_str("</p>");
                paragraph.clear();
            }
            if in_ul {
                out.push_str("</ul>");
                in_ul = false;
            }
            if in_code {
                out.push_str("</code></pre>");
                in_code = false;
            } else {
                out.push_str("<pre><code>");
                in_code = true;
            }
            continue;
        }

        if in_code {
            out.push_str(&escape_html(trimmed));
            out.push('\n');
            continue;
        }

        if trimmed.is_empty() {
            if !paragraph.is_empty() {
                out.push_str("<p>");
                out.push_str(&markdown_inline(&paragraph, allow_list));
                out.push_str("</p>");
                paragraph.clear();
            }
            if in_ul {
                out.push_str("</ul>");
                in_ul = false;
            }
            continue;
        }

        if let Some(rest) = trimmed.strip_prefix("# ") {
            flush_markdown_paragraph(&mut out, &mut paragraph, &mut in_ul, allow_list);
            out.push_str("<h1>");
            out.push_str(&markdown_inline(rest, allow_list));
            out.push_str("</h1>");
            continue;
        }
        if let Some(rest) = trimmed.strip_prefix("## ") {
            flush_markdown_paragraph(&mut out, &mut paragraph, &mut in_ul, allow_list);
            out.push_str("<h2>");
            out.push_str(&markdown_inline(rest, allow_list));
            out.push_str("</h2>");
            continue;
        }
        if let Some(rest) = trimmed.strip_prefix("### ") {
            flush_markdown_paragraph(&mut out, &mut paragraph, &mut in_ul, allow_list);
            out.push_str("<h3>");
            out.push_str(&markdown_inline(rest, allow_list));
            out.push_str("</h3>");
            continue;
        }

        if let Some(rest) = trimmed.strip_prefix("- ").or_else(|| trimmed.strip_prefix("* ")) {
            if !paragraph.is_empty() {
                out.push_str("<p>");
                out.push_str(&markdown_inline(&paragraph, allow_list));
                out.push_str("</p>");
                paragraph.clear();
            }
            if !in_ul {
                out.push_str("<ul>");
                in_ul = true;
            }
            out.push_str("<li>");
            out.push_str(&markdown_inline(rest, allow_list));
            out.push_str("</li>");
            continue;
        }

        if in_ul {
            out.push_str("</ul>");
            in_ul = false;
        }

        if !paragraph.is_empty() {
            paragraph.push(' ');
        }
        paragraph.push_str(trimmed);
    }

    if in_code {
        out.push_str("</code></pre>");
    }
    if in_ul {
        out.push_str("</ul>");
    }
    if !paragraph.is_empty() {
        out.push_str("<p>");
        out.push_str(&markdown_inline(&paragraph, allow_list));
        out.push_str("</p>");
    }

    out
}

fn flush_markdown_paragraph(
    out: &mut String,
    paragraph: &mut String,
    in_ul: &mut bool,
    allow_list: &[AllowListRule],
) {
    if !paragraph.is_empty() {
        out.push_str("<p>");
        out.push_str(&markdown_inline(paragraph, allow_list));
        out.push_str("</p>");
        paragraph.clear();
    }
    if *in_ul {
        out.push_str("</ul>");
        *in_ul = false;
    }
}

fn markdown_inline(text: &str, allow_list: &[AllowListRule]) -> String {
    let escaped = escape_html(text);
    let code_applied = replace_inline_marker(&escaped, "`", "<code>", "</code>");
    let bold_applied = replace_inline_marker(&code_applied, "**", "<strong>", "</strong>");
    let italic_applied = replace_inline_marker(&bold_applied, "*", "<em>", "</em>");
    replace_markdown_links(&italic_applied, allow_list)
}

fn replace_inline_marker(text: &str, marker: &str, open: &str, close: &str) -> String {
    if marker.is_empty() {
        return text.to_string();
    }
    let mut out = String::new();
    let mut cursor = 0usize;
    let mut opened = false;
    while let Some(rel) = text[cursor..].find(marker) {
        let pos = cursor + rel;
        out.push_str(&text[cursor..pos]);
        if opened {
            out.push_str(close);
        } else {
            out.push_str(open);
        }
        opened = !opened;
        cursor = pos + marker.len();
    }
    out.push_str(&text[cursor..]);
    if opened {
        out.push_str(close);
    }
    out
}

fn replace_markdown_links(text: &str, allow_list: &[AllowListRule]) -> String {
    let mut out = String::new();
    let mut cursor = 0usize;
    while let Some(rel_open) = text[cursor..].find('[') {
        let open = cursor + rel_open;
        out.push_str(&text[cursor..open]);
        let Some(rel_mid) = text[open..].find("](") else {
            out.push_str(&text[open..]);
            return out;
        };
        let mid = open + rel_mid;
        let Some(rel_close) = text[mid + 2..].find(')') else {
            out.push_str(&text[open..]);
            return out;
        };
        let close = mid + 2 + rel_close;
        let label = &text[open + 1..mid];
        let url = &text[mid + 2..close];
        if is_url_allowed(url, allow_list) {
            out.push_str("<a href=\"");
            out.push_str(url);
            out.push_str("\">");
            out.push_str(label);
            out.push_str("</a>");
        } else {
            out.push_str(label);
        }
        cursor = close + 1;
    }
    out.push_str(&text[cursor..]);
    out
}

fn is_url_allowed(url: &str, allow_list: &[AllowListRule]) -> bool {
    let url_rules: Vec<&str> = allow_list
        .iter()
        .filter(|r| r.rule_type.eq_ignore_ascii_case("url"))
        .map(|r| r.value.as_str())
        .collect();

    if url_rules.is_empty() {
        // Deny-by-default when the developer has configured *any* allowList rules
        // (meaning security is active) but forgot to add URL rules.
        // When allowList is completely absent, stay permissive so unsecured pages work.
        // To allow all URLs explicitly, add {"type":"url","value":"*"} to allowList.
        return allow_list.is_empty();
    }
    if url_rules.iter().any(|v| *v == "*") {
        return true;
    }

    url_rules.iter().any(|allowed| {
        allow_rule_matches(url, allowed) || (!allowed.ends_with('/') && url.starts_with(allowed))
    })
}

fn allow_rule_matches(candidate: &str, rule_value: &str) -> bool {
    let rule = rule_value.trim();
    if rule.is_empty() {
        return false;
    }
    if rule == "*" {
        return true;
    }
    if candidate == rule {
        return true;
    }
    if rule.ends_with('/') && candidate.starts_with(rule) {
        return true;
    }
    false
}

fn is_css_allowed(href: &str, allow_list: &[AllowListRule]) -> bool {
    let css_rules: Vec<&str> = allow_list
        .iter()
        .filter(|r| r.rule_type.eq_ignore_ascii_case("css"))
        .map(|r| r.value.as_str())
        .collect();
    let url_rules: Vec<&str> = allow_list
        .iter()
        .filter(|r| r.rule_type.eq_ignore_ascii_case("url"))
        .map(|r| r.value.as_str())
        .collect();

    if !css_rules.is_empty() && !css_rules.iter().any(|rule| allow_rule_matches(href, rule)) {
        return false;
    }

    if !url_rules.is_empty()
        && !url_rules.iter().any(|rule| {
            allow_rule_matches(href, rule) || (!rule.ends_with('/') && href.starts_with(rule))
        })
    {
        return false;
    }

    true
}

#[derive(Debug, Clone)]
struct ParsedLoadScriptRule {
    src_rule: String,
    integrity_pin: Option<String>,
}

fn parse_load_script_rule(raw: &str) -> ParsedLoadScriptRule {
    let trimmed = raw.trim();
    if let Some((lhs, rhs)) = trimmed.split_once('|') {
        let src_rule = lhs.trim().to_string();
        let pin = rhs.trim();
        let integrity_pin = if pin.is_empty() {
            None
        } else {
            Some(pin.to_string())
        };
        return ParsedLoadScriptRule {
            src_rule,
            integrity_pin,
        };
    }

    if let Some((lhs, rhs)) = trimmed.rsplit_once('#') {
        let pin = rhs.trim();
        if pin.starts_with("sha") {
            return ParsedLoadScriptRule {
                src_rule: lhs.trim().to_string(),
                integrity_pin: Some(pin.to_string()),
            };
        }
    }

    ParsedLoadScriptRule {
        src_rule: trimmed.to_string(),
        integrity_pin: None,
    }
}

fn is_remote_script_src(src: &str) -> bool {
    let lowered = src.trim().to_ascii_lowercase();
    lowered.starts_with("https://") || lowered.starts_with("http://") || lowered.starts_with("//")
}

fn merged_load_scripts(
    input_load_scripts: &[String],
    program: &Value,
    diagnostics: &mut Vec<Diagnostic>,
) -> Vec<String> {
    let mut out = Vec::<String>::new();
    let mut seen = HashSet::<String>::new();

    for src in input_load_scripts {
        let normalized = src.trim().to_string();
        if normalized.is_empty() {
            continue;
        }
        if seen.insert(normalized.clone()) {
            out.push(normalized);
        }
    }

    if let Some(list) = program.get("loadScripts").and_then(Value::as_array) {
        for raw in list {
            let Some(src) = raw.as_str() else {
                continue;
            };
            let normalized = src.trim().to_string();
            if normalized.is_empty() {
                continue;
            }
            if seen.insert(normalized.clone()) {
                out.push(normalized);
            }
        }
    } else if program.get("loadScripts").is_some() {
        diagnostics.push(Diagnostic {
            level: DiagnosticLevel::Warn,
            code: "invalid_load_scripts".to_string(),
            message: "program.loadScripts must be an array of script src strings".to_string(),
        });
    }

    out
}

fn is_script_allowed_by_load_scripts(
    src: &str,
    integrity: Option<&str>,
    load_scripts: &[String],
) -> bool {
    for raw in load_scripts {
        let parsed = parse_load_script_rule(raw);
        if !allow_rule_matches(src, &parsed.src_rule) {
            continue;
        }
        if let Some(pin) = parsed.integrity_pin.as_deref() {
            if integrity.map(str::trim) == Some(pin) {
                return true;
            }
            continue;
        }
        if is_remote_script_src(src) && integrity.map(str::trim).unwrap_or_default().is_empty() {
            continue;
        }
        return true;
    }
    false
}

fn is_script_allowed_by_allow_list(src: &str, allow_list: &[AllowListRule]) -> bool {
    let js_rules: Vec<&str> = allow_list
        .iter()
        .filter(|r| r.rule_type.eq_ignore_ascii_case("js"))
        .map(|r| r.value.as_str())
        .collect();
    let url_rules: Vec<&str> = allow_list
        .iter()
        .filter(|r| r.rule_type.eq_ignore_ascii_case("url"))
        .map(|r| r.value.as_str())
        .collect();

    if !js_rules.is_empty() && !js_rules.iter().any(|rule| allow_rule_matches(src, rule)) {
        return false;
    }

    if !url_rules.is_empty()
        && !url_rules.iter().any(|rule| {
            allow_rule_matches(src, rule) || (!rule.ends_with('/') && src.starts_with(rule))
        })
    {
        return false;
    }

    true
}

fn has_attr_flag(tag_inner: &str, key: &str) -> bool {
    let lowered = tag_inner.to_ascii_lowercase();
    let target = key.to_ascii_lowercase();
    for token in lowered.split_whitespace() {
        if token == target
            || token.starts_with(&format!("{target}="))
            || token.starts_with(&format!("{target}>"))
        {
            return true;
        }
    }
    false
}

fn build_safe_external_script_tag(tag_inner: &str, src: &str) -> String {
    let mut out = String::new();
    out.push_str("<script src=\"");
    out.push_str(&escape_html(src));
    out.push('"');

    if let Some(script_type) = extract_attr_value(tag_inner, "type") {
        let lowered = script_type.trim().to_ascii_lowercase();
        if lowered == "module"
            || lowered == "text/javascript"
            || lowered == "application/javascript"
        {
            out.push_str(" type=\"");
            out.push_str(&escape_html(script_type.trim()));
            out.push('"');
        }
    }
    if has_attr_flag(tag_inner, "defer") {
        out.push_str(" defer");
    }
    if has_attr_flag(tag_inner, "async") {
        out.push_str(" async");
    }
    if has_attr_flag(tag_inner, "nomodule") {
        out.push_str(" nomodule");
    }
    if let Some(integrity) = extract_attr_value(tag_inner, "integrity") {
        out.push_str(" integrity=\"");
        out.push_str(&escape_html(integrity.trim()));
        out.push('"');
    }
    if let Some(crossorigin) = extract_attr_value(tag_inner, "crossorigin") {
        out.push_str(" crossorigin=\"");
        out.push_str(&escape_html(crossorigin.trim()));
        out.push('"');
    }
    if let Some(referrerpolicy) = extract_attr_value(tag_inner, "referrerpolicy") {
        out.push_str(" referrerpolicy=\"");
        out.push_str(&escape_html(referrerpolicy.trim()));
        out.push('"');
    }

    out.push_str("></script>");
    out
}

/// Apply strict script wall:
/// - remove inline scripts,
/// - keep only scripts allowed by both `load_scripts` and allow-list rules,
/// - enforce integrity requirements for remote scripts.
fn apply_script_security_wall(
    html: &str,
    load_scripts: &[String],
    allow_list: &[AllowListRule],
    diagnostics: &mut Vec<Diagnostic>,
) -> String {
    let mut out = String::new();
    let mut cursor = 0usize;
    let lower = html.to_ascii_lowercase();

    let mut kept = 0usize;
    let mut blocked_inline = 0usize;
    let mut blocked_unlisted = 0usize;
    let mut blocked_integrity = 0usize;

    while let Some(rel_idx) = lower[cursor..].find("<script") {
        let open_start = cursor + rel_idx;
        out.push_str(&html[cursor..open_start]);
        let Some(open_end) = find_tag_end(html, open_start) else {
            out.push_str(&html[open_start..]);
            break;
        };
        let tag_inner = &html[open_start + 1..open_end];
        let content_start = open_end + 1;
        let Some(close_start) = find_close_tag(html, content_start, "script") else {
            blocked_inline += 1;
            cursor = open_end + 1;
            continue;
        };
        let close_end = close_start + "</script>".len();

        let src = extract_attr_value(tag_inner, "src")
            .map(|v| v.trim().to_string())
            .unwrap_or_default();
        let integrity = extract_attr_value(tag_inner, "integrity")
            .map(|v| v.trim().to_string())
            .filter(|v| !v.is_empty());

        if src.is_empty() {
            blocked_inline += 1;
            cursor = close_end;
            continue;
        }

        let in_load_scripts =
            is_script_allowed_by_load_scripts(&src, integrity.as_deref(), load_scripts);
        let in_allow_list = is_script_allowed_by_allow_list(&src, allow_list);
        if !in_allow_list {
            blocked_unlisted += 1;
            cursor = close_end;
            continue;
        }
        if !in_load_scripts {
            blocked_integrity += 1;
            cursor = close_end;
            continue;
        }

        out.push_str(&build_safe_external_script_tag(tag_inner, &src));
        kept += 1;
        cursor = close_end;
    }

    out.push_str(&html[cursor..]);

    diagnostics.push(Diagnostic {
        level: DiagnosticLevel::Info,
        code: "security_script_wall".to_string(),
        message: format!(
            "security script wall: kept={kept}, blocked_inline={blocked_inline}, blocked_unlisted={blocked_unlisted}, blocked_integrity={blocked_integrity}"
        ),
    });

    out
}

/// Apply stylesheet allow-list filtering for `<link rel="stylesheet" href="...">`.
fn apply_css_security_wall(
    html: &str,
    allow_list: &[AllowListRule],
    diagnostics: &mut Vec<Diagnostic>,
) -> String {
    let mut out = String::new();
    let mut cursor = 0usize;
    let lower = html.to_ascii_lowercase();
    let mut blocked_stylesheets = 0usize;

    while let Some(rel_idx) = lower[cursor..].find("<link") {
        let open_start = cursor + rel_idx;
        out.push_str(&html[cursor..open_start]);
        let Some(open_end) = find_tag_end(html, open_start) else {
            out.push_str(&html[open_start..]);
            break;
        };

        let tag_inner = &html[open_start + 1..open_end];
        let rel = extract_attr_value(tag_inner, "rel")
            .unwrap_or_default()
            .to_ascii_lowercase();
        let href = extract_attr_value(tag_inner, "href").unwrap_or_default();

        let is_stylesheet = rel
            .split_whitespace()
            .any(|token| token.eq_ignore_ascii_case("stylesheet"));
        if is_stylesheet {
            if href.trim().is_empty() || !is_css_allowed(href.trim(), allow_list) {
                blocked_stylesheets += 1;
                cursor = open_end + 1;
                continue;
            }
        }

        out.push_str(&html[open_start..open_end + 1]);
        cursor = open_end + 1;
    }

    out.push_str(&html[cursor..]);
    diagnostics.push(Diagnostic {
        level: DiagnosticLevel::Info,
        code: "security_css_wall".to_string(),
        message: format!("security css wall: blocked_stylesheets={blocked_stylesheets}"),
    });
    out
}

#[derive(Debug, Clone)]
struct ParsedHtmlAttr {
    key: String,
    value: Option<String>,
}

#[derive(Debug, Clone)]
struct ParsedOpenTag {
    name: String,
    attrs: Vec<ParsedHtmlAttr>,
    self_closing: bool,
}

fn parse_open_tag(inner: &str) -> Option<ParsedOpenTag> {
    let s = inner.trim();
    if s.is_empty() || s.starts_with('/') || s.starts_with('!') || s.starts_with('?') {
        return None;
    }
    let bytes = s.as_bytes();
    let mut i = 0usize;
    while i < bytes.len() && bytes[i].is_ascii_whitespace() {
        i += 1;
    }
    let name_start = i;
    while i < bytes.len() && !bytes[i].is_ascii_whitespace() && bytes[i] != b'/' {
        i += 1;
    }
    if i <= name_start {
        return None;
    }

    let name = s[name_start..i].to_ascii_lowercase();
    let mut attrs = Vec::<ParsedHtmlAttr>::new();
    let mut self_closing = s.ends_with('/');

    while i < bytes.len() {
        while i < bytes.len() && bytes[i].is_ascii_whitespace() {
            i += 1;
        }
        if i >= bytes.len() {
            break;
        }
        if bytes[i] == b'/' {
            self_closing = true;
            break;
        }

        let key_start = i;
        while i < bytes.len()
            && !bytes[i].is_ascii_whitespace()
            && bytes[i] != b'='
            && bytes[i] != b'/'
        {
            i += 1;
        }
        if i <= key_start {
            break;
        }
        let key = s[key_start..i].to_string();

        while i < bytes.len() && bytes[i].is_ascii_whitespace() {
            i += 1;
        }

        let mut value = None;
        if i < bytes.len() && bytes[i] == b'=' {
            i += 1;
            while i < bytes.len() && bytes[i].is_ascii_whitespace() {
                i += 1;
            }
            if i < bytes.len() && (bytes[i] == b'"' || bytes[i] == b'\'') {
                let quote = bytes[i];
                i += 1;
                let val_start = i;
                while i < bytes.len() && bytes[i] != quote {
                    i += 1;
                }
                value = Some(s[val_start..i].to_string());
                if i < bytes.len() {
                    i += 1;
                }
            } else {
                let val_start = i;
                while i < bytes.len() && !bytes[i].is_ascii_whitespace() && bytes[i] != b'/' {
                    i += 1;
                }
                value = Some(s[val_start..i].to_string());
            }
        }

        attrs.push(ParsedHtmlAttr { key, value });
    }

    Some(ParsedOpenTag {
        name,
        attrs,
        self_closing,
    })
}

fn rebuild_open_tag(tag: &ParsedOpenTag) -> String {
    let mut out = String::new();
    out.push('<');
    out.push_str(&tag.name);
    for attr in &tag.attrs {
        let key = attr.key.trim();
        if key.is_empty() {
            continue;
        }
        out.push(' ');
        out.push_str(key);
        if let Some(value) = &attr.value {
            out.push_str("=\"");
            out.push_str(&escape_html(value));
            out.push('"');
        }
    }
    if tag.self_closing {
        out.push_str(" />");
    } else {
        out.push('>');
    }
    out
}

fn is_url_attr(tag_name: &str, attr_name: &str) -> bool {
    match attr_name {
        "href" => matches!(tag_name, "a" | "area" | "link"),
        "src" => matches!(
            tag_name,
            "img" | "iframe" | "frame" | "audio" | "video" | "source" | "track" | "input" | "embed"
        ),
        "action" => tag_name == "form",
        "formaction" => matches!(tag_name, "button" | "input"),
        "poster" => tag_name == "video",
        "data" => tag_name == "object",
        _ => false,
    }
}

/// Apply URL attribute and inline handler wall on raw HTML.
///
/// Removes:
/// - disallowed URL attrs (`href/src/action/formaction/poster/data`)
/// - inline `on*` event handler attributes.
fn apply_url_attribute_security_wall(
    html: &str,
    allow_list: &[AllowListRule],
    diagnostics: &mut Vec<Diagnostic>,
) -> String {
    let mut out = String::new();
    let mut cursor = 0usize;
    let lower = html.to_ascii_lowercase();

    let mut blocked_url_attrs = 0usize;
    let mut blocked_inline_handlers = 0usize;

    while let Some(rel_idx) = lower[cursor..].find('<') {
        let open_start = cursor + rel_idx;
        out.push_str(&html[cursor..open_start]);
        let Some(open_end) = find_tag_end(html, open_start) else {
            out.push_str(&html[open_start..]);
            break;
        };

        let inner = &html[open_start + 1..open_end];
        if let Some(mut parsed) = parse_open_tag(inner) {
            if parsed.name == "script" || parsed.name == "style" {
                out.push_str(&html[open_start..open_end + 1]);
                cursor = open_end + 1;
                continue;
            }

            let tag_name = parsed.name.clone();
            let mut changed = false;
            parsed.attrs.retain(|attr| {
                let key = attr.key.to_ascii_lowercase();
                if key.starts_with("on") {
                    blocked_inline_handlers += 1;
                    changed = true;
                    return false;
                }
                if is_url_attr(&tag_name, &key) {
                    let value = attr.value.as_deref().unwrap_or_default().trim();
                    if !value.is_empty() && !is_url_allowed(value, allow_list) {
                        blocked_url_attrs += 1;
                        changed = true;
                        return false;
                    }
                }
                true
            });

            if changed {
                out.push_str(&rebuild_open_tag(&parsed));
            } else {
                out.push_str(&html[open_start..open_end + 1]);
            }
        } else {
            out.push_str(&html[open_start..open_end + 1]);
        }

        cursor = open_end + 1;
    }

    out.push_str(&html[cursor..]);
    diagnostics.push(Diagnostic {
        level: DiagnosticLevel::Info,
        code: "security_url_wall".to_string(),
        message: format!(
            "security url wall: blocked_url_attrs={blocked_url_attrs}, blocked_inline_handlers={blocked_inline_handlers}"
        ),
    });
    out
}

fn reactive_binding_kind(kind: &str) -> String {
    kind.to_string()
}

fn parsed_attr_value<'a>(attrs: &'a [ParsedHtmlAttr], key: &str) -> Option<&'a str> {
    for attr in attrs {
        if attr.key.eq_ignore_ascii_case(key) {
            if let Some(value) = attr.value.as_deref() {
                return Some(value);
            }
        }
    }
    None
}

fn parsed_attr_value_owned(attrs: &[ParsedHtmlAttr], key: &str) -> Option<String> {
    parsed_attr_value(attrs, key).map(|v| v.to_string())
}

fn parsed_attr_set(attrs: &mut Vec<ParsedHtmlAttr>, key: &str, value: String) {
    for attr in attrs.iter_mut() {
        if attr.key.eq_ignore_ascii_case(key) {
            attr.value = Some(value);
            return;
        }
    }
    attrs.push(ParsedHtmlAttr {
        key: key.to_string(),
        value: Some(value),
    });
}

fn ensure_reactive_jid(attrs: &mut Vec<ParsedHtmlAttr>, jid_counter: &mut usize) -> String {
    if let Some(existing) = parsed_attr_value(attrs, "data-jid") {
        return existing.to_string();
    }
    let new_jid = format!("j{}", *jid_counter);
    *jid_counter += 1;
    parsed_attr_set(attrs, "data-jid", new_jid.clone());
    new_jid
}

fn parse_close_tag_name(inner: &str) -> Option<String> {
    let trimmed = inner.trim_start();
    let rest = trimmed.strip_prefix('/')?;
    let mut name = String::new();
    for ch in rest.chars() {
        if ch.is_ascii_alphanumeric() || ch == '-' || ch == '_' {
            name.push(ch.to_ascii_lowercase());
        } else {
            break;
        }
    }
    if name.is_empty() { None } else { Some(name) }
}

/// Extract compile-time reactive bindings and inject stable `data-jid` markers.
///
/// Strategy:
/// - scan HTML tags once,
/// - assign `data-jid` to elements using static reactive attrs (`j-text`, `j-model`, `j-attr:*`),
/// - register `data-josie-map` roots with string `source` paths,
/// - skip nested extraction inside map roots to avoid duplicated local-scope bindings,
/// - use wildcard (`*`) map subscription when static source path cannot be derived.
///
/// Notes:
/// - map key expressions are runtime concerns (`data-josie-map.key`) and are not
///   represented as dependency paths here.
fn extract_reactive_bindings_and_annotate_html(
    html: &str,
    diagnostics: &mut Vec<Diagnostic>,
) -> (String, Vec<ReactiveBinding>) {
    let mut out = String::new();
    let mut cursor = 0usize;
    let mut bindings = Vec::<ReactiveBinding>::new();
    let mut jid_counter = 0usize;
    let mut stack: Vec<bool> = Vec::new();

    while let Some(rel_idx) = html[cursor..].find('<') {
        let open_start = cursor + rel_idx;
        out.push_str(&html[cursor..open_start]);
        let Some(open_end) = find_tag_end(html, open_start) else {
            out.push_str(&html[open_start..]);
            return (out, bindings);
        };

        let inner = &html[open_start + 1..open_end];
        let trimmed = inner.trim_start();

        if parse_close_tag_name(inner).is_some() {
            if !stack.is_empty() {
                stack.pop();
            }
            out.push_str(&html[open_start..open_end + 1]);
            cursor = open_end + 1;
            continue;
        }

        let Some(mut parsed) = parse_open_tag(inner) else {
            out.push_str(&html[open_start..open_end + 1]);
            cursor = open_end + 1;
            continue;
        };

        if parsed.name == "script" || parsed.name == "style" {
            out.push_str(&html[open_start..open_end + 1]);
            cursor = open_end + 1;
            if !parsed.self_closing {
                stack.push(false);
            }
            continue;
        }

        let inside_map_context = stack.iter().any(|v| *v);
        let map_raw = parsed_attr_value_owned(&parsed.attrs, "data-josie-map");
        let mut is_map_root = false;

        if !inside_map_context {
            if let Some(path) = parsed_attr_value_owned(&parsed.attrs, "j-text")
                .map(|v| v.trim().to_string())
                .filter(|v| !v.is_empty())
            {
                let jid = ensure_reactive_jid(&mut parsed.attrs, &mut jid_counter);
                bindings.push(ReactiveBinding {
                    jid,
                    kind: reactive_binding_kind("text"),
                    path,
                    attr: None,
                });
            }

            if let Some(path) = parsed_attr_value_owned(&parsed.attrs, "j-show")
                .map(|v| v.trim().to_string())
                .filter(|v| !v.is_empty())
            {
                let jid = ensure_reactive_jid(&mut parsed.attrs, &mut jid_counter);
                bindings.push(ReactiveBinding {
                    jid,
                    kind: reactive_binding_kind("show"),
                    path,
                    attr: None,
                });
            }

            if let Some(path) = parsed_attr_value_owned(&parsed.attrs, "j-model")
                .map(|v| v.trim().to_string())
                .filter(|v| !v.is_empty())
            {
                let jid = ensure_reactive_jid(&mut parsed.attrs, &mut jid_counter);
                bindings.push(ReactiveBinding {
                    jid,
                    kind: reactive_binding_kind("model"),
                    path,
                    attr: None,
                });
            }

            let attr_entries: Vec<(String, String)> = parsed
                .attrs
                .iter()
                .filter_map(|attr| {
                    let key = attr.key.trim();
                    if !key.to_ascii_lowercase().starts_with("j-attr:") {
                        return None;
                    }
                    let attr_name = key["j-attr:".len()..].trim();
                    if attr_name.is_empty() {
                        return None;
                    }
                    let path = attr.value.as_deref().unwrap_or_default().trim();
                    if path.is_empty() {
                        return None;
                    }
                    Some((attr_name.to_string(), path.to_string()))
                })
                .collect();
            for (attr_name, path) in attr_entries {
                let jid = ensure_reactive_jid(&mut parsed.attrs, &mut jid_counter);
                bindings.push(ReactiveBinding {
                    jid,
                    kind: reactive_binding_kind("attr"),
                    path,
                    attr: Some(attr_name),
                });
            }

            if let Some(raw_cfg) = map_raw.as_deref() {
                is_map_root = true;
                let cfg = serde_json::from_str::<Value>(raw_cfg);
                match cfg {
                    Ok(Value::Object(obj)) => {
                        if let Some(source) = obj.get("source").and_then(Value::as_str) {
                            let source = source.trim();
                            if !source.is_empty() {
                                let jid = ensure_reactive_jid(&mut parsed.attrs, &mut jid_counter);
                                bindings.push(ReactiveBinding {
                                    jid,
                                    kind: reactive_binding_kind("map"),
                                    path: source.to_string(),
                                    attr: None,
                                });
                            }
                        } else {
                            let jid = ensure_reactive_jid(&mut parsed.attrs, &mut jid_counter);
                            bindings.push(ReactiveBinding {
                                jid,
                                kind: reactive_binding_kind("map"),
                                path: "*".to_string(),
                                attr: None,
                            });
                            diagnostics.push(Diagnostic {
                                level: DiagnosticLevel::Warn,
                                code: "map_binding_missing_source".to_string(),
                                message: "data-josie-map has no string source; falling back to runtime map behavior".to_string(),
                            });
                        }
                    }
                    _ => {
                        let jid = ensure_reactive_jid(&mut parsed.attrs, &mut jid_counter);
                        bindings.push(ReactiveBinding {
                            jid,
                            kind: reactive_binding_kind("map"),
                            path: "*".to_string(),
                            attr: None,
                        });
                        diagnostics.push(Diagnostic {
                            level: DiagnosticLevel::Warn,
                            code: "map_binding_invalid_json".to_string(),
                            message: "data-josie-map is not valid JSON; falling back to runtime map behavior".to_string(),
                        })
                    }
                }
            }
        } else if map_raw.is_some() {
            is_map_root = true;
        }

        out.push_str(&rebuild_open_tag(&parsed));
        if !parsed.self_closing && !trimmed.ends_with('/') {
            stack.push(inside_map_context || is_map_root);
        }
        cursor = open_end + 1;
    }

    out.push_str(&html[cursor..]);
    diagnostics.push(Diagnostic {
        level: DiagnosticLevel::Info,
        code: "reactive_bindings_extracted".to_string(),
        message: format!("reactive bindings extracted: {}", bindings.len()),
    });
    (out, bindings)
}

fn merged_allow_list(
    input_allow_list: &[AllowListRule],
    program: &Value,
    diagnostics: &mut Vec<Diagnostic>,
) -> Vec<AllowListRule> {
    let mut out = Vec::<AllowListRule>::new();
    let mut seen = HashSet::<(String, String)>::new();

    for rule in input_allow_list {
        let key = (
            rule.rule_type.to_ascii_lowercase(),
            rule.value.trim().to_string(),
        );
        if key.1.is_empty() {
            continue;
        }
        if seen.insert(key.clone()) {
            out.push(AllowListRule {
                rule_type: key.0,
                value: key.1,
            });
        }
    }

    if let Some(list) = program.get("allowList").and_then(Value::as_array) {
        for raw in list {
            let Some(obj) = raw.as_object() else {
                continue;
            };
            let Some(rule_type) = obj.get("type").and_then(Value::as_str) else {
                continue;
            };
            let Some(value) = obj.get("value").and_then(Value::as_str) else {
                continue;
            };
            let key = (rule_type.to_ascii_lowercase(), value.trim().to_string());
            if key.1.is_empty() {
                continue;
            }
            if seen.insert(key.clone()) {
                out.push(AllowListRule {
                    rule_type: key.0,
                    value: key.1,
                });
            }
        }
    } else if program.get("allowList").is_some() {
        diagnostics.push(Diagnostic {
            level: DiagnosticLevel::Warn,
            code: "invalid_allow_list".to_string(),
            message: "program.allowList must be an array of {type,value}".to_string(),
        });
    }

    out
}

fn extract_tag_name(tag_inner: &str) -> Option<String> {
    let trimmed = tag_inner.trim_start();
    if trimmed.starts_with('/') || trimmed.starts_with('!') || trimmed.starts_with('?') {
        return None;
    }
    let mut name = String::new();
    for ch in trimmed.chars() {
        if ch.is_ascii_alphanumeric() || ch == '-' || ch == '_' {
            name.push(ch.to_ascii_lowercase());
        } else {
            break;
        }
    }
    if name.is_empty() { None } else { Some(name) }
}

fn resolve_var_path<'a>(vars: &'a Map<String, Value>, path: &str) -> Option<&'a Value> {
    let mut parts = path.split('.');
    let first = parts.next()?;
    let mut current = vars.get(first)?;
    for segment in parts {
        match current {
            Value::Object(obj) => {
                current = obj.get(segment)?;
            }
            Value::Array(arr) => {
                let idx = segment.parse::<usize>().ok()?;
                current = arr.get(idx)?;
            }
            _ => return None,
        }
    }
    Some(current)
}

fn value_to_plain_string(value: &Value) -> String {
    match value {
        Value::Null => String::new(),
        Value::String(v) => v.clone(),
        Value::Number(v) => v.to_string(),
        Value::Bool(v) => v.to_string(),
        _ => value.to_string(),
    }
}

fn apply_markdown_bindings_from_vars(
    html: &str,
    vars: &Map<String, Value>,
    allow_list: &[AllowListRule],
) -> String {
    let mut out = String::new();
    let mut cursor = 0usize;
    let lower = html.to_ascii_lowercase();

    while let Some(rel_lt) = lower[cursor..].find('<') {
        let open_start = cursor + rel_lt;
        let Some(open_end) = find_tag_end(html, open_start) else {
            out.push_str(&html[cursor..]);
            return out;
        };
        let inner = &html[open_start + 1..open_end];
        let Some(md_path) = extract_attr_value(inner, "j-md-var") else {
            out.push_str(&html[cursor..open_end + 1]);
            cursor = open_end + 1;
            continue;
        };
        let Some(tag_name) = extract_tag_name(inner) else {
            out.push_str(&html[cursor..open_end + 1]);
            cursor = open_end + 1;
            continue;
        };
        let content_start = open_end + 1;
        let Some(close_start) = find_close_tag(html, content_start, &tag_name) else {
            out.push_str(&html[cursor..open_end + 1]);
            cursor = open_end + 1;
            continue;
        };
        let close_end = close_start + format!("</{tag_name}>").len();

        out.push_str(&html[cursor..content_start]);
        if let Some(value) = resolve_var_path(vars, &md_path) {
            let markdown_raw = value_to_plain_string(value);
            out.push_str(&render_markdown_basic(&markdown_raw, allow_list));
        } else {
            out.push_str(&html[content_start..close_start]);
        }
        out.push_str(&html[close_start..close_end]);
        cursor = close_end;
    }

    out.push_str(&html[cursor..]);
    out
}

fn escape_html(input: &str) -> String {
    let mut out = String::with_capacity(input.len());
    for ch in input.chars() {
        match ch {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '"' => out.push_str("&quot;"),
            '\'' => out.push_str("&#39;"),
            _ => out.push(ch),
        }
    }
    out
}

fn apply_tailwind_processor(
    html: &str,
    program: &Value,
    diagnostics: &mut Vec<Diagnostic>,
) -> String {
    let html = strip_tailwind_cdn_scripts(html);
    let mut tokens = HashSet::new();
    collect_class_tokens_from_html(&html, &mut tokens);
    collect_class_tokens_from_program(program, &mut tokens);

    if tokens.is_empty() {
        diagnostics.push(Diagnostic {
            level: DiagnosticLevel::Info,
            code: "tailwind_no_tokens".to_string(),
            message: "tailwind processor enabled but no class tokens found".to_string(),
        });
        return html;
    }

    let mut sorted: Vec<String> = tokens.into_iter().collect();
    sorted.sort_unstable();

    let mut css = String::new();
    css.push_str(TAILWIND_PREFLIGHT_CSS);
    let mut unsupported = 0usize;

    for token in &sorted {
        let emitted = if let Some(rule) = token_css_rule(token) {
            css.push('\n');
            css.push_str(&rule);
            true
        } else {
            false
        };
        if !emitted {
            unsupported += 1;
        }
    }

    if unsupported > 0 {
        diagnostics.push(Diagnostic {
            level: DiagnosticLevel::Warn,
            code: "tailwind_unsupported_tokens".to_string(),
            message: format!("tailwind skipped {unsupported} unsupported tokens"),
        });
    }

    let css = minify_css_light(&css);
    let css_bytes = css.len();
    let css_kb = (css_bytes + 1023) / 1024;
    let style_tag = format!(
        "<!-- {{!-- {}kb --}} --><style data-josie-tw>/* {}kb */{}</style>",
        css_kb, css_kb, css
    );
    inject_style_tag(&html, &style_tag)
}

fn strip_tailwind_cdn_scripts(html: &str) -> String {
    let mut out = String::new();
    let mut cursor = 0usize;
    let lower = html.to_ascii_lowercase();

    while let Some(rel_idx) = lower[cursor..].find("<script") {
        let open_start = cursor + rel_idx;
        out.push_str(&html[cursor..open_start]);

        let Some(open_end) = find_tag_end(html, open_start) else {
            out.push_str(&html[open_start..]);
            return out;
        };
        let inside = &html[open_start + 1..open_end];
        let src = extract_attr_value(inside, "src")
            .unwrap_or_default()
            .to_ascii_lowercase();

        let content_start = open_end + 1;
        let Some(close_start) = find_close_tag(html, content_start, "script") else {
            out.push_str(&html[open_start..]);
            return out;
        };
        let close_end = close_start + "</script>".len();

        if src.contains("tailwindcss.com") || src.contains("tailwindcss") {
            cursor = close_end;
            continue;
        }

        out.push_str(&html[open_start..close_end]);
        cursor = close_end;
    }

    out.push_str(&html[cursor..]);
    out
}

#[allow(dead_code)]
fn collect_class_tokens_from_html(html: &str, out: &mut HashSet<String>) {
    let mut cursor = 0usize;
    while let Some(rel_lt) = html[cursor..].find('<') {
        let lt = cursor + rel_lt;
        let Some(end) = find_tag_end(html, lt) else {
            break;
        };
        let rest = &html[lt..];
        if rest.starts_with("</") || rest.starts_with("<!") || rest.starts_with("<?") {
            cursor = end + 1;
            continue;
        }
        let inner = &html[lt + 1..end];
        if let Some(class_str) = extract_attr_value(inner, "class") {
            for token in class_str.split_whitespace() {
                if !token.is_empty() {
                    out.insert(token.to_string());
                }
            }
        }
        cursor = end + 1;
    }
}

fn collect_class_tokens_from_program(value: &Value, out: &mut HashSet<String>) {
    match value {
        Value::Object(obj) => {
            for (k, v) in obj {
                if (k.eq_ignore_ascii_case("class")
                    || k.eq_ignore_ascii_case("classname")
                    || k.to_ascii_lowercase().ends_with("class"))
                    && v.is_string()
                {
                    if let Some(class_str) = v.as_str() {
                        for token in class_str.split_whitespace() {
                            if !token.is_empty() {
                                out.insert(token.to_string());
                            }
                        }
                    }
                }
                collect_class_tokens_from_program(v, out);
            }
        }
        Value::Array(arr) => {
            for item in arr {
                collect_class_tokens_from_program(item, out);
            }
        }
        _ => {}
    }
}

fn inject_style_tag(html: &str, style_tag: &str) -> String {
    if html.contains("data-josie-tw") {
        return html.to_string();
    }
    if let Some(idx) = html.rfind("</head>") {
        let mut out = String::new();
        out.push_str(&html[..idx]);
        out.push_str(style_tag);
        out.push_str(&html[idx..]);
        return out;
    }
    if let Some(idx) = html.rfind("</body>") {
        let mut out = String::new();
        out.push_str(&html[..idx]);
        out.push_str(style_tag);
        out.push_str(&html[idx..]);
        return out;
    }
    let mut out = html.to_string();
    out.push_str(style_tag);
    out
}

fn has_processor(processors: &[String], name: &str) -> bool {
    processors.iter().any(|p| p == name)
}

fn runtime_asset_name(mode: &RuntimeMode) -> &'static str {
    match mode {
        RuntimeMode::Compiled => "josie-runtime.js",
        RuntimeMode::Interpreted => "josie-runtime-dev.js",
    }
}

fn runtime_asset_source(mode: &RuntimeMode) -> &'static str {
    match mode {
        RuntimeMode::Compiled => JOSIE_RUNTIME_JS,
        RuntimeMode::Interpreted => JOSIE_RUNTIME_DEV_JS,
    }
}

fn extract_client_state(program: &Value) -> Value {
    program
        .get("state")
        .and_then(Value::as_object)
        .and_then(|o| o.get("client").cloned())
        .unwrap_or_else(|| {
            program
                .get("state")
                .cloned()
                .unwrap_or_else(|| Value::Object(Map::new()))
        })
}

fn extract_server_state(program: &Value) -> Value {
    program
        .get("state")
        .and_then(Value::as_object)
        .and_then(|o| o.get("server").cloned())
        .unwrap_or_else(|| Value::Object(Map::new()))
}

/// Inject runtime bootstrap snippet into final export HTML.
///
/// Interpreted mode embeds program + state bootstrap directly.
///
/// Compiled mode keeps HTML bootstrap small:
/// - loads `josie-runtime.js`,
/// - loads `program.compiled.js` (state/security/bindings payload + compiled dispatch),
/// - then calls `JOSIE.init(...)`.
fn inject_runtime_bootstrap(
    html: &str,
    program: &Value,
    allow_list: &[AllowListRule],
    load_scripts: &[String],
    bindings: &[ReactiveBinding],
    runtime_mode: &RuntimeMode,
    runtime_vars: &Map<String, Value>,
) -> Result<String, String> {
    let state = extract_client_state(program);
    let mut server_state = extract_server_state(program);
    if !server_state.is_object() {
        server_state = Value::Object(Map::new());
    }
    if let Some(server_obj) = server_state.as_object_mut() {
        for (k, v) in runtime_vars {
            server_obj.insert(k.clone(), v.clone());
        }
    }
    let runtime_asset = runtime_asset_name(runtime_mode);
    let security = json!({
        "allowList": allow_list,
        "loadScripts": load_scripts
    });
    let bindings_json = json!(bindings);
    let extra_load_script_tags = build_missing_load_script_tags(html, load_scripts);
    let snippet = match runtime_mode {
        RuntimeMode::Interpreted => {
            let bootstrap = json!({
                "program": program,
                "state": state,
                "serverState": server_state,
                "security": security,
                "bindings": bindings_json
            });
            let bootstrap_text = serde_json::to_string(&bootstrap)
                .map_err(|e| format!("failed to encode runtime bootstrap: {e}"))?;
            format!(
                "{}<script>window.__JOSIE_BOOTSTRAP__={};</script><script src=\"./{}\"></script>",
                extra_load_script_tags, bootstrap_text, runtime_asset
            )
        }
        RuntimeMode::Compiled => format!(
            "{}<script>window.__JOSIE_AUTO_INIT__=false;</script><script src=\"./{}\"></script><script src=\"./program.compiled.js\"></script><script>window.__JOSIE_SERVER_STATE__={};window.JOSIE&&window.JOSIE.init({{state:window.__JOSIE_INITIAL_STATE__||{{}},serverState:window.__JOSIE_SERVER_STATE__||{{}},security:window.__JOSIE_SECURITY__||{{}}}});</script>",
            extra_load_script_tags,
            runtime_asset,
            serde_json::to_string(&server_state)
                .map_err(|e| format!("failed to encode runtime server state: {e}"))?
        ),
    };

    if let Some(idx) = html.rfind("</body>") {
        let mut out = String::new();
        out.push_str(&html[..idx]);
        out.push_str(&snippet);
        out.push_str(&html[idx..]);
        Ok(out)
    } else {
        let mut out = html.to_string();
        out.push_str(&snippet);
        Ok(out)
    }
}

fn build_missing_load_script_tags(html: &str, load_scripts: &[String]) -> String {
    let mut out = String::new();
    let lowered = html.to_ascii_lowercase();
    let mut seen = HashSet::<String>::new();

    for raw in load_scripts {
        let parsed = parse_load_script_rule(raw);
        let src = parsed.src_rule.trim();
        if src.is_empty() || src.contains('*') {
            continue;
        }
        if !seen.insert(src.to_string()) {
            continue;
        }
        let needle_double = format!("src=\"{}\"", src.to_ascii_lowercase());
        let needle_single = format!("src='{}'", src.to_ascii_lowercase());
        if lowered.contains(&needle_double) || lowered.contains(&needle_single) {
            continue;
        }

        out.push_str("<script src=\"");
        out.push_str(&escape_html(src));
        out.push('"');
        if let Some(pin) = parsed.integrity_pin.as_deref() {
            out.push_str(" integrity=\"");
            out.push_str(&escape_html(pin));
            out.push('"');
            out.push_str(" crossorigin=\"anonymous\"");
        }
        out.push_str("></script>");
    }

    out
}

fn inline_runtime_assets(
    html: &str,
    runtime_asset: &str,
    runtime_js: &str,
    compiled_js: Option<&str>,
) -> String {
    let runtime_tag = format!("<script src=\"./{}\"></script>", runtime_asset);
    let runtime_inline = format!("<script>{}</script>", runtime_js);
    let mut out = html.replace(&runtime_tag, &runtime_inline);
    if let Some(compiled_js) = compiled_js {
        let compiled_tag = "<script src=\"./program.compiled.js\"></script>";
        let compiled_inline = format!("<script>{}</script>", compiled_js);
        out = out.replace(compiled_tag, &compiled_inline);
    }
    out
}

/// Compile program payload into executable JS bundles for compiled mode.
///
/// Generated JS includes:
/// - initial state snapshot,
/// - security snapshot,
/// - compile-time reactive bindings (`window.__JOSIE_BINDINGS__`),
/// - compiled step/action dispatch tables.
///
/// The compiled payload intentionally avoids shipping full interpreted program JSON.
fn compile_program_to_js(
    program: &Value,
    allow_list: &[AllowListRule],
    load_scripts: &[String],
    bindings: &[ReactiveBinding],
) -> Result<String, String> {
    let mut js = String::new();
    js.push_str("(function(window){'use strict';");
    js.push_str("const J=window.JOSIE;if(!J){return;}");
    let initial_state = extract_client_state(program);
    let initial_state_js = serde_json::to_string(&initial_state)
        .map_err(|e| format!("failed to encode initial compiled state: {e}"))?;
    js.push_str("window.__JOSIE_INITIAL_STATE__=");
    js.push_str(&initial_state_js);
    js.push(';');
    let server_state = extract_server_state(program);
    let server_state_js = serde_json::to_string(&server_state)
        .map_err(|e| format!("failed to encode server state: {e}"))?;
    js.push_str("window.__JOSIE_SERVER_STATE__=");
    js.push_str(&server_state_js);
    js.push(';');
    let security_js = serde_json::to_string(&json!({
        "allowList": allow_list,
        "loadScripts": load_scripts
    }))
    .map_err(|e| format!("failed to encode runtime security config: {e}"))?;
    js.push_str("window.__JOSIE_SECURITY__=");
    js.push_str(&security_js);
    js.push(';');
    let bindings_js = serde_json::to_string(bindings)
        .map_err(|e| format!("failed to encode runtime bindings: {e}"))?;
    js.push_str("window.__JOSIE_BINDINGS__=");
    js.push_str(&bindings_js);
    js.push(';');
    js.push_str("const steps={};");

    if let Some(steps) = program.get("steps").and_then(Value::as_array) {
        for step in steps {
            let Some(step_obj) = step.as_object() else {
                continue;
            };
            let Some(step_id) = step_obj.get("id").and_then(Value::as_str) else {
                continue;
            };
            if step_id.is_empty() {
                continue;
            }
            let step_id_js = serde_json::to_string(step_id)
                .map_err(|e| format!("failed to encode step id: {e}"))?;
            let body = compile_step_fn(step_obj)?;
            js.push_str(&format!("steps[{step_id_js}]={body};"));
        }
    }

    js.push_str("window.__JOSIE_COMPILED_STEPS__=Object.assign(window.__JOSIE_COMPILED_STEPS__||{},steps);");
    js.push_str("const actions={};");
    if let Some(actions) = program.get("actions").and_then(Value::as_object) {
        for (name, action) in actions {
            let name_js = serde_json::to_string(name)
                .map_err(|e| format!("failed to encode action name: {e}"))?;
            let body = compile_action_fn(action)?;
            js.push_str(&format!("actions[{name_js}]={body};"));
        }
    }
    js.push_str("window.__JOSIE_COMPILED_ACTIONS__=Object.assign(window.__JOSIE_COMPILED_ACTIONS__||{},actions);");
    js.push_str("if(typeof J.loadCompiled==='function'){J.loadCompiled();}");
    js.push_str("})(window);");
    Ok(js)
}

fn compile_step_fn(step: &Map<String, Value>) -> Result<String, String> {
    let mut out = String::from("function(scope){scope=scope||{locals:{},event:null};");
    if let Some(when) = step.get("when") {
        let when_js = compile_expr(when, "scope")?;
        out.push_str(&format!("if(!J.truthy({when_js})){{return null;}}"));
    }

    let op = step.get("op").and_then(Value::as_str).unwrap_or_default();
    match op {
        "set" => {
            if let Some(into) = step.get("into").and_then(Value::as_str) {
                let expr = step
                    .get("args")
                    .and_then(Value::as_array)
                    .and_then(|a| a.first())
                    .cloned()
                    .or_else(|| step.get("do").cloned());
                if let Some(expr) = expr {
                    let into_js = js_encode_str(into)?;
                    let expr_js = compile_expr(&expr, "scope")?;
                    out.push_str(&format!("return J.set({into_js},{expr_js});"));
                    out.push('}');
                    return Ok(out);
                }
            }
            if let Some(args) = step.get("args").and_then(Value::as_array) {
                if args.len() >= 2 {
                    let path_js = compile_expr(&args[0], "scope")?;
                    let value_js = compile_expr(&args[1], "scope")?;
                    out.push_str(&format!("return J.set({path_js},{value_js});"));
                    out.push('}');
                    return Ok(out);
                }
            }
        }
        "call" => {
            if let Some(from) = step.get("from").and_then(Value::as_str) {
                let from_js = js_encode_str(from)?;
                let args = step
                    .get("args")
                    .and_then(Value::as_array)
                    .cloned()
                    .unwrap_or_default();
                let mut args_js = String::from("[");
                for (i, a) in args.iter().enumerate() {
                    if i > 0 {
                        args_js.push(',');
                    }
                    args_js.push_str(&compile_expr(a, "scope")?);
                }
                args_js.push(']');
                out.push_str(&format!("return J.call({from_js},{args_js},scope);"));
                out.push('}');
                return Ok(out);
            }
        }
        "return" => {
            if let Some(from) = step.get("from").and_then(Value::as_str) {
                let from_js = js_encode_str(from)?;
                out.push_str(&format!("return J.get({from_js},scope);"));
                out.push('}');
                return Ok(out);
            }
            if let Some(expr) = step
                .get("args")
                .and_then(Value::as_array)
                .and_then(|a| a.first())
            {
                let expr_js = compile_expr(expr, "scope")?;
                out.push_str(&format!("return {expr_js};"));
                out.push('}');
                return Ok(out);
            }
        }
        "map" | "filter" | "for_each" | "reduce" => {
            // Pipeline form: { "op": "map", "from": "client.items", "do": expr, "into": "client.result" }
            if let (Some(from), Some(do_expr)) = (
                step.get("from").and_then(Value::as_str),
                step.get("do"),
            ) {
                let list_node = Value::Array(vec![
                    Value::String("var".to_string()),
                    Value::String(from.to_string()),
                ]);
                let expr = Value::Array(vec![
                    Value::String(op.to_string()),
                    list_node,
                    do_expr.clone(),
                ]);
                let expr_js = compile_expr(&expr, "scope")?;
                if let Some(into) = step.get("into").and_then(Value::as_str) {
                    let into_js = js_encode_str(into)?;
                    out.push_str(&format!("return J.set({into_js},{expr_js});"));
                } else {
                    out.push_str(&format!("return {expr_js};"));
                }
                out.push('}');
                return Ok(out);
            }
            // Expression form (args array)
            let args = step
                .get("args")
                .and_then(Value::as_array)
                .cloned()
                .unwrap_or_default();
            let mut expr = vec![Value::String(op.to_string())];
            expr.extend(args);
            let expr_js = compile_expr(&Value::Array(expr), "scope")?;
            out.push_str(&format!("return {expr_js};"));
            out.push('}');
            return Ok(out);
        }
        "do" | "if" | "match" | "get" => {
            let args = step
                .get("args")
                .and_then(Value::as_array)
                .cloned()
                .unwrap_or_default();
            let mut expr = vec![Value::String(op.to_string())];
            expr.extend(args);
            let expr_js = compile_expr(&Value::Array(expr), "scope")?;
            out.push_str(&format!("return {expr_js};"));
            out.push('}');
            return Ok(out);
        }
        _ => {}
    }

    if let Some(expr) = step.get("do") {
        let expr_js = compile_expr(expr, "scope")?;
        out.push_str(&format!("return {expr_js};"));
        out.push('}');
        return Ok(out);
    }
    if let Some(expr) = step
        .get("args")
        .and_then(Value::as_array)
        .and_then(|a| a.first())
    {
        let expr_js = compile_expr(expr, "scope")?;
        out.push_str(&format!("return {expr_js};"));
        out.push('}');
        return Ok(out);
    }

    out.push_str("return null;");
    out.push('}');
    Ok(out)
}

fn compile_action_fn(action: &Value) -> Result<String, String> {
    let mut out = String::from(
        "function(event){const scope={locals:{},event:event||null};let out=null;",
    );

    if let Some(step_id) = action.as_str() {
        let step_id_js = js_encode_str(step_id)?;
        out.push_str(&format!("out=J.runStep({step_id_js},scope);"));
        out.push_str("return out;}");
        return Ok(out);
    }

    if action.is_array() {
        let expr_js = compile_expr(action, "scope")?;
        out.push_str(&format!("out={expr_js};"));
        out.push_str("return out;}");
        return Ok(out);
    }

    if let Some(obj) = action.as_object() {
        if let Some(step_id) = obj.get("runStep").and_then(Value::as_str) {
            let step_id_js = js_encode_str(step_id)?;
            out.push_str(&format!("out=J.runStep({step_id_js},scope);"));
            out.push_str("return out;}");
            return Ok(out);
        }

        if let Some(run) = obj.get("run").and_then(Value::as_array) {
            out.push_str("let last=null;");
            for entry in run {
                let Some(entry_obj) = entry.as_object() else {
                    continue;
                };
                if let Some(cond) = entry_obj.get("if") {
                    let cond_js = compile_action_condition(cond, "scope", "last")?;
                    out.push_str(&format!("if(!({cond_js})){{}}else{{"));
                }
                if let Some(step_name) = entry_obj.get("step").and_then(Value::as_str) {
                    let step_js = js_encode_str(step_name)?;
                    out.push_str(&format!("last=J.runStep({step_js},scope);"));
                } else if let Some(expr) = entry_obj.get("expr") {
                    let expr_js = compile_expr(expr, "scope")?;
                    out.push_str(&format!("last={expr_js};"));
                }
                if entry_obj.get("if").is_some() {
                    out.push('}');
                }
            }
            out.push_str("out=last;");
            out.push_str("return out;}");
            return Ok(out);
        }

        if let Some(expr) = obj.get("expr") {
            let expr_js = compile_expr(expr, "scope")?;
            out.push_str(&format!("out={expr_js};"));
            out.push_str("return out;}");
            return Ok(out);
        }
    }

    out.push_str("return out;}");
    Ok(out)
}

fn compile_action_condition(cond: &Value, scope_ident: &str, last_ident: &str) -> Result<String, String> {
    let scope_with_last = format!(
        "{{locals:Object.assign({{}},({scope}).locals||{{}},{{last:{last}}}),event:({scope}).event||null}}",
        scope = scope_ident,
        last = last_ident
    );
    match cond {
        Value::Null => Ok("true".to_string()),
        Value::Bool(b) => Ok(if *b { "true" } else { "false" }.to_string()),
        Value::String(s) => {
            if let Some(rest) = s.strip_prefix('!') {
                let rest_js = js_encode_str(rest)?;
                Ok(format!("!J.truthy(J.get({rest_js},{scope_with_last}))"))
            } else {
                let s_js = js_encode_str(s)?;
                Ok(format!("J.truthy(J.get({s_js},{scope_with_last}))"))
            }
        }
        _ => {
            let expr_js = compile_expr(cond, &scope_with_last)?;
            Ok(format!("J.truthy({expr_js})"))
        }
    }
}

fn compile_expr(node: &Value, scope_ident: &str) -> Result<String, String> {
    match node {
        Value::Null => Ok("null".to_string()),
        Value::Bool(b) => Ok(if *b { "true" } else { "false" }.to_string()),
        Value::Number(n) => Ok(n.to_string()),
        Value::String(s) => js_encode_str(s),
        Value::Object(obj) => compile_object_literal(obj, scope_ident),
        Value::Array(arr) => {
            if arr.is_empty() {
                return Ok("[]".to_string());
            }
            let Some(op) = arr.first().and_then(Value::as_str) else {
                return compile_array_literal(arr, scope_ident);
            };
            let args = &arr[1..];
            match op {
                "var" => {
                    if args.is_empty() {
                        return Ok("null".to_string());
                    }
                    let path_js = compile_expr(&args[0], scope_ident)?;
                    Ok(format!("J.get({path_js},{scope_ident})"))
                }
                "set" => {
                    if args.len() < 2 {
                        return Ok("null".to_string());
                    }
                    let path_js = compile_expr(&args[0], scope_ident)?;
                    let value_js = compile_expr(&args[1], scope_ident)?;
                    Ok(format!("J.set({path_js},{value_js})"))
                }
                "+" => {
                    if args.is_empty() {
                        return Ok("0".to_string());
                    }
                    let mut out = String::from("(");
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            out.push('+');
                        }
                        out.push_str("Number(");
                        out.push_str(&compile_expr(arg, scope_ident)?);
                        out.push_str("||0)");
                    }
                    out.push(')');
                    Ok(out)
                }
                "-" => {
                    if args.is_empty() {
                        return Ok("0".to_string());
                    }
                    if args.len() == 1 {
                        let one = compile_expr(&args[0], scope_ident)?;
                        return Ok(format!("(-Number({one}||0))"));
                    }
                    let a = compile_expr(&args[0], scope_ident)?;
                    let b = compile_expr(&args[1], scope_ident)?;
                    Ok(format!("(Number({a}||0)-Number({b}||0))"))
                }
                "*" => {
                    if args.is_empty() {
                        return Ok("1".to_string());
                    }
                    let mut out = String::from("(");
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            out.push('*');
                        }
                        out.push_str("Number(");
                        out.push_str(&compile_expr(arg, scope_ident)?);
                        out.push_str("||0)");
                    }
                    out.push(')');
                    Ok(out)
                }
                "/" => {
                    if args.len() < 2 {
                        return Ok("0".to_string());
                    }
                    let a = compile_expr(&args[0], scope_ident)?;
                    let b = compile_expr(&args[1], scope_ident)?;
                    Ok(format!("(function(){{const __a=Number({a}||0);const __b=Number({b}||0);return __b===0?0:(__a/__b);}})()"))
                }
                "%" => {
                    if args.len() < 2 {
                        return Ok("0".to_string());
                    }
                    let a = compile_expr(&args[0], scope_ident)?;
                    let b = compile_expr(&args[1], scope_ident)?;
                    Ok(format!("(function(){{const __a=Number({a}||0);const __b=Number({b}||0);return __b===0?0:(__a%__b);}})()"))
                }
                "==" | "!=" | ">" | "<" | ">=" | "<=" => {
                    if args.len() < 2 {
                        return Ok("false".to_string());
                    }
                    let a = compile_expr(&args[0], scope_ident)?;
                    let b = compile_expr(&args[1], scope_ident)?;
                    Ok(format!("({a}{op}{b})"))
                }
                "if" => {
                    if args.is_empty() {
                        return Ok("null".to_string());
                    }
                    let cond = compile_expr(&args[0], scope_ident)?;
                    let yes = if args.len() > 1 {
                        compile_expr(&args[1], scope_ident)?
                    } else {
                        "null".to_string()
                    };
                    let no = if args.len() > 2 {
                        compile_expr(&args[2], scope_ident)?
                    } else {
                        "null".to_string()
                    };
                    Ok(format!("(J.truthy({cond})?({yes}):({no}))"))
                }
                "&&" => {
                    let mut out = String::from("(function(){let __last=true;");
                    for (idx, arg) in args.iter().enumerate() {
                        let a = compile_expr(arg, scope_ident)?;
                        out.push_str(&format!(
                            "const __v_{idx}={a};if(!J.truthy(__v_{idx})){{return false;}}__last=__v_{idx};"
                        ));
                    }
                    out.push_str("return __last;})()");
                    Ok(out)
                }
                "||" => {
                    let mut out = String::from("(function(){");
                    for (idx, arg) in args.iter().enumerate() {
                        let a = compile_expr(arg, scope_ident)?;
                        out.push_str(&format!(
                            "const __v_{idx}={a};if(J.truthy(__v_{idx})){{return __v_{idx};}}"
                        ));
                    }
                    out.push_str("return false;})()");
                    Ok(out)
                }
                "!" => {
                    let a = if args.is_empty() {
                        "null".to_string()
                    } else {
                        compile_expr(&args[0], scope_ident)?
                    };
                    Ok(format!("(!J.truthy({a}))"))
                }
                "do" => {
                    let mut out = String::from("(function(){let __r=null;");
                    for arg in args {
                        let a = compile_expr(arg, scope_ident)?;
                        out.push_str(&format!("__r={a};"));
                    }
                    out.push_str("return __r;})()");
                    Ok(out)
                }
                "len" => {
                    if args.is_empty() {
                        return Ok("0".to_string());
                    }
                    let a = compile_expr(&args[0], scope_ident)?;
                    Ok(format!("(function(){{const __v={a};if(Array.isArray(__v)||typeof __v==='string'){{return __v.length;}}if(__v&&typeof __v==='object'){{return Object.keys(__v).length;}}return 0;}})()"))
                }
                "push" => {
                    if args.len() < 2 {
                        return Ok("[]".to_string());
                    }
                    let list = compile_expr(&args[0], scope_ident)?;
                    let item = compile_expr(&args[1], scope_ident)?;
                    Ok(format!("(function(){{const __l={list};const __i={item};return Array.isArray(__l)?__l.concat([__i]):[__i];}})()"))
                }
                "get" => {
                    if args.len() < 2 {
                        return Ok("null".to_string());
                    }
                    let base = compile_expr(&args[0], scope_ident)?;
                    let key = compile_expr(&args[1], scope_ident)?;
                    Ok(format!("J.getFrom({base},{key})"))
                }
                "map" => {
                    if args.len() < 2 {
                        return Ok("[]".to_string());
                    }
                    let list = compile_expr(&args[0], scope_ident)?;
                    let item_expr = compile_expr(&args[1], "__s")?;
                    Ok(format!("(function(){{const __list={list};if(!Array.isArray(__list)){{return [];}}const __out=[];for(let __i=0;__i<__list.length;__i++){{const __item=__list[__i];const __s={{locals:Object.assign({{}},({scope}).locals||{{}},{{item:__item,index:__i}}),event:({scope}).event||null}};__out.push({item_expr});}}return __out;}})()", scope = scope_ident))
                }
                "filter" => {
                    if args.len() < 2 {
                        return Ok("[]".to_string());
                    }
                    let list = compile_expr(&args[0], scope_ident)?;
                    let keep_expr = compile_expr(&args[1], "__s")?;
                    Ok(format!("(function(){{const __list={list};if(!Array.isArray(__list)){{return [];}}const __out=[];for(let __i=0;__i<__list.length;__i++){{const __item=__list[__i];const __s={{locals:Object.assign({{}},({scope}).locals||{{}},{{item:__item,index:__i}}),event:({scope}).event||null}};if(J.truthy({keep_expr})){{__out.push(__item);}}}}return __out;}})()", scope = scope_ident))
                }
                "for_each" => {
                    if args.len() < 2 {
                        return Ok("null".to_string());
                    }
                    let list = compile_expr(&args[0], scope_ident)?;
                    let body_expr = compile_expr(&args[1], "__s")?;
                    Ok(format!("(function(){{const __list={list};if(!Array.isArray(__list)){{return null;}}let __last=null;for(let __i=0;__i<__list.length;__i++){{const __item=__list[__i];const __s={{locals:Object.assign({{}},({scope}).locals||{{}},{{item:__item,index:__i}}),event:({scope}).event||null}};__last={body_expr};}}return __last;}})()", scope = scope_ident))
                }
                "reduce" => {
                    if args.len() < 2 {
                        return Ok("null".to_string());
                    }
                    let list = compile_expr(&args[0], scope_ident)?;
                    let body_expr = compile_expr(&args[1], "__s")?;
                    let init_expr = if args.len() > 2 {
                        compile_expr(&args[2], scope_ident)?
                    } else {
                        "null".to_string()
                    };
                    Ok(format!("(function(){{const __list={list};if(!Array.isArray(__list)){{return null;}}let __acc={init_expr};for(let __i=0;__i<__list.length;__i++){{const __item=__list[__i];const __s={{locals:Object.assign({{}},({scope}).locals||{{}},{{item:__item,index:__i,acc:__acc}}),event:({scope}).event||null}};__acc={body_expr};}}return __acc;}})()", scope = scope_ident))
                }
                "match" => {
                    if args.is_empty() {
                        return Ok("null".to_string());
                    }
                    let target = compile_expr(&args[0], scope_ident)?;
                    let mut out = format!("(function(){{const __m={target};");
                    let mut i = 1usize;
                    while i + 1 < args.len() {
                        let pat = &args[i];
                        let result = &args[i + 1];
                        if pat.as_str() == Some("_") {
                            let res_js = compile_expr(result, scope_ident)?;
                            out.push_str(&format!("return {res_js};"));
                            out.push_str("})()");
                            return Ok(out);
                        }
                        let pat_js = compile_expr(pat, scope_ident)?;
                        let res_js = compile_expr(result, scope_ident)?;
                        out.push_str(&format!("if({pat_js}===__m){{return {res_js};}}"));
                        i += 2;
                    }
                    out.push_str("return null;})()");
                    Ok(out)
                }
                "w.event.value" => Ok(format!(
                    "(({s}.event&&('value' in {s}.event))?{s}.event.value:null)",
                    s = scope_ident
                )),
                "w.event.key" => Ok(format!(
                    "(({s}.event&&('key' in {s}.event))?{s}.event.key:null)",
                    s = scope_ident
                )),
                "w.event.prevent" => Ok(format!("(function(){{const __raw={s}.event&&{s}.event.raw?{s}.event.raw:null;if(__raw&&typeof __raw.preventDefault==='function'){{__raw.preventDefault();return true;}}return false;}})()", s = scope_ident)),
                "call" => {
                    if args.is_empty() {
                        return Ok("null".to_string());
                    }
                    let name = compile_expr(&args[0], scope_ident)?;
                    let mut arg_list = String::from("[");
                    for (i, arg) in args.iter().enumerate().skip(1) {
                        if i > 1 {
                            arg_list.push(',');
                        }
                        arg_list.push_str(&compile_expr(arg, scope_ident)?);
                    }
                    arg_list.push(']');
                    Ok(format!("J.call({name},{arg_list},{scope_ident})"))
                }
                "util.concat" => {
                    let mut arg_list = String::from("[");
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            arg_list.push(',');
                        }
                        arg_list.push_str(&compile_expr(arg, scope_ident)?);
                    }
                    arg_list.push(']');
                    Ok(format!("J.util.concat({arg_list})"))
                }
                "util.lower" => Ok(format!("J.util.lower({})", compile_expr(args.first().unwrap_or(&Value::Null), scope_ident)?)),
                "util.upper" => Ok(format!("J.util.upper({})", compile_expr(args.first().unwrap_or(&Value::Null), scope_ident)?)),
                "util.contains" => {
                    let a = compile_expr(args.first().unwrap_or(&Value::Null), scope_ident)?;
                    let b = compile_expr(args.get(1).unwrap_or(&Value::Null), scope_ident)?;
                    Ok(format!("J.util.contains({a},{b})"))
                }
                "util.template" => {
                    let mut arg_list = String::from("[");
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            arg_list.push(',');
                        }
                        arg_list.push_str(&compile_expr(arg, scope_ident)?);
                    }
                    arg_list.push(']');
                    Ok(format!("J.util.template({arg_list})"))
                }
                "util.to_int" => Ok(format!("J.util.toInt({})", compile_expr(args.first().unwrap_or(&Value::Null), scope_ident)?)),
                "util.to_float" => Ok(format!("J.util.toFloat({})", compile_expr(args.first().unwrap_or(&Value::Null), scope_ident)?)),
                "util.to_string" => Ok(format!("J.util.toString({})", compile_expr(args.first().unwrap_or(&Value::Null), scope_ident)?)),
                "util.json_parse" => Ok(format!("J.util.jsonParse({})", compile_expr(args.first().unwrap_or(&Value::Null), scope_ident)?)),
                "util.json_stringify" => {
                    let a = compile_expr(args.first().unwrap_or(&Value::Null), scope_ident)?;
                    let b = compile_expr(args.get(1).unwrap_or(&Value::Null), scope_ident)?;
                    Ok(format!("J.util.jsonStringify({a},{b})"))
                }
                "util.trim" => Ok(format!("J.util.trim({})", compile_expr(args.first().unwrap_or(&Value::Null), scope_ident)?)),
                "util.str_len" => Ok(format!("J.util.strLen({})", compile_expr(args.first().unwrap_or(&Value::Null), scope_ident)?)),
                "util.get_path" => {
                    let a = compile_expr(args.first().unwrap_or(&Value::Null), scope_ident)?;
                    let b = compile_expr(args.get(1).unwrap_or(&Value::Null), scope_ident)?;
                    Ok(format!("J.util.getPath({a},{b})"))
                }
                "util.as_array" => Ok(format!("J.util.asArray({})", compile_expr(args.first().unwrap_or(&Value::Null), scope_ident)?)),
                "util.to_bool" => Ok(format!("J.util.toBool({})", compile_expr(args.first().unwrap_or(&Value::Null), scope_ident)?)),
                "util.replace" => {
                    let a = compile_expr(args.first().unwrap_or(&Value::Null), scope_ident)?;
                    let b = compile_expr(args.get(1).unwrap_or(&Value::Null), scope_ident)?;
                    let c = compile_expr(args.get(2).unwrap_or(&Value::Null), scope_ident)?;
                    Ok(format!("J.util.replace({a},{b},{c})"))
                }
                "util.replace_all" => {
                    let a = compile_expr(args.first().unwrap_or(&Value::Null), scope_ident)?;
                    let b = compile_expr(args.get(1).unwrap_or(&Value::Null), scope_ident)?;
                    let c = compile_expr(args.get(2).unwrap_or(&Value::Null), scope_ident)?;
                    Ok(format!("J.util.replaceAll({a},{b},{c})"))
                }
                "util.split" => {
                    let a = compile_expr(args.first().unwrap_or(&Value::Null), scope_ident)?;
                    let b = compile_expr(args.get(1).unwrap_or(&Value::Null), scope_ident)?;
                    Ok(format!("J.util.split({a},{b})"))
                }
                "util.join" => {
                    let a = compile_expr(args.first().unwrap_or(&Value::Null), scope_ident)?;
                    let b = compile_expr(args.get(1).unwrap_or(&Value::Null), scope_ident)?;
                    Ok(format!("J.util.join({a},{b})"))
                }
                "util.regex_match" => {
                    let a = compile_expr(args.first().unwrap_or(&Value::Null), scope_ident)?;
                    let b = compile_expr(args.get(1).unwrap_or(&Value::Null), scope_ident)?;
                    let c = compile_expr(args.get(2).unwrap_or(&Value::Null), scope_ident)?;
                    Ok(format!("J.util.regexMatch({a},{b},{c})"))
                }
                "util.regex_find" => {
                    let a = compile_expr(args.first().unwrap_or(&Value::Null), scope_ident)?;
                    let b = compile_expr(args.get(1).unwrap_or(&Value::Null), scope_ident)?;
                    let c = compile_expr(args.get(2).unwrap_or(&Value::Null), scope_ident)?;
                    Ok(format!("J.util.regexFind({a},{b},{c})"))
                }
                "util.regex_find_all" => {
                    let a = compile_expr(args.first().unwrap_or(&Value::Null), scope_ident)?;
                    let b = compile_expr(args.get(1).unwrap_or(&Value::Null), scope_ident)?;
                    let c = compile_expr(args.get(2).unwrap_or(&Value::Null), scope_ident)?;
                    Ok(format!("J.util.regexFindAll({a},{b},{c})"))
                }
                "util.regex_replace" => {
                    let a = compile_expr(args.first().unwrap_or(&Value::Null), scope_ident)?;
                    let b = compile_expr(args.get(1).unwrap_or(&Value::Null), scope_ident)?;
                    let c = compile_expr(args.get(2).unwrap_or(&Value::Null), scope_ident)?;
                    let d = compile_expr(args.get(3).unwrap_or(&Value::Null), scope_ident)?;
                    Ok(format!("J.util.regexReplace({a},{b},{c},{d})"))
                }
                _ => {
                    if op.starts_with("x.") {
                        let op_js = js_encode_str(op)?;
                        let mut arg_list = String::from("[");
                        for (i, arg) in args.iter().enumerate() {
                            if i > 0 {
                                arg_list.push(',');
                            }
                            arg_list.push_str(&compile_expr(arg, scope_ident)?);
                        }
                        arg_list.push(']');
                        Ok(format!("J.callExternal({op_js},{arg_list},{scope_ident})"))
                    } else if op.starts_with("data.") {
                        // data.* ops compile to direct J.data.method(compiledArgs) calls.
                        // Each argument is compiled to native JS — no evalLite interpreter.
                        let method = &op["data.".len()..];
                        // Safety: only emit a direct call for known safe method names.
                        let safe = method.chars().all(|c| c.is_ascii_alphanumeric() || c == '_');
                        if safe && !method.starts_with('_') {
                            let mut arg_list = String::new();
                            for (i, arg) in args.iter().enumerate() {
                                if i > 0 { arg_list.push(','); }
                                arg_list.push_str(&compile_expr(arg, scope_ident)?);
                            }
                            Ok(format!("J.data.{method}({arg_list})"))
                        } else {
                            // Unknown / unsafe name — fall back to evalLite
                            let node_js = serde_json::to_string(&Value::Array(
                                std::iter::once(Value::String(op.to_string()))
                                    .chain(args.iter().cloned())
                                    .collect(),
                            ))
                            .map_err(|e| format!("failed to encode data.* expr: {e}"))?;
                            Ok(format!("J.evalLite({node_js},{scope_ident})"))
                        }
                    } else {
                        compile_array_literal(arr, scope_ident)
                    }
                }
            }
        }
    }
}

fn compile_object_literal(obj: &Map<String, Value>, scope_ident: &str) -> Result<String, String> {
    let mut out = String::from("{");
    for (i, (k, v)) in obj.iter().enumerate() {
        if i > 0 {
            out.push(',');
        }
        let key_js = js_encode_str(k)?;
        let value_js = compile_expr(v, scope_ident)?;
        out.push_str(&format!("{key_js}:{value_js}"));
    }
    out.push('}');
    Ok(out)
}

fn compile_array_literal(arr: &[Value], scope_ident: &str) -> Result<String, String> {
    let mut out = String::from("[");
    for (i, v) in arr.iter().enumerate() {
        if i > 0 {
            out.push(',');
        }
        out.push_str(&compile_expr(v, scope_ident)?);
    }
    out.push(']');
    Ok(out)
}

fn js_encode_str(input: &str) -> Result<String, String> {
    serde_json::to_string(input).map_err(|e| format!("failed to encode js string: {e}"))
}

fn minify_html_light(input: &str) -> String {
    let mut out = String::with_capacity(input.len());
    let mut in_tag = false;
    let mut in_quote: Option<char> = None;
    let mut pending_text_space = false;

    for ch in input.chars() {
        if in_tag {
            if let Some(q) = in_quote {
                out.push(ch);
                if ch == q {
                    in_quote = None;
                }
                continue;
            }

            match ch {
                '"' | '\'' => {
                    in_quote = Some(ch);
                    out.push(ch);
                }
                '>' => {
                    while out.ends_with(' ') {
                        out.pop();
                    }
                    out.push('>');
                    in_tag = false;
                }
                _ if ch.is_whitespace() => {
                    if !out.ends_with(' ') && !out.ends_with('<') {
                        out.push(' ');
                    }
                }
                _ => out.push(ch),
            }
            continue;
        }

        match ch {
            '<' => {
                while out.ends_with(' ') {
                    out.pop();
                }
                out.push('<');
                in_tag = true;
                pending_text_space = false;
            }
            _ if ch.is_whitespace() => {
                pending_text_space = true;
            }
            _ => {
                if pending_text_space {
                    if !out.ends_with('>') && !out.ends_with('<') {
                        out.push(' ');
                    }
                    pending_text_space = false;
                }
                out.push(ch);
            }
        }
    }

    rebalance_html_minified_lines(&out)
}

fn rebalance_html_minified_lines(input: &str) -> String {
    let mut out = input.replace("><", ">\n<");
    while out.contains("\n\n") {
        out = out.replace("\n\n", "\n");
    }
    out
}

fn minify_css_light(input: &str) -> String {
    input
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .collect::<Vec<_>>()
        .join("")
}

fn minify_js_light(input: &str) -> String {
    match jsminify::minify_js(input) {
        Ok(minified) => {
            if !minified.contains('\n') && !minified.contains('\r') {
                minified
            } else {
                minify_js_fallback(input)
            }
        }
        Err(_) => minify_js_fallback(input),
    }
}

fn minify_js_fallback(input: &str) -> String {
    input
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .filter(|line| !line.starts_with("//"))
        .filter(|line| !line.starts_with("/*"))
        .filter(|line| !line.starts_with('*'))
        .filter(|line| !line.starts_with("*/"))
        .collect::<Vec<_>>()
        .join("")
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::{SystemTime, UNIX_EPOCH};

    #[test]
    fn compile_and_render_stitches_components() {
        let mut engine = JosieWebEngine::new();
        let mut components = HashMap::new();
        components.insert(
            "card".to_string(),
            "<article><h2>{{title}}</h2><j-component name=\"badge\" /></article>".to_string(),
        );
        components.insert(
            "badge".to_string(),
            "<span class=\"badge\">OK</span>".to_string(),
        );

        let input = CompileInput {
            template: "<main><j-component name=\"card\" /></main><script type=\"application/josie+json\">{\"kind\":\"program\",\"version\":\"0.1.0\",\"steps\":[],\"actions\":{},\"resources\":[]}</script>".to_string(),
            components,
            processors: vec!["tailwind".to_string()],
            allow_list: vec![],
            load_scripts: vec![],
            runtime_mode: None,
            cache: None,
            cache_policy: Some(CachePolicy::Ram),
        };
        let compiled = engine.compile(input).expect("compile");
        let output = engine
            .render(RenderInput {
                compiled,
                vars: serde_json::from_value(json!({"title":"Alpha"})).expect("vars"),
            })
            .expect("render");

        assert!(output.html.contains("<h2>Alpha</h2>"));
        assert!(output.html.contains("badge"));
    }

    #[test]
    fn markdown_processor_renders_j_md_block() {
        let mut engine = JosieWebEngine::new();
        let input = CompileInput {
            template: "<main><j-md># Title\n\n- one\n- two\n\nplain **bold**</j-md></main><script type=\"application/josie+json\">{\"kind\":\"program\",\"version\":\"0.1.0\",\"steps\":[],\"actions\":{},\"resources\":[]}</script>".to_string(),
            components: HashMap::new(),
            processors: vec!["markdown".to_string()],
            allow_list: vec![],
            load_scripts: vec![],
            runtime_mode: None,
            cache: None,
            cache_policy: None,
        };
        let compiled = engine.compile(input).expect("compile");
        let rendered = engine
            .render(RenderInput {
                compiled,
                vars: Map::new(),
            })
            .expect("render");

        assert!(rendered.html.contains("<h1>Title</h1>"));
        assert!(rendered.html.contains("<ul><li>one</li><li>two</li></ul>"));
        assert!(rendered.html.contains("<strong>bold</strong>"));
    }

    #[test]
    fn markdown_bindings_render_from_vars_and_filter_urls() {
        let mut engine = JosieWebEngine::new();
        let input = CompileInput {
            template: "<main><article j-md-var=\"server.content\"></article></main><script type=\"application/josie+json\">{\"kind\":\"program\",\"version\":\"0.1.0\",\"steps\":[],\"actions\":{},\"resources\":[]}</script>".to_string(),
            components: HashMap::new(),
            processors: vec!["markdown".to_string()],
            allow_list: vec![AllowListRule {
                rule_type: "url".to_string(),
                value: "https://docs.example.com".to_string(),
            }],
            load_scripts: vec![],
            runtime_mode: None,
            cache: None,
            cache_policy: None,
        };
        let compiled = engine.compile(input).expect("compile");
        let rendered = engine
            .render(RenderInput {
                compiled,
                vars: serde_json::from_value(json!({
                    "server": {
                        "content": "# Intro\n\n[Good](https://docs.example.com/start)\n\n[Bad](https://evil.example/boom)"
                    }
                }))
                .expect("vars"),
            })
            .expect("render");

        assert!(rendered.html.contains("<h1>Intro</h1>"));
        assert!(rendered
            .html
            .contains("<a href=\"https://docs.example.com/start\">Good</a>"));
        assert!(!rendered.html.contains("href=\"https://evil.example/boom\""));
        assert!(rendered.html.contains(">Bad<"));
    }

    #[test]
    fn markdown_bindings_url_star_allows_all_links() {
        let mut engine = JosieWebEngine::new();
        let input = CompileInput {
            template: "<main><article j-md-var=\"server.content\"></article></main><script type=\"application/josie+json\">{\"kind\":\"program\",\"version\":\"0.1.0\",\"steps\":[],\"actions\":{},\"resources\":[]}</script>".to_string(),
            components: HashMap::new(),
            processors: vec!["markdown".to_string()],
            allow_list: vec![AllowListRule {
                rule_type: "url".to_string(),
                value: "*".to_string(),
            }],
            load_scripts: vec![],
            runtime_mode: None,
            cache: None,
            cache_policy: None,
        };
        let compiled = engine.compile(input).expect("compile");
        let rendered = engine
            .render(RenderInput {
                compiled,
                vars: serde_json::from_value(json!({
                    "server": {
                        "content": "[A](https://a.example)\n\n[B](https://b.example)"
                    }
                }))
                .expect("vars"),
            })
            .expect("render");

        assert!(rendered.html.contains("<a href=\"https://a.example\">A</a>"));
        assert!(rendered.html.contains("<a href=\"https://b.example\">B</a>"));
    }

    #[test]
    fn security_script_wall_only_keeps_load_scripts() {
        let mut engine = JosieWebEngine::new();
        let input = CompileInput {
            template: "<main>ok</main><script src=\"https://cdn.jsdelivr.net/npm/three@0.161.0/build/three.min.js\" defer integrity=\"sha384-abc\"></script><script src=\"https://evil.example/x.js\"></script><script>window.pwned=1</script><script type=\"application/josie+json\">{\"kind\":\"program\",\"version\":\"0.1.0\",\"steps\":[],\"actions\":{},\"resources\":[]}</script>".to_string(),
            components: HashMap::new(),
            processors: vec!["security".to_string()],
            allow_list: vec![AllowListRule {
                rule_type: "js".to_string(),
                value: "https://cdn.jsdelivr.net/".to_string(),
            }],
            load_scripts: vec!["https://cdn.jsdelivr.net/npm/three@0.161.0/build/three.min.js".to_string()],
            runtime_mode: None,
            cache: None,
            cache_policy: None,
        };
        let compiled = engine.compile(input).expect("compile");
        let rendered = engine
            .render(RenderInput {
                compiled,
                vars: Map::new(),
            })
            .expect("render");

        assert!(rendered
            .html
            .contains("<script src=\"https://cdn.jsdelivr.net/npm/three@0.161.0/build/three.min.js\" defer integrity=\"sha384-abc\"></script>"));
        assert!(!rendered.html.contains("https://evil.example/x.js"));
        assert!(!rendered.html.contains("window.pwned=1"));
        assert!(rendered
            .diagnostics
            .iter()
            .any(|d| d.code == "security_script_wall"));
    }

    #[test]
    fn security_script_wall_blocks_when_not_in_load_scripts() {
        let mut engine = JosieWebEngine::new();
        let input = CompileInput {
            template: "<main>ok</main><script src=\"https://cdn.jsdelivr.net/npm/three@0.161.0/build/three.min.js\"></script><script type=\"application/josie+json\">{\"kind\":\"program\",\"version\":\"0.1.0\",\"steps\":[],\"actions\":{},\"resources\":[]}</script>".to_string(),
            components: HashMap::new(),
            processors: vec!["security".to_string()],
            allow_list: vec![AllowListRule {
                rule_type: "js".to_string(),
                value: "*".to_string(),
            }],
            load_scripts: vec![],
            runtime_mode: None,
            cache: None,
            cache_policy: None,
        };
        let compiled = engine.compile(input).expect("compile");
        let rendered = engine
            .render(RenderInput {
                compiled,
                vars: Map::new(),
            })
            .expect("render");

        assert!(!rendered
            .html
            .contains("https://cdn.jsdelivr.net/npm/three@0.161.0/build/three.min.js"));
    }

    #[test]
    fn security_script_wall_requires_integrity_for_remote_scripts() {
        let mut engine = JosieWebEngine::new();
        let input = CompileInput {
            template: "<main>ok</main><script src=\"https://cdn.jsdelivr.net/npm/three@0.161.0/build/three.min.js\"></script><script type=\"application/josie+json\">{\"kind\":\"program\",\"version\":\"0.1.0\",\"steps\":[],\"actions\":{},\"resources\":[]}</script>".to_string(),
            components: HashMap::new(),
            processors: vec!["security".to_string()],
            allow_list: vec![AllowListRule {
                rule_type: "js".to_string(),
                value: "*".to_string(),
            }],
            load_scripts: vec!["https://cdn.jsdelivr.net/npm/three@0.161.0/build/three.min.js".to_string()],
            runtime_mode: None,
            cache: None,
            cache_policy: None,
        };
        let compiled = engine.compile(input).expect("compile");
        let rendered = engine
            .render(RenderInput {
                compiled,
                vars: Map::new(),
            })
            .expect("render");

        assert!(!rendered.html.contains("three.min.js"));
        assert!(rendered
            .diagnostics
            .iter()
            .any(|d| d.code == "security_script_wall" && d.message.contains("blocked_integrity=1")));
    }

    #[test]
    fn security_script_wall_supports_pinned_integrity_rule() {
        let mut engine = JosieWebEngine::new();
        let input = CompileInput {
            template: "<main>ok</main><script src=\"https://cdn.jsdelivr.net/npm/three@0.161.0/build/three.min.js\" integrity=\"sha384-abc\"></script><script type=\"application/josie+json\">{\"kind\":\"program\",\"version\":\"0.1.0\",\"steps\":[],\"actions\":{},\"resources\":[]}</script>".to_string(),
            components: HashMap::new(),
            processors: vec!["security".to_string()],
            allow_list: vec![AllowListRule {
                rule_type: "js".to_string(),
                value: "*".to_string(),
            }],
            load_scripts: vec![
                "https://cdn.jsdelivr.net/npm/three@0.161.0/build/three.min.js|sha384-abc"
                    .to_string(),
            ],
            runtime_mode: None,
            cache: None,
            cache_policy: None,
        };
        let compiled = engine.compile(input).expect("compile");
        let rendered = engine
            .render(RenderInput {
                compiled,
                vars: Map::new(),
            })
            .expect("render");

        assert!(rendered.html.contains("three.min.js"));
        assert!(rendered.html.contains("integrity=\"sha384-abc\""));
    }

    #[test]
    fn security_css_wall_blocks_disallowed_stylesheet_links() {
        let mut engine = JosieWebEngine::new();
        let input = CompileInput {
            template: "<!doctype html><html><head><link rel=\"stylesheet\" href=\"https://evil.example/x.css\"><link rel=\"stylesheet\" href=\"https://cdn.example/safe.css\"></head><body>ok</body></html><script type=\"application/josie+json\">{\"kind\":\"program\",\"version\":\"0.1.0\",\"steps\":[],\"actions\":{},\"resources\":[]}</script>".to_string(),
            components: HashMap::new(),
            processors: vec!["security".to_string()],
            allow_list: vec![
                AllowListRule {
                    rule_type: "css".to_string(),
                    value: "https://cdn.example/".to_string(),
                },
                AllowListRule {
                    rule_type: "url".to_string(),
                    value: "https://cdn.example/".to_string(),
                },
            ],
            load_scripts: vec![],
            runtime_mode: None,
            cache: None,
            cache_policy: None,
        };
        let compiled = engine.compile(input).expect("compile");
        let rendered = engine
            .render(RenderInput {
                compiled,
                vars: Map::new(),
            })
            .expect("render");

        assert!(!rendered.html.contains("https://evil.example/x.css"));
        assert!(rendered.html.contains("https://cdn.example/safe.css"));
        assert!(rendered
            .diagnostics
            .iter()
            .any(|d| d.code == "security_css_wall"));
    }

    #[test]
    fn security_url_wall_strips_disallowed_attrs_and_inline_handlers() {
        let mut engine = JosieWebEngine::new();
        let input = CompileInput {
            template: "<main><a href=\"https://ok.example/docs\" onclick=\"alert(1)\">ok</a><a href=\"https://evil.example/boom\">bad</a><img src=\"https://evil.example/x.png\"></main><script type=\"application/josie+json\">{\"kind\":\"program\",\"version\":\"0.1.0\",\"steps\":[],\"actions\":{},\"resources\":[]}</script>".to_string(),
            components: HashMap::new(),
            processors: vec!["security".to_string()],
            allow_list: vec![AllowListRule {
                rule_type: "url".to_string(),
                value: "https://ok.example/".to_string(),
            }],
            load_scripts: vec![],
            runtime_mode: None,
            cache: None,
            cache_policy: None,
        };
        let compiled = engine.compile(input).expect("compile");
        let rendered = engine
            .render(RenderInput {
                compiled,
                vars: Map::new(),
            })
            .expect("render");

        assert!(rendered.html.contains("href=\"https://ok.example/docs\""));
        assert!(!rendered.html.contains("onclick="));
        assert!(!rendered.html.contains("href=\"https://evil.example/boom\""));
        assert!(!rendered.html.contains("src=\"https://evil.example/x.png\""));
        assert!(rendered
            .diagnostics
            .iter()
            .any(|d| d.code == "security_url_wall"));
    }

    #[test]
    fn compile_extracts_reactive_bindings_and_data_jid() {
        let mut engine = JosieWebEngine::new();
        let input = CompileInput {
            template: "<main><h1 j-text=\"client.title\"></h1><input j-model=\"client.name\"><div j-attr:class=\"client.boxClass\"></div></main><script type=\"application/josie+json\">{\"kind\":\"program\",\"version\":\"0.1.0\",\"state\":{\"client\":{\"title\":\"Hi\",\"name\":\"A\",\"boxClass\":\"x\"}},\"steps\":[],\"actions\":{},\"resources\":[]}</script>".to_string(),
            components: HashMap::new(),
            processors: vec!["security".to_string()],
            allow_list: vec![],
            load_scripts: vec![],
            runtime_mode: Some(RuntimeMode::Compiled),
            cache: None,
            cache_policy: None,
        };
        let compiled = engine.compile(input).expect("compile");
        let rendered = engine
            .render(RenderInput {
                compiled,
                vars: Map::new(),
            })
            .expect("render");

        assert!(rendered.html.contains("data-jid=\"j0\""));
        assert!(rendered.html.contains("data-jid=\"j1\""));
        assert!(rendered.html.contains("data-jid=\"j2\""));
        assert!(rendered
            .diagnostics
            .iter()
            .any(|d| d.code == "reactive_bindings_extracted"));
    }

    #[test]
    fn render_inject_vars_escapes_html() {
        let mut engine = JosieWebEngine::new();
        let input = CompileInput {
            template: "<div>{{server.payload}}</div><script type=\"application/josie+json\">{\"kind\":\"program\",\"version\":\"0.1.0\",\"steps\":[],\"actions\":{},\"resources\":[]}</script>".to_string(),
            components: HashMap::new(),
            processors: vec![],
            allow_list: vec![],
            load_scripts: vec![],
            runtime_mode: None,
            cache: None,
            cache_policy: None,
        };
        let compiled = engine.compile(input).expect("compile");
        let rendered = engine
            .render(RenderInput {
                compiled,
                vars: serde_json::from_value(json!({
                    "server": {
                        "payload": "<script>alert('xss')</script>"
                    }
                }))
                .expect("vars"),
            })
            .expect("render");

        assert!(!rendered.html.contains("<script>alert('xss')</script>"));
        assert!(rendered
            .html
            .contains("&lt;script&gt;alert(&#39;xss&#39;)&lt;/script&gt;"));
    }

    #[test]
    fn compile_fails_when_action_references_unknown_step() {
        let mut engine = JosieWebEngine::new();
        let input = CompileInput {
            template: "<main>ok</main><script type=\"application/josie+json\">{\"kind\":\"program\",\"version\":\"0.1.0\",\"steps\":[{\"id\":\"s.ok\",\"op\":\"return\",\"args\":[1]}],\"actions\":{\"click\":{\"runStep\":\"s.missing\"}},\"resources\":[]}</script>".to_string(),
            components: HashMap::new(),
            processors: vec![],
            allow_list: vec![],
            load_scripts: vec![],
            runtime_mode: None,
            cache: None,
            cache_policy: None,
        };
        let err = engine.compile(input).expect_err("compile must fail");
        assert!(err.contains("missing step id"));
    }

    #[test]
    fn compile_validates_memo_contract() {
        let mut engine = JosieWebEngine::new();
        let input = CompileInput {
            template: "<main>ok</main><script type=\"application/josie+json\">{\"kind\":\"program\",\"version\":\"0.1.0\",\"steps\":[{\"id\":\"memo.compute\",\"op\":\"return\",\"args\":[1]}],\"actions\":{},\"memos\":[{\"id\":\"m.a\",\"deps\":[\"client.items\"],\"runStep\":\"memo.compute\"}],\"resources\":[]}</script>".to_string(),
            components: HashMap::new(),
            processors: vec![],
            allow_list: vec![],
            load_scripts: vec![],
            runtime_mode: None,
            cache: None,
            cache_policy: None,
        };
        let err = engine.compile(input).expect_err("compile must fail");
        assert!(err.contains("program.memos[0].into is required"));
    }

    #[test]
    fn compile_validates_effect_contract() {
        let mut engine = JosieWebEngine::new();
        let input = CompileInput {
            template: "<main>ok</main><script type=\"application/josie+json\">{\"kind\":\"program\",\"version\":\"0.1.0\",\"steps\":[{\"id\":\"fx.save\",\"op\":\"return\",\"args\":[1]}],\"actions\":{},\"effects\":[{\"id\":\"fx.1\",\"deps\":[\"client.items\"],\"runStep\":\"fx.save\",\"immediate\":\"yes\"}],\"resources\":[]}</script>".to_string(),
            components: HashMap::new(),
            processors: vec![],
            allow_list: vec![],
            load_scripts: vec![],
            runtime_mode: None,
            cache: None,
            cache_policy: None,
        };
        let err = engine.compile(input).expect_err("compile must fail");
        assert!(err.contains("program.effects[0].immediate must be a boolean"));
    }

    #[test]
    fn tailwind_processor_injects_css_rules() {
        let mut engine = JosieWebEngine::new();
        let input = CompileInput {
            template: "<!doctype html><html><head></head><body><div class=\"p-4 text-red-500\">x</div></body></html><script type=\"application/josie+json\">{\"kind\":\"program\",\"version\":\"0.1.0\",\"state\":{\"client\":{\"rowClass\":\"border-emerald-500 bg-emerald-500/10\"}},\"steps\":[],\"actions\":{},\"resources\":[]}</script>".to_string(),
            components: HashMap::new(),
            processors: vec!["tailwind".to_string()],
            allow_list: vec![],
            load_scripts: vec![],
            runtime_mode: None,
            cache: None,
            cache_policy: None,
        };
        let compiled = engine.compile(input).expect("compile");
        let rendered = engine
            .render(RenderInput {
                compiled,
                vars: Map::new(),
            })
            .expect("render");

        assert!(rendered.html.contains("<style data-josie-tw>"));
        assert!(rendered.html.contains(".p-4"));
        assert!(rendered.html.contains(".text-red-500"));
        assert!(rendered.html.contains(".border-emerald-500"));
    }

    #[test]
    fn compile_cache_disk_roundtrip() {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time")
            .as_nanos();
        let root = std::env::temp_dir().join(format!("josie-web-cache-test-{nanos}"));

        let mut engine = JosieWebEngine::new().with_disk_root(&root);
        let input = CompileInput {
            template: "<div>Hello {{name}}</div><script type=\"application/josie+json\">{\"kind\":\"program\",\"version\":\"0.1.0\",\"steps\":[]}</script>".to_string(),
            components: HashMap::new(),
            processors: vec![],
            allow_list: vec![],
            load_scripts: vec![],
            runtime_mode: None,
            cache: Some(CacheRef {
                namespace: "default".to_string(),
                key: "home".to_string(),
                version: Some("1".to_string()),
            }),
            cache_policy: Some(CachePolicy::Disk),
        };

        let compiled = engine.compile(input.clone()).expect("compile");
        let mut engine2 = JosieWebEngine::new().with_disk_root(&root);
        let hit = engine2
            .cache_get(&CacheRef {
                namespace: "default".to_string(),
                key: "home".to_string(),
                version: Some("1".to_string()),
            })
            .expect("cache hit");
        assert_eq!(hit.fingerprint, compiled.fingerprint);
    }

    #[test]
    fn export_compiled_injects_runtime_and_minifies_when_enabled() {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time")
            .as_nanos();
        let out = std::env::temp_dir().join(format!("josie-web-export-test-{nanos}"));

        let mut engine = JosieWebEngine::new();
        let compiled = engine
            .compile(CompileInput {
                template: "<!doctype html>\n<html>\n<body>\n<main>Hello</main>\n<script type=\"application/josie+json\">{\"kind\":\"program\",\"version\":\"0.1.0\",\"state\":{},\"actions\":{},\"resources\":[],\"steps\":[]}</script>\n</body>\n</html>".to_string(),
                components: HashMap::new(),
                processors: vec!["minify".to_string()],
                allow_list: vec![],
                load_scripts: vec![],
                runtime_mode: None,
                cache: None,
                cache_policy: None,
            })
            .expect("compile");

        engine
            .export_compiled(&compiled, &out)
            .expect("export compiled");

        let index = fs::read_to_string(out.join("index.html")).expect("read index");
        let runtime = fs::read_to_string(out.join("josie-runtime.js")).expect("read runtime");
        let program = fs::read_to_string(out.join("program.josie")).expect("read program");
        let compiled_js = fs::read_to_string(out.join("program.compiled.js")).expect("read program.compiled.js");

        assert!(!index.contains("__JOSIE_BOOTSTRAP__"));
        assert!(index.contains("__JOSIE_AUTO_INIT__=false"));
        assert!(index.contains("josie-runtime.js"));
        assert!(index.contains("program.compiled.js"));
        assert!(compiled_js.contains("__JOSIE_INITIAL_STATE__"));
        assert!(compiled_js.contains("__JOSIE_BINDINGS__"));
        assert!(index.contains('\n'));
        assert!(runtime.contains("window.JOSIE"));
        assert!(!program.contains('\n'));
        assert!(compiled_js.contains("window.__JOSIE_INITIAL_STATE__"));
    }

    #[test]
    fn render_with_runtime_inline_embeds_assets_for_webhook_pages() {
        let mut engine = JosieWebEngine::new();
        let compiled = engine
            .compile(CompileInput {
                template: "<!doctype html><html><body><main><h1 j-text=\"client.title\"></h1></main><script type=\"application/josie+json\">{\"kind\":\"program\",\"version\":\"0.1.0\",\"state\":{\"client\":{\"title\":\"Todo\"}},\"actions\":{},\"resources\":[],\"steps\":[]}</script></body></html>".to_string(),
                components: HashMap::new(),
                processors: vec!["minify".to_string()],
                allow_list: vec![],
                load_scripts: vec![],
                runtime_mode: Some(RuntimeMode::Compiled),
                cache: None,
                cache_policy: None,
            })
            .expect("compile");

        let rendered = engine
            .render_with_runtime(
                RenderInput {
                    compiled,
                    vars: Map::new(),
                },
                true,
            )
            .expect("render with runtime");

        assert!(rendered.html.contains("__JOSIE_AUTO_INIT__=false"));
        assert!(rendered.html.contains("window.__JOSIE_INITIAL_STATE__"));
        assert!(rendered.html.contains("window.__JOSIE_BINDINGS__"));
        assert!(!rendered.html.contains("src=\"./josie-runtime.js\""));
        assert!(!rendered.html.contains("src=\"./program.compiled.js\""));
    }

    #[test]
    fn render_with_runtime_merges_render_vars_into_server_state() {
        let mut engine = JosieWebEngine::new();
        let compiled = engine
            .compile(CompileInput {
                template: "<!doctype html><html><body><main><h1 j-text=\"server.title\"></h1></main><script type=\"application/josie+json\">{\"kind\":\"program\",\"version\":\"0.1.0\",\"state\":{\"server\":{\"title\":\"from-program\"}},\"actions\":{},\"resources\":[],\"steps\":[]}</script></body></html>".to_string(),
                components: HashMap::new(),
                processors: vec!["minify".to_string()],
                allow_list: vec![],
                load_scripts: vec![],
                runtime_mode: Some(RuntimeMode::Compiled),
                cache: None,
                cache_policy: None,
            })
            .expect("compile");

        let mut vars = Map::new();
        vars.insert("title".to_string(), json!("from-render"));
        let rendered = engine
            .render_with_runtime(
                RenderInput {
                    compiled,
                    vars,
                },
                true,
            )
            .expect("render with runtime");

        assert!(rendered
            .html
            .contains("window.__JOSIE_SERVER_STATE__={\"title\":\"from-render\"}"));
    }

    #[test]
    fn export_compiled_interpreted_mode_skips_compiled_bundle() {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time")
            .as_nanos();
        let out = std::env::temp_dir().join(format!("josie-web-export-interpreted-{nanos}"));

        let mut engine = JosieWebEngine::new();
        let compiled = engine
            .compile(CompileInput {
                template: "<!doctype html><html><body><script type=\"application/josie+json\">{\"kind\":\"program\",\"version\":\"0.1.0\",\"state\":{},\"actions\":{},\"resources\":[],\"steps\":[]}</script></body></html>".to_string(),
                components: HashMap::new(),
                processors: vec![],
                allow_list: vec![],
                load_scripts: vec![],
                runtime_mode: Some(RuntimeMode::Interpreted),
                cache: None,
                cache_policy: None,
            })
            .expect("compile");

        engine
            .export_compiled(&compiled, &out)
            .expect("export compiled");

        let index = fs::read_to_string(out.join("index.html")).expect("read index");
        assert!(index.contains("josie-runtime-dev.js"));
        assert!(!index.contains("program.compiled.js"));
        assert!(out.join("josie-runtime-dev.js").exists());
        assert!(!out.join("josie-runtime.js").exists());
        assert!(!out.join("program.compiled.js").exists());
    }

    #[test]
    fn build_static_site_from_hierarchical_folders() {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time")
            .as_nanos();
        let root = std::env::temp_dir().join(format!("josie-web-static-site-{nanos}"));
        let source = root.join("site");
        let out = root.join("dist");

        fs::create_dir_all(source.join("pages/blog")).expect("create pages");
        fs::create_dir_all(source.join("components/ui")).expect("create components");
        fs::create_dir_all(source.join("public")).expect("create public");

        fs::write(
            source.join("components/ui/badge.josieml"),
            "<span class=\"text-emerald-500\">BADGE_OK</span>",
        )
        .expect("write component");
        fs::write(
            source.join("pages/index.josieml"),
            "<main><h1>Home</h1><j-component name=\"ui/badge\" /></main><script type=\"application/josie+json\">{\"kind\":\"program\",\"version\":\"0.1.0\",\"steps\":[],\"actions\":{},\"resources\":[]}</script>",
        )
        .expect("write page index");
        fs::write(
            source.join("pages/blog/post.josieml"),
            "<main><h1>Post</h1></main><script type=\"application/josie+json\">{\"kind\":\"program\",\"version\":\"0.1.0\",\"steps\":[],\"actions\":{},\"resources\":[]}</script>",
        )
        .expect("write page post");
        fs::write(source.join("public/robots.txt"), "User-agent: *").expect("write public asset");

        let mut engine = JosieWebEngine::new();
        let result = engine
            .build_static_site(StaticSiteBuildInput {
                source_dir: source.to_string_lossy().to_string(),
                output_dir: out.to_string_lossy().to_string(),
                pages_dir: None,
                components_dir: None,
                public_dir: None,
                processors: vec!["tailwind".to_string()],
                allow_list: vec![],
                load_scripts: vec![],
                runtime_mode: Some(RuntimeMode::Compiled),
                cache_policy: None,
                dev_hmr: None,
            })
            .expect("build static site");

        assert_eq!(result.pages_built, 2);
        assert!(result.generated_routes.contains(&"/".to_string()));
        assert!(result.generated_routes.contains(&"/blog/post".to_string()));
        assert_eq!(result.assets_copied, 1);

        assert!(out.join("index.html").exists());
        assert!(out.join("blog/post/index.html").exists());
        assert!(out.join("josie-runtime.js").exists());
        assert!(out.join("blog/post/josie-runtime.js").exists());
        assert!(out.join("robots.txt").exists());

        let index_html = fs::read_to_string(out.join("index.html")).expect("read index");
        assert!(index_html.contains("BADGE_OK"));
    }

    #[test]
    fn minify_html_preserves_attribute_boundaries() {
        let src = r#"
<!doctype html>
<html>
  <body>
    <input
      type="text"
      placeholder="A B"
      class="x y"
    />
  </body>
</html>
"#;
        let out = minify_html_light(src);
        assert!(out.contains(r#"<input type="text" placeholder="A B" class="x y" />"#));
        assert!(!out.contains("<inputtype="));
    }
}
