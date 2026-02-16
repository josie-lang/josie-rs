use crate::{JosieDocument, render_with_runtime};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::{HashMap, VecDeque};
use std::fs;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::path::{Component, Path, PathBuf};

const IR_VERSION: &str = "josieml-ir-v0.3";
const ENGINE_VERSION: &str = "josie-web-engine-v0.3";

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct GroupConfig {
    #[serde(default)]
    pub allowed_resources: Vec<String>,
    #[serde(default)]
    pub processors: Vec<String>,
    pub default_theme: Option<String>,
    #[serde(default)]
    pub http_allow_patterns: Vec<String>,
    #[serde(default = "default_http_methods")]
    pub http_allow_methods: Vec<String>,
    #[serde(default = "default_http_timeout_ms")]
    pub http_timeout_ms: u64,
}

fn default_http_methods() -> Vec<String> {
    vec![
        "GET".to_string(),
        "POST".to_string(),
        "PUT".to_string(),
        "DELETE".to_string(),
        "PATCH".to_string(),
    ]
}

fn default_http_timeout_ms() -> u64 {
    5000
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct ResourceDefinition {
    #[serde(rename = "type")]
    pub resource_type: String,
    pub src: String,
    pub integrity: Option<String>,
    #[serde(default)]
    pub deps: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SystemConfig {
    #[serde(default)]
    pub root_path: PathBuf,
    #[serde(default)]
    pub system_path: PathBuf,
    #[serde(default)]
    pub max_memory_mb: usize,
    #[serde(default)]
    pub groups: HashMap<String, GroupConfig>,
    #[serde(default)]
    pub resources: HashMap<String, ResourceDefinition>,
}

impl SystemConfig {
    pub fn minimal(root_path: PathBuf) -> Self {
        let mut groups = HashMap::new();
        groups.insert(
            "default".to_string(),
            GroupConfig {
                allowed_resources: Vec::new(),
                processors: vec!["tailwind".to_string()],
                default_theme: None,
                http_allow_patterns: Vec::new(),
                http_allow_methods: default_http_methods(),
                http_timeout_ms: default_http_timeout_ms(),
            },
        );

        Self {
            system_path: root_path.clone(),
            root_path,
            max_memory_mb: 128,
            groups,
            resources: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AllowedResource {
    pub slug: String,
    pub path: String,
    #[serde(rename = "type")]
    pub resource_type: String,
    pub integrity: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RouteJobConfig {
    #[serde(default)]
    pub allowed_resources: Vec<AllowedResource>,
    #[serde(default)]
    pub processors: Vec<String>,
    #[serde(default)]
    pub http_allow_patterns: Vec<String>,
    #[serde(default = "default_http_methods")]
    pub http_allow_methods: Vec<String>,
    #[serde(default = "default_http_timeout_ms")]
    pub http_timeout_ms: u64,
    pub cache_policy: CachePolicy,
    pub strict_mode: bool,
}

impl Default for RouteJobConfig {
    fn default() -> Self {
        Self {
            allowed_resources: Vec::new(),
            processors: vec!["tailwind".to_string()],
            http_allow_patterns: Vec::new(),
            http_allow_methods: default_http_methods(),
            http_timeout_ms: default_http_timeout_ms(),
            cache_policy: CachePolicy::Auto,
            strict_mode: true,
        }
    }
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
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

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct RouteJob {
    pub user: String,
    pub group: String,
    pub file_path: PathBuf,
    #[serde(default)]
    pub params: HashMap<String, String>,
    pub config: RouteJobConfig,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct WebRequestContext {
    #[serde(default)]
    pub method: String,
    #[serde(default)]
    pub path: String,
    #[serde(default)]
    pub body: Value,
    #[serde(default)]
    pub user: String,
    #[serde(default)]
    pub group: String,
    #[serde(default)]
    pub params: HashMap<String, String>,
    #[serde(default)]
    pub query: HashMap<String, String>,
    #[serde(default)]
    pub headers: HashMap<String, String>,
    #[serde(default)]
    pub cookies: HashMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct TemplateSlot {
    pub position: usize,
    pub key: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AssetRef {
    #[serde(rename = "type")]
    pub asset_type: String,
    pub name: String,
    pub src: String,
    pub integrity: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum DiagnosticLevel {
    Info,
    Warn,
    Error,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Diagnostic {
    pub level: DiagnosticLevel,
    pub code: String,
    pub message: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct ViewTemplate {
    pub id: String,
    pub ir_version: String,
    pub buffer: Vec<u8>,
    #[serde(default)]
    pub slots: Vec<TemplateSlot>,
    #[serde(default)]
    pub assets: Vec<AssetRef>,
    #[serde(default)]
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct RenderOutput {
    pub html: String,
    #[serde(default)]
    pub hydration_payload: serde_json::Map<String, Value>,
    #[serde(default)]
    pub assets: Vec<AssetRef>,
    pub fingerprint: String,
    #[serde(default)]
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
struct UserConfigOverride {
    pub allowed_resources: Option<Vec<String>>,
    pub processors: Option<Vec<String>>,
    pub http_allow_patterns: Option<Vec<String>>,
    pub http_allow_methods: Option<Vec<String>>,
    pub http_timeout_ms: Option<u64>,
    pub cache_policy: Option<CachePolicy>,
    pub strict_mode: Option<bool>,
}

#[derive(Debug, Clone)]
struct CacheEntry {
    template: ViewTemplate,
    bytes: usize,
}

pub trait JosieWebCore {
    fn init(&mut self, config: SystemConfig) -> Result<(), String>;
    fn gc(&mut self);
    fn match_route(&self, user: &str, group: &str, url: &str) -> Result<RouteJob, String>;
    fn compile_view(&mut self, job: &RouteJob) -> Result<ViewTemplate, String>;
    fn render_view(&self, view: &ViewTemplate, data: &Value) -> Result<RenderOutput, String>;
    fn invalidate(&mut self, keys: &[String]);
    fn prewarm(&mut self, route_keys: &[String]) -> Result<(), String>;
}

pub trait JosieWebServer: JosieWebCore {
    fn serve(&mut self, bind: &str, port: u16) -> Result<(), String>;
    fn dev_watch(&mut self, paths: &[PathBuf]) -> Result<(), String>;
    fn shutdown(&mut self) -> Result<(), String>;
}

#[derive(Debug, Default)]
pub struct JosieWebEngine {
    config: Option<SystemConfig>,
    l1_cache: HashMap<String, CacheEntry>,
    l1_order: VecDeque<String>,
    l1_bytes: usize,
}

impl JosieWebEngine {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn config(&self) -> Option<&SystemConfig> {
        self.config.as_ref()
    }

    pub fn render_route(
        &mut self,
        user: &str,
        group: &str,
        url: &str,
        data: &Value,
    ) -> Result<(RouteJob, RenderOutput), String> {
        let job = self.match_route(user, group, url)?;
        let view = self.compile_view(&job)?;
        let output = self.render_view(&view, data)?;
        Ok((job, output))
    }

    fn max_l1_bytes(&self) -> usize {
        let mb = self
            .config
            .as_ref()
            .map(|c| c.max_memory_mb.max(1))
            .unwrap_or(64);
        mb * 1024 * 1024
    }

    fn touch_cache_key(&mut self, key: &str) {
        if let Some(pos) = self.l1_order.iter().position(|existing| existing == key) {
            self.l1_order.remove(pos);
        }
        self.l1_order.push_back(key.to_string());
    }

    fn put_cache(&mut self, key: String, template: ViewTemplate) {
        let bytes = template.buffer.len();
        if let Some(old) = self
            .l1_cache
            .insert(key.clone(), CacheEntry { template, bytes })
        {
            self.l1_bytes = self.l1_bytes.saturating_sub(old.bytes);
        }
        self.l1_bytes = self.l1_bytes.saturating_add(bytes);
        self.touch_cache_key(&key);
        self.gc();
    }

    fn get_cache(&mut self, key: &str) -> Option<ViewTemplate> {
        let hit = self.l1_cache.get(key).map(|entry| entry.template.clone());
        if hit.is_some() {
            self.touch_cache_key(key);
        }
        hit
    }

    fn merge_route_config(&self, user_root: &Path, group: &str) -> RouteJobConfig {
        let mut merged = RouteJobConfig::default();

        if let Some(cfg) = &self.config {
            if let Some(group_cfg) = cfg.groups.get(group) {
                merged.processors = if group_cfg.processors.is_empty() {
                    merged.processors
                } else {
                    group_cfg.processors.clone()
                };
                merged.http_allow_patterns = group_cfg.http_allow_patterns.clone();
                merged.http_allow_methods = if group_cfg.http_allow_methods.is_empty() {
                    merged.http_allow_methods
                } else {
                    group_cfg.http_allow_methods.clone()
                };
                merged.http_timeout_ms = if group_cfg.http_timeout_ms == 0 {
                    merged.http_timeout_ms
                } else {
                    group_cfg.http_timeout_ms
                };

                merged.allowed_resources = group_cfg
                    .allowed_resources
                    .iter()
                    .filter_map(|name| {
                        cfg.resources.get(name).map(|res| AllowedResource {
                            slug: name.clone(),
                            path: res.src.clone(),
                            resource_type: res.resource_type.clone(),
                            integrity: res.integrity.clone(),
                        })
                    })
                    .collect();
            }
        }

        let override_path = user_root.join("josie.config.json");
        if override_path.is_file() {
            if let Ok(content) = fs::read_to_string(&override_path) {
                if let Ok(ovr) = serde_json::from_str::<UserConfigOverride>(&content) {
                    if let Some(processors) = ovr.processors {
                        merged.processors = processors;
                    }
                    if let Some(http_allow_patterns) = ovr.http_allow_patterns {
                        merged.http_allow_patterns = http_allow_patterns;
                    }
                    if let Some(http_allow_methods) = ovr.http_allow_methods {
                        merged.http_allow_methods = if http_allow_methods.is_empty() {
                            merged.http_allow_methods
                        } else {
                            http_allow_methods
                        };
                    }
                    if let Some(http_timeout_ms) = ovr.http_timeout_ms {
                        if http_timeout_ms > 0 {
                            merged.http_timeout_ms = http_timeout_ms;
                        }
                    }
                    if let Some(cache_policy) = ovr.cache_policy {
                        merged.cache_policy = cache_policy;
                    }
                    if let Some(strict_mode) = ovr.strict_mode {
                        merged.strict_mode = strict_mode;
                    }
                    if let Some(allowed) = ovr.allowed_resources {
                        merged.allowed_resources = allowed
                            .into_iter()
                            .filter_map(|name| {
                                self.config
                                    .as_ref()
                                    .and_then(|cfg| cfg.resources.get(&name))
                                    .map(|res| AllowedResource {
                                        slug: name,
                                        path: res.src.clone(),
                                        resource_type: res.resource_type.clone(),
                                        integrity: res.integrity.clone(),
                                    })
                            })
                            .collect();
                    }
                }
            }
        }

        merged
    }

    fn compile_cache_key(&self, job: &RouteJob, source: &str) -> String {
        let mut hasher = DefaultHasher::new();
        ENGINE_VERSION.hash(&mut hasher);
        IR_VERSION.hash(&mut hasher);
        job.user.hash(&mut hasher);
        job.group.hash(&mut hasher);
        job.file_path.hash(&mut hasher);
        source.hash(&mut hasher);
        job.config.processors.hash(&mut hasher);
        for res in &job.config.allowed_resources {
            res.slug.hash(&mut hasher);
            res.path.hash(&mut hasher);
        }
        format!("{:016x}", hasher.finish())
    }
}

impl JosieWebCore for JosieWebEngine {
    fn init(&mut self, config: SystemConfig) -> Result<(), String> {
        self.config = Some(config);
        self.l1_cache.clear();
        self.l1_order.clear();
        self.l1_bytes = 0;
        Ok(())
    }

    fn gc(&mut self) {
        let max_bytes = self.max_l1_bytes();
        while self.l1_bytes > max_bytes {
            let Some(oldest_key) = self.l1_order.pop_front() else {
                break;
            };
            if let Some(old) = self.l1_cache.remove(&oldest_key) {
                self.l1_bytes = self.l1_bytes.saturating_sub(old.bytes);
            }
        }
    }

    fn match_route(&self, user: &str, group: &str, url: &str) -> Result<RouteJob, String> {
        let cfg = self
            .config
            .as_ref()
            .ok_or_else(|| "engine not initialized".to_string())?;
        let rel = sanitize_rel_path(url).ok_or_else(|| "invalid path".to_string())?;
        let user_root = cfg.root_path.join(user);

        let file_path = resolve_document_path(&user_root, &rel)
            .or_else(|| resolve_document_path(&cfg.root_path, &rel))
            .ok_or_else(|| format!("route not found: {}", url))?;

        let merged = self.merge_route_config(&user_root, group);

        Ok(RouteJob {
            user: user.to_string(),
            group: group.to_string(),
            file_path,
            params: HashMap::new(),
            config: merged,
        })
    }

    fn compile_view(&mut self, job: &RouteJob) -> Result<ViewTemplate, String> {
        let source = fs::read_to_string(&job.file_path)
            .map_err(|e| format!("failed to read {}: {e}", job.file_path.display()))?;
        let cache_key = self.compile_cache_key(job, &source);
        if let Some(cached) = self.get_cache(&cache_key) {
            return Ok(cached);
        }

        let ext = job
            .file_path
            .extension()
            .and_then(|s| s.to_str())
            .unwrap_or_default();

        let mut diagnostics = Vec::<Diagnostic>::new();
        let requested_resources = extract_requested_resources(ext, &source).unwrap_or_default();

        let allow_map: HashMap<String, AllowedResource> = job
            .config
            .allowed_resources
            .iter()
            .map(|r| (r.slug.clone(), r.clone()))
            .collect();

        let mut resolved_assets = Vec::<AssetRef>::new();
        for name in requested_resources {
            if let Some(resource) = allow_map.get(&name) {
                let normalized_type = resource.resource_type.trim().to_ascii_lowercase();
                let asset_type = if normalized_type == "css" || normalized_type == "js" {
                    normalized_type
                } else if resource.path.ends_with(".css") {
                    "css".to_string()
                } else {
                    "js".to_string()
                };
                resolved_assets.push(AssetRef {
                    asset_type,
                    name: name.clone(),
                    src: resource.path.clone(),
                    integrity: resource.integrity.clone(),
                });
            } else if job.config.strict_mode {
                return Err(format!("resource '{name}' is not allowed for this route"));
            } else {
                diagnostics.push(Diagnostic {
                    level: DiagnosticLevel::Warn,
                    code: "resource_not_allowed".to_string(),
                    message: format!("resource '{name}' ignored because it is not allowlisted"),
                });
            }
        }

        let mut html = compile_html(&job.config.processors, ext, &source, &mut diagnostics)?;
        html = inject_assets(html, &resolved_assets);

        let slots = extract_slots(&html);
        let template = ViewTemplate {
            id: cache_key.clone(),
            ir_version: IR_VERSION.to_string(),
            buffer: html.into_bytes(),
            slots,
            assets: resolved_assets,
            diagnostics,
        };

        self.put_cache(cache_key, template.clone());
        Ok(template)
    }

    fn render_view(&self, view: &ViewTemplate, data: &Value) -> Result<RenderOutput, String> {
        let mut html = String::from_utf8(view.buffer.clone())
            .map_err(|e| format!("invalid template buffer utf8: {e}"))?;

        if let Some(obj) = data.as_object() {
            for (key, value) in obj {
                let token = format!("{{{{{key}}}}}");
                let value_str = match value {
                    Value::Null => String::new(),
                    Value::String(s) => s.clone(),
                    Value::Number(n) => n.to_string(),
                    Value::Bool(b) => b.to_string(),
                    _ => value.to_string(),
                };
                html = html.replace(&token, &value_str);
            }
        }

        let mut hasher = DefaultHasher::new();
        html.hash(&mut hasher);
        let fingerprint = format!("{:016x}", hasher.finish());

        let hydration_payload = data.as_object().cloned().unwrap_or_default();

        Ok(RenderOutput {
            html,
            hydration_payload,
            assets: view.assets.clone(),
            fingerprint,
            diagnostics: view.diagnostics.clone(),
        })
    }

    fn invalidate(&mut self, keys: &[String]) {
        if keys.is_empty() {
            self.l1_cache.clear();
            self.l1_order.clear();
            self.l1_bytes = 0;
            return;
        }

        for key in keys {
            if let Some(entry) = self.l1_cache.remove(key) {
                self.l1_bytes = self.l1_bytes.saturating_sub(entry.bytes);
            }
            if let Some(pos) = self.l1_order.iter().position(|existing| existing == key) {
                self.l1_order.remove(pos);
            }
        }
    }

    fn prewarm(&mut self, route_keys: &[String]) -> Result<(), String> {
        for route in route_keys {
            let job = self.match_route("default", "default", route)?;
            let _ = self.compile_view(&job)?;
        }
        Ok(())
    }
}

fn sanitize_rel_path(path: &str) -> Option<PathBuf> {
    let trimmed = path.trim_start_matches('/');
    let rel = PathBuf::from(trimmed);
    for comp in rel.components() {
        if matches!(
            comp,
            Component::ParentDir | Component::RootDir | Component::Prefix(_)
        ) {
            return None;
        }
    }
    Some(rel)
}

fn resolve_document_path(root: &Path, rel: &Path) -> Option<PathBuf> {
    let is_doc = |p: &Path| {
        p.is_file()
            && p.extension()
                .and_then(|s| s.to_str())
                .map(|ext| matches!(ext, "josie" | "json" | "josieml"))
                .unwrap_or(false)
    };

    if rel.as_os_str().is_empty() {
        for name in ["index.josieml", "index.josie", "index.json"] {
            let p = root.join(name);
            if is_doc(&p) {
                return Some(p);
            }
        }
        return None;
    }

    let full = root.join(rel);
    if is_doc(&full) {
        return Some(full);
    }

    let mut candidates = vec![
        full.with_extension("josieml"),
        full.with_extension("josie"),
        full.with_extension("json"),
    ];

    if full.is_dir() {
        candidates.push(full.join("index.josieml"));
        candidates.push(full.join("index.josie"));
        candidates.push(full.join("index.json"));
    }

    candidates.into_iter().find(|p| is_doc(p))
}

fn extract_requested_resources(ext: &str, source: &str) -> Option<Vec<String>> {
    if ext == "josieml" {
        let (_, payload) = split_josieml_payload(source)?;
        let json: Value = serde_json::from_str(payload).ok()?;
        return Some(resources_from_json(&json));
    }

    let json: Value = serde_json::from_str(source).ok()?;
    Some(resources_from_json(&json))
}

fn resources_from_json(json: &Value) -> Vec<String> {
    json.get("web")
        .and_then(|v| v.get("resources"))
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str().map(|s| s.to_string()))
                .collect()
        })
        .unwrap_or_default()
}

fn compile_html(
    processors: &[String],
    ext: &str,
    source: &str,
    diagnostics: &mut Vec<Diagnostic>,
) -> Result<String, String> {
    let _use_tailwind = processors.iter().any(|p| p == "tailwind");

    for processor in processors {
        if processor != "tailwind" && processor != "security" && processor != "minify" {
            diagnostics.push(Diagnostic {
                level: DiagnosticLevel::Warn,
                code: "unknown_processor".to_string(),
                message: format!("processor '{processor}' is not implemented; skipped"),
            });
        }
    }

    if ext == "josieml" {
        #[cfg(feature = "tailwind")]
        {
            if _use_tailwind {
                return crate::tailwind::render_josieml_with_runtime(source);
            }
            return crate::tailwind::render_josieml_with_runtime(source);
        }
        #[cfg(not(feature = "tailwind"))]
        {
            return Err(
                "josieml rendering requires 'tailwind' feature in current engine".to_string(),
            );
        }
    }

    let doc = serde_json::from_str::<JosieDocument>(source)
        .map_err(|e| format!("failed to parse josie/json document: {e}"))?;

    #[cfg(feature = "tailwind")]
    {
        if _use_tailwind {
            return Ok(crate::tailwind::render_with_runtime(&doc));
        }
    }

    Ok(render_with_runtime(&doc))
}

fn split_josieml_payload(source: &str) -> Option<(&str, &str)> {
    let marker = "<script type=\"application/josie+json\">";
    let start = source.find(marker)?;
    let payload_start = start + marker.len();
    let rest = &source[payload_start..];
    let end = rest.find("</script>")?;
    let payload = &rest[..end];
    Some((&source[..start], payload.trim()))
}

fn inject_assets(mut html: String, assets: &[AssetRef]) -> String {
    if assets.is_empty() {
        return html;
    }

    let mut head_inserts = String::new();
    let mut body_inserts = String::new();

    for asset in assets {
        match asset.asset_type.as_str() {
            "css" => {
                let integrity = asset
                    .integrity
                    .as_ref()
                    .map(|v| {
                        format!(
                            " integrity=\"{}\" crossorigin=\"anonymous\"",
                            escape_html(v)
                        )
                    })
                    .unwrap_or_default();
                head_inserts.push_str(&format!(
                    "<link rel=\"stylesheet\" href=\"{}\"{} data-josie-resource=\"{}\">",
                    escape_html(&asset.src),
                    integrity,
                    escape_html(&asset.name)
                ));
            }
            _ => {
                let integrity = asset
                    .integrity
                    .as_ref()
                    .map(|v| {
                        format!(
                            " integrity=\"{}\" crossorigin=\"anonymous\"",
                            escape_html(v)
                        )
                    })
                    .unwrap_or_default();
                body_inserts.push_str(&format!(
                    "<script src=\"{}\"{} data-josie-resource=\"{}\"></script>",
                    escape_html(&asset.src),
                    integrity,
                    escape_html(&asset.name)
                ));
            }
        }
    }

    if !head_inserts.is_empty() {
        if let Some(idx) = html.rfind("</head>") {
            html.insert_str(idx, &head_inserts);
        } else {
            html = format!("{}{}", head_inserts, html);
        }
    }

    if !body_inserts.is_empty() {
        if let Some(idx) = html.rfind("</body>") {
            html.insert_str(idx, &body_inserts);
        } else {
            html.push_str(&body_inserts);
        }
    }

    html
}

fn extract_slots(html: &str) -> Vec<TemplateSlot> {
    let mut out = Vec::new();
    let bytes = html.as_bytes();
    let mut i = 0usize;
    while i + 3 < bytes.len() {
        if bytes[i] == b'{' && bytes[i + 1] == b'{' {
            let start = i + 2;
            let mut j = start;
            while j + 1 < bytes.len() {
                if bytes[j] == b'}' && bytes[j + 1] == b'}' {
                    let key = html[start..j].trim().to_string();
                    if !key.is_empty() {
                        out.push(TemplateSlot { position: i, key });
                    }
                    i = j + 2;
                    break;
                }
                j += 1;
            }
        }
        i += 1;
    }
    out
}

fn escape_html(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&#39;")
}

#[cfg(test)]
mod tests {
    use super::*;
    #[cfg(feature = "tailwind")]
    use std::path::PathBuf;

    #[test]
    fn test_slot_extraction() {
        let slots = extract_slots("<h1>{{ title }}</h1><p>{{body}}</p>");
        assert_eq!(slots.len(), 2);
        assert_eq!(slots[0].key, "title");
        assert_eq!(slots[1].key, "body");
    }

    #[test]
    fn test_render_replacement() {
        let view = ViewTemplate {
            id: "x".to_string(),
            ir_version: IR_VERSION.to_string(),
            buffer: b"<h1>{{title}}</h1>".to_vec(),
            slots: vec![TemplateSlot {
                position: 4,
                key: "title".to_string(),
            }],
            assets: Vec::new(),
            diagnostics: Vec::new(),
        };
        let engine = JosieWebEngine::new();
        let output = engine
            .render_view(&view, &serde_json::json!({"title": "Hello"}))
            .expect("render should succeed");
        assert!(output.html.contains("Hello"));
    }

    #[cfg(feature = "tailwind")]
    #[test]
    fn test_css_resource_type_uses_link_even_without_css_extension() {
        use std::time::{SystemTime, UNIX_EPOCH};

        let ts = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock after unix epoch")
            .as_nanos();
        let root =
            std::env::temp_dir().join(format!("josie-font-resource-{}-{}", std::process::id(), ts));
        std::fs::create_dir_all(root.join("default")).expect("create temp root");

        let login = root.join("default").join("login.josieml");
        std::fs::write(
            &login,
            r#"<!DOCTYPE html>
<html><head><title>T</title></head><body><div class="font-montserrat">X</div>
<script type="application/josie+json">
{
  "web": { "resources": ["montserrat-font"] },
  "theme": { "colors": {}, "space": [0], "radius": [0], "font_size": {"base": 16}, "font_family": {"montserrat": "Montserrat, sans-serif"}, "shadows": {}, "gradients": {}, "transitions": {}, "recipes": {}, "variants": {} }
}
</script></body></html>"#,
        )
        .expect("write login template");

        let mut cfg = SystemConfig::minimal(root.clone());
        cfg.groups
            .get_mut("default")
            .expect("default group exists")
            .allowed_resources = vec!["montserrat-font".to_string()];
        cfg.resources.insert(
            "montserrat-font".to_string(),
            ResourceDefinition {
                resource_type: "css".to_string(),
                src: "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;500;600;700&display=swap".to_string(),
                integrity: None,
                deps: Vec::new(),
            },
        );

        let mut engine = JosieWebEngine::new();
        engine.init(cfg).expect("init engine");
        let job = engine
            .match_route("default", "default", "/login")
            .expect("route must resolve");
        let view = engine.compile_view(&job).expect("compile must succeed");
        let output = engine
            .render_view(&view, &serde_json::json!({}))
            .expect("render must succeed");

        assert!(
            output.html.contains(
                "rel=\"stylesheet\" href=\"https://fonts.googleapis.com/css2?family=Montserrat"
            ),
            "css-type allowlisted font should be injected as <link rel=stylesheet>"
        );
    }

    #[cfg(feature = "tailwind")]
    #[test]
    fn test_openobe_login_compiles_and_renders_through_core_contract() {
        let crate_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let root = crate_dir.join("../../examples/openobe");
        assert!(root.is_dir(), "expected examples/openobe directory");

        let mut engine = JosieWebEngine::new();
        let mut cfg = SystemConfig::minimal(root.clone());
        cfg.groups
            .get_mut("default")
            .expect("default group exists")
            .allowed_resources = vec![
            "montserrat-font".to_string(),
            "openobe-helper-js".to_string(),
        ];
        cfg.resources.insert(
            "montserrat-font".to_string(),
            ResourceDefinition {
                resource_type: "css".to_string(),
                src: "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;500;600;700&display=swap".to_string(),
                integrity: None,
                deps: Vec::new(),
            },
        );
        cfg.resources.insert(
            "openobe-helper-js".to_string(),
            ResourceDefinition {
                resource_type: "js".to_string(),
                src: "/openobe-helper.js".to_string(),
                integrity: None,
                deps: Vec::new(),
            },
        );
        engine.init(cfg).expect("engine init must succeed");

        let job = engine
            .match_route("default", "default", "/login")
            .expect("openobe /login route should resolve");
        let view = engine
            .compile_view(&job)
            .expect("openobe /login should compile");
        let output = engine
            .render_view(&view, &serde_json::json!({}))
            .expect("openobe /login should render");

        assert!(
            output.html.contains("OpenOBE"),
            "expected OpenOBE content in rendered HTML"
        );
        assert!(
            output.html.contains("bg-gradient-to-br"),
            "expected Tailwind class footprint in rendered output"
        );
        assert!(
            output.html.contains("/campus-logo.svg"),
            "expected static asset references to remain in output"
        );
    }
}
