//! Tailwind-style renderer for JOSIE web documents.
//!
//! This module orchestrates the end-to-end flow:
//! - expand `style.tw` into `className`
//! - collect only used utility rules
//! - emit compact CSS into `<style data-josie-tw>`
//! - inject runtime script when requested
//!
//! # Architecture
//! - `tw.rs` is the token compiler.
//! - `mod.rs` is the document transformer + CSS assembler + caches.
//!
//! # Why this split matters
//! LLMs and implementers can port token compilation independently from
//! document traversal/runtime integration.
//!
use crate::{JOSIE_RUNTIME_JS, JosieDocument, JosieNode, render as render_web};
use serde_json::Value;
use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::{DefaultHasher, Hash, Hasher};
use std::sync::{Mutex, OnceLock};
mod josieml;
mod tw;
pub use josieml::render_josieml_with_runtime;
use tw::{ParsedTw, parse_tw_tokens, token_css_rule};

pub const JOSIE_NAME: &str = "JOSIE";
pub const JOSIE_EXPANSION: &str = "JSON Omni Safe Interactive Expressions";
pub const CRATE_STATUS: &str = "alpha";
const TW_ENGINE_VERSION: &str = "tw-engine";
const RENDER_CACHE_LIMIT: usize = 256;
const CSS_CACHE_LIMIT: usize = 512;
pub(crate) const TAILWIND_PREFLIGHT_CSS: &str = r#"*,:before,:after{box-sizing:border-box;border:0 solid var(--josie-color-border,#e5e7eb);}html,:host{line-height:1.5;-webkit-text-size-adjust:100%;tab-size:4;font-family:var(--josie-font-sans,ui-sans-serif,system-ui,sans-serif);font-feature-settings:normal;font-variation-settings:normal;-webkit-tap-highlight-color:transparent;}body{margin:0;line-height:inherit;}hr{height:0;color:inherit;border-top-width:1px;}abbr:where([title]){text-decoration:underline dotted;}h1,h2,h3,h4,h5,h6{font-size:inherit;font-weight:inherit;}a{color:inherit;text-decoration:inherit;}b,strong{font-weight:bolder;}code,kbd,samp,pre{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;font-size:1em;}small{font-size:80%;}table{text-indent:0;border-color:inherit;border-collapse:collapse;}button,input,optgroup,select,textarea{font:inherit;letter-spacing:inherit;color:inherit;margin:0;padding:0;background-color:transparent;border-radius:0;}button,select{text-transform:none;}button,input:where([type='button']),input:where([type='reset']),input:where([type='submit']){-webkit-appearance:button;background-color:transparent;background-image:none;}::-webkit-inner-spin-button,::-webkit-outer-spin-button{height:auto;}[type='search']{-webkit-appearance:textfield;outline-offset:-2px;}::-webkit-search-decoration{-webkit-appearance:none;}::-webkit-file-upload-button{-webkit-appearance:button;font:inherit;}summary{display:list-item;}blockquote,dl,dd,h1,h2,h3,h4,h5,h6,hr,figure,p,pre{margin:0;}fieldset{margin:0;padding:0;}legend{padding:0;}ol,ul,menu{list-style:none;margin:0;padding:0;}textarea{resize:vertical;}input::placeholder,textarea::placeholder{opacity:1;color:#9ca3af;}button,[role='button']{cursor:pointer;}:disabled{cursor:default;}img,svg,video,canvas,audio,iframe,embed,object{display:block;vertical-align:middle;}img,video{max-width:100%;height:auto;}[hidden]:where(:not([hidden='until-found'])){display:none;}"#;

static RENDER_CACHE: OnceLock<Mutex<LruCache>> = OnceLock::new();
static CSS_CACHE: OnceLock<Mutex<LruCache>> = OnceLock::new();

#[derive(Debug)]
struct LruCache {
    limit: usize,
    map: HashMap<u64, String>,
    order: VecDeque<u64>,
}

impl LruCache {
    fn new(limit: usize) -> Self {
        Self {
            limit,
            map: HashMap::new(),
            order: VecDeque::new(),
        }
    }

    fn get(&mut self, key: u64) -> Option<String> {
        let value = self.map.get(&key).cloned();
        if value.is_some() {
            self.touch(key);
        }
        value
    }

    fn insert(&mut self, key: u64, value: String) {
        if self.map.contains_key(&key) {
            self.map.insert(key, value);
            self.touch(key);
            return;
        }

        self.map.insert(key, value);
        self.order.push_back(key);
        self.evict_if_needed();
    }

    fn touch(&mut self, key: u64) {
        if let Some(pos) = self.order.iter().position(|existing| *existing == key) {
            self.order.remove(pos);
        }
        self.order.push_back(key);
    }

    fn evict_if_needed(&mut self) {
        while self.map.len() > self.limit {
            let Some(oldest) = self.order.pop_front() else {
                break;
            };
            self.map.remove(&oldest);
        }
    }
}

/// Diagnostics from expanding `style.tw` into class names.
#[derive(Debug, Clone, Default)]
pub struct TwTransformReport {
    pub nodes_with_tw: usize,
    pub tokens_total: usize,
    pub tokens_supported: usize,
    pub tokens_ignored: usize,
    pub unique_tw_patterns: usize,
    pub ignored_tokens: HashMap<String, usize>,
}

/// Coarse payload estimate for utility-CSS approach.
#[derive(Debug, Clone, Default)]
pub struct TwSizeEstimate {
    pub nodes_with_tw: usize,
    pub unique_style_signatures: usize,
    pub estimated_inline_bytes: usize,
    pub estimated_jit_bytes: usize,
    pub estimated_savings_bytes: isize,
    pub runtime_bytes: usize,
}

pub fn project_identity() -> serde_json::Value {
    serde_json::json!({
        "name": JOSIE_NAME,
        "expansion": JOSIE_EXPANSION,
        "crate": "josie-web",
        "status": CRATE_STATUS
    })
}

/// Render a JOSIE document with Tailwind expansion, without JS runtime bootstrap.
pub fn render(doc: &JosieDocument) -> String {
    render_cached(doc, false)
}

/// Render a JOSIE document with Tailwind expansion and runtime bootstrap.
pub fn render_with_runtime(doc: &JosieDocument) -> String {
    render_cached(doc, true)
}

/// Return embedded browser runtime JS used by JOSIE hydration.
pub fn get_runtime_js() -> &'static str {
    JOSIE_RUNTIME_JS
}

/// Generate CSS for only the utilities used by a document.
///
/// This function is deterministic for a fixed document/theme and does not
/// mutate the document.
pub fn get_tailwind_css(doc: &JosieDocument) -> String {
    get_tailwind_css_internal(doc)
}

fn get_tailwind_css_cached(doc: &JosieDocument) -> String {
    let cache_key = css_cache_key(doc);
    let cache = CSS_CACHE.get_or_init(|| Mutex::new(LruCache::new(CSS_CACHE_LIMIT)));

    if let Ok(mut guard) = cache.lock() {
        if let Some(cached) = guard.get(cache_key) {
            return cached;
        }
    }

    let css = get_tailwind_css_internal(doc);

    if let Ok(mut guard) = cache.lock() {
        guard.insert(cache_key, css.clone());
    }

    css
}

fn get_tailwind_css_internal(doc: &JosieDocument) -> String {
    let mut rules = HashMap::new();

    if let Some(body) = &doc.body {
        for node in body {
            collect_used_rules(node, &mut rules);
        }
    }

    let mut sorted_tokens: Vec<String> = rules.keys().cloned().collect();
    sorted_tokens.sort_unstable_by_key(|token| (token_responsive_rank(token), token.clone()));

    let mut css = String::new();
    css.push_str(TAILWIND_PREFLIGHT_CSS);
    css.push('\n');
    css.push_str(&theme_color_var_css(doc));
    for token in sorted_tokens {
        if let Some(rule) = rules.get(&token) {
            css.push_str(rule);
            css.push('\n');
        }
    }

    css
}

fn render_cached(doc: &JosieDocument, with_runtime: bool) -> String {
    let cache_key = render_cache_key(doc, with_runtime);
    let cache = RENDER_CACHE.get_or_init(|| Mutex::new(LruCache::new(RENDER_CACHE_LIMIT)));

    if let Ok(mut guard) = cache.lock() {
        if let Some(cached) = guard.get(cache_key) {
            return cached;
        }
    }

    let (expanded, _) = expand_tw_document(doc);
    let mut base_html = render_web(&expanded);
    if with_runtime {
        base_html = inject_runtime_bootstrap(base_html, &expanded);
    }
    let css = get_tailwind_css_cached(&expanded);
    let rendered = inject_used_tw_css(base_html, &css);

    if let Ok(mut guard) = cache.lock() {
        guard.insert(cache_key, rendered.clone());
    }

    rendered
}

fn inject_runtime_bootstrap(mut html: String, doc: &JosieDocument) -> String {
    let theme_value = doc.theme.clone().unwrap_or_default();
    let state_value = doc
        .state
        .as_ref()
        .map(|s| serde_json::Value::Object(s.client.clone()))
        .unwrap_or_else(|| serde_json::json!({}));
    let bootstrap = serde_json::json!({
        "state": state_value,
        "theme": theme_value
    });
    let state_script = format!(
        "<script>window.__JOSIE__ = {};</script>",
        serde_json::to_string(&bootstrap).unwrap_or_else(|_| "{}".to_string())
    );

    if let Some(pos) = html.find("</body>") {
        let before = &html[..pos];
        let after = &html[pos..];
        html = format!(
            "{}{}<script>{}</script>{}",
            before, state_script, JOSIE_RUNTIME_JS, after
        );
    }

    html
}

fn render_cache_key(doc: &JosieDocument, with_runtime: bool) -> u64 {
    let mut hasher = DefaultHasher::new();
    TW_ENGINE_VERSION.hash(&mut hasher);
    with_runtime.hash(&mut hasher);
    hash_doc_into(doc, &mut hasher);
    hasher.finish()
}

fn css_cache_key(doc: &JosieDocument) -> u64 {
    let mut hasher = DefaultHasher::new();
    TW_ENGINE_VERSION.hash(&mut hasher);
    hash_doc_into(doc, &mut hasher);
    hasher.finish()
}

fn hash_doc_into(doc: &JosieDocument, hasher: &mut DefaultHasher) {
    if let Ok(bytes) = serde_json::to_vec(doc) {
        bytes.hash(hasher);
    } else {
        format!("{:?}", doc).hash(hasher);
    }
}

fn theme_color_var_css(doc: &JosieDocument) -> String {
    let Some(theme) = &doc.theme else {
        return String::new();
    };

    if theme.colors.is_empty() && theme.font_family.is_empty() {
        return String::new();
    }

    let mut keys: Vec<&String> = theme.colors.keys().collect();
    keys.sort_unstable();

    let mut vars = String::new();
    for key in keys {
        if let Some(value) = theme.colors.get(key) {
            let slug = key
                .chars()
                .map(|c| {
                    if c.is_ascii_alphanumeric() || c == '-' || c == '_' {
                        c.to_ascii_lowercase()
                    } else {
                        '-'
                    }
                })
                .collect::<String>();
            vars.push_str(&format!("--josie-color-{}:{};", slug, value));
        }
    }

    let mut font_keys: Vec<&String> = theme.font_family.keys().collect();
    font_keys.sort_unstable();
    for key in font_keys {
        if let Some(value) = theme.font_family.get(key) {
            let slug = key
                .chars()
                .map(|c| {
                    if c.is_ascii_alphanumeric() || c == '-' || c == '_' {
                        c.to_ascii_lowercase()
                    } else {
                        '-'
                    }
                })
                .collect::<String>();
            vars.push_str(&format!("--josie-font-{}:{};", slug, value));
        }
    }

    if vars.is_empty() {
        String::new()
    } else {
        format!(":root{{{}}}\n", vars)
    }
}

fn inject_used_tw_css(mut html: String, css: &str) -> String {
    if css.trim().is_empty() {
        return html;
    }

    if let Some(head_pos) = html.find("</head>") {
        let before = &html[..head_pos];
        let after = &html[head_pos..];
        let style_tag = format!("<style data-josie-tw>\n{}\n</style>", css);
        html = format!("{}{}{}", before, style_tag, after);
    }

    html
}

pub fn expand_tw_document(doc: &JosieDocument) -> (JosieDocument, TwTransformReport) {
    let mut out = doc.clone();
    let mut report = TwTransformReport::default();
    let mut unique_tw = HashSet::new();

    if let Some(body) = out.body.as_mut() {
        for node in body {
            process_node(node, &mut report, &mut unique_tw);
        }
    }

    report.unique_tw_patterns = unique_tw.len();
    (out, report)
}

/// Estimate size impact between inline styles and utility-class JIT CSS.
///
/// The estimate is heuristic and intended for coarse telemetry/optimization,
/// not byte-perfect production accounting.
pub fn estimate_tw_payload(doc: &JosieDocument) -> TwSizeEstimate {
    let mut signatures: HashMap<String, usize> = HashMap::new();
    let mut nodes_with_tw = 0usize;
    let mut inline_bytes = 0usize;

    if let Some(body) = &doc.body {
        for node in body {
            collect_estimates(node, &mut nodes_with_tw, &mut inline_bytes, &mut signatures);
        }
    }

    let mut jit_css_bytes = 0usize;
    let mut idx = 0usize;
    for sig in signatures.keys() {
        let class_name = format!("j{}", idx);
        jit_css_bytes += format!(".{}{{{}}}", class_name, sig).len();
        idx += 1;
    }

    // class="j123" + one leading space.
    let class_attr_bytes = nodes_with_tw * 12;
    let jit_bytes = jit_css_bytes + class_attr_bytes;
    let savings = inline_bytes as isize - jit_bytes as isize;

    TwSizeEstimate {
        nodes_with_tw,
        unique_style_signatures: signatures.len(),
        estimated_inline_bytes: inline_bytes,
        estimated_jit_bytes: jit_bytes,
        estimated_savings_bytes: savings,
        runtime_bytes: JOSIE_RUNTIME_JS.len(),
    }
}

fn collect_estimates(
    node: &JosieNode,
    nodes_with_tw: &mut usize,
    inline_bytes: &mut usize,
    signatures: &mut HashMap<String, usize>,
) {
    if let Some(tw) = extract_tw(node) {
        let parsed = parse_tw_tokens(&tw, &node.node_type);
        let sig = signature_for_estimate(&parsed);
        if !sig.is_empty() {
            *nodes_with_tw += 1;
            // style="...".
            *inline_bytes += sig.len() + 9;
            *signatures.entry(sig).or_insert(0) += 1;
        }
    }

    if let Some(each) = &node.each_item {
        collect_estimates(each, nodes_with_tw, inline_bytes, signatures);
    }

    if let Some(children) = &node.children {
        for child in children {
            collect_estimates(child, nodes_with_tw, inline_bytes, signatures);
        }
    }
}

fn signature_for_estimate(parsed: &ParsedTw) -> String {
    let mut parts: Vec<String> = Vec::new();

    let mut style_keys: Vec<&String> = parsed.styles.keys().collect();
    style_keys.sort_unstable();
    for key in style_keys {
        if let Some(v) = parsed.styles.get(key) {
            parts.push(format!("{}:{};", key, value_to_sig(v)));
        }
    }

    let mut prop_keys: Vec<&String> = parsed.props.keys().collect();
    prop_keys.sort_unstable();
    for key in prop_keys {
        if let Some(v) = parsed.props.get(key) {
            parts.push(format!("prop.{}:{};", key, value_to_sig(v)));
        }
    }

    parts.join("")
}

fn value_to_sig(v: &Value) -> String {
    match v {
        Value::String(s) => s.clone(),
        Value::Number(n) => n.to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Null => "null".to_string(),
        _ => v.to_string(),
    }
}

fn process_node(
    node: &mut JosieNode,
    report: &mut TwTransformReport,
    unique_tw: &mut HashSet<String>,
) {
    if let Some(tw) = extract_tw(node) {
        unique_tw.insert(tw.clone());
        report.nodes_with_tw += 1;

        let parsed = parse_tw_tokens(&tw, &node.node_type);
        report.tokens_total += parsed.total_tokens;
        report.tokens_supported += parsed.supported_tokens;
        report.tokens_ignored += parsed.ignored_tokens.len();
        for token in &parsed.ignored_tokens {
            *report.ignored_tokens.entry(token.clone()).or_insert(0) += 1;
        }

        apply_tw_classes(node, &parsed.applied_tokens);
    }

    if let Some(each) = node.each_item.as_mut() {
        process_node(each, report, unique_tw);
    }

    if let Some(children) = node.children.as_mut() {
        for child in children {
            process_node(child, report, unique_tw);
        }
    }
}

fn apply_tw_classes(node: &mut JosieNode, tw_tokens: &[String]) {
    if tw_tokens.is_empty() {
        if let Some(style) = node.style.as_mut() {
            style.remove("tw");
            if style.is_empty() {
                node.style = None;
            }
        }
        return;
    }

    if node.props.is_none() {
        node.props = Some(HashMap::new());
    }

    if let Some(props) = node.props.as_mut() {
        let existing = props
            .get("className")
            .and_then(Value::as_str)
            .unwrap_or_default();
        let merged = merge_class_names(existing, tw_tokens);
        if !merged.is_empty() {
            props.insert("className".to_string(), Value::String(merged));
        }
    }

    if let Some(style) = node.style.as_mut() {
        style.remove("tw");
        if style.is_empty() {
            node.style = None;
        }
    }
}

fn merge_class_names(existing: &str, tokens: &[String]) -> String {
    let mut seen = HashSet::new();
    let mut out = Vec::new();

    for t in existing.split_whitespace() {
        if !t.is_empty() && seen.insert(t.to_string()) {
            out.push(t.to_string());
        }
    }

    for t in tokens {
        if !t.is_empty() && seen.insert(t.clone()) {
            out.push(t.clone());
        }
    }

    out.join(" ")
}

fn extract_tw(node: &JosieNode) -> Option<String> {
    node.style
        .as_ref()
        .and_then(|s| s.get("tw"))
        .and_then(Value::as_str)
        .map(|s| s.to_string())
}

fn collect_used_rules(node: &JosieNode, out: &mut HashMap<String, String>) {
    if let Some(tw) = extract_tw(node) {
        let parsed = parse_tw_tokens(&tw, &node.node_type);
        for token in parsed.applied_tokens {
            if out.contains_key(&token) {
                continue;
            }
            if let Some(rule) = token_css_rule(&token) {
                out.insert(token, rule);
            }
        }
    }

    if let Some(props) = &node.props {
        if let Some(class_name) = props.get("className").and_then(Value::as_str) {
            for token in class_name.split_whitespace() {
                if out.contains_key(token) {
                    continue;
                }
                if let Some(rule) = token_css_rule(token) {
                    out.insert(token.to_string(), rule);
                }
            }
        }
    }

    if let Some(each) = &node.each_item {
        collect_used_rules(each, out);
    }

    if let Some(children) = &node.children {
        for child in children {
            collect_used_rules(child, out);
        }
    }
}

/// Rank tokens by responsive breakpoint variant for stable cascade order.
///
/// Base utilities rank `0`.
/// `sm/md/lg/xl/2xl` rank progressively higher so responsive rules are emitted
/// after base rules, matching expected utility override behavior.
fn token_responsive_rank(token: &str) -> u8 {
    let mut depth = 0usize;
    let mut part = String::new();
    let mut rank = 0u8;

    let bump_rank = |v: &str| match v {
        "sm" => 1u8,
        "md" => 2u8,
        "lg" => 3u8,
        "xl" => 4u8,
        "2xl" => 5u8,
        _ => 0u8,
    };

    for ch in token.chars() {
        match ch {
            '[' => {
                depth += 1;
                part.push(ch);
            }
            ']' => {
                depth = depth.saturating_sub(1);
                part.push(ch);
            }
            ':' if depth == 0 => {
                rank = rank.max(bump_rank(&part));
                part.clear();
            }
            _ => part.push(ch),
        }
    }

    rank
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{HeadNode, Theme};
    use serde_json::json;

    fn empty_node(node_type: &str) -> JosieNode {
        JosieNode {
            node_type: node_type.to_string(),
            props: None,
            style: None,
            bind: None,
            on: None,
            children: None,
            is_ssr: None,
            client: None,
            source: None,
            each_item: None,
            filter: None,
        }
    }

    #[test]
    fn test_expand_tw_precedence() {
        let mut node = empty_node("Stack");
        node.style = Some(
            [
                (
                    "tw".to_string(),
                    json!("p-4 bg-blue-600 rounded-lg shadow-md text-white"),
                ),
                ("bg".to_string(), json!("danger")),
                ("p".to_string(), json!(2)),
            ]
            .into_iter()
            .collect(),
        );

        let doc = JosieDocument {
            version: Some("1.0".to_string()),
            theme: Some(Theme::default()),
            state: None,
            head: Some(HeadNode {
                title: Some("tw".to_string()),
            }),
            body: Some(vec![node]),
        };

        let (expanded, report) = expand_tw_document(&doc);
        assert_eq!(report.nodes_with_tw, 1);
        assert!(report.tokens_supported >= 4);

        let node = expanded
            .body
            .as_ref()
            .and_then(|b| b.first())
            .expect("node should exist");

        let style = node.style.as_ref().expect("style should exist");
        assert_eq!(style.get("bg"), Some(&json!("danger")));
        assert_eq!(style.get("p"), Some(&json!(2)));
        assert!(style.get("tw").is_none());

        let class_name = node
            .props
            .as_ref()
            .and_then(|p| p.get("className"))
            .and_then(Value::as_str)
            .unwrap_or("");
        assert!(class_name.contains("p-4"));
        assert!(class_name.contains("bg-blue-600"));
        assert!(class_name.contains("rounded-lg"));
        assert!(class_name.contains("text-white"));
    }

    #[test]
    fn test_render_with_runtime_from_tw() {
        let mut node = empty_node("Stack");
        node.style = Some(
            [("tw".to_string(), json!("p-4 bg-blue-600 rounded-lg"))]
                .into_iter()
                .collect(),
        );

        let doc = JosieDocument {
            version: Some("1.0".to_string()),
            theme: Some(Theme::default()),
            state: None,
            head: Some(HeadNode {
                title: Some("tw-render".to_string()),
            }),
            body: Some(vec![node]),
        };

        let html = render_with_runtime(&doc);
        assert!(html.contains("class=\"p-4 bg-blue-600 rounded-lg\""));
        assert!(html.contains(".p-4{padding:1rem;}"));
        assert!(html.contains(".bg-blue-600{background-color:#2563eb;}"));
        assert!(html.contains(".rounded-lg{border-radius:0.5rem;}"));
    }

    #[test]
    fn test_runtime_and_css_export() {
        let mut node = empty_node("Text");
        node.style = Some(
            [("tw".to_string(), json!("text-white"))]
                .into_iter()
                .collect(),
        );

        let doc = JosieDocument {
            version: Some("1.0".to_string()),
            theme: Some(Theme::default()),
            state: None,
            head: Some(HeadNode {
                title: Some("exports".to_string()),
            }),
            body: Some(vec![node]),
        };

        let (expanded, _) = expand_tw_document(&doc);
        let css = get_tailwind_css(&expanded);
        assert!(css.contains(".text-white{color:#ffffff;}"));
        assert!(get_runtime_js().contains("JOSIE Runtime Notes"));
    }

    #[test]
    fn test_variant_and_arbitrary_classes_emit_css() {
        let mut node = empty_node("Stack");
        node.style = Some(
            [(
                "tw".to_string(),
                json!("h-[80vh] left-[40%] lg:px-6 hover:bg-slate-50"),
            )]
            .into_iter()
            .collect(),
        );

        let doc = JosieDocument {
            version: Some("1.0".to_string()),
            theme: Some(Theme::default()),
            state: None,
            head: Some(HeadNode {
                title: Some("variants".to_string()),
            }),
            body: Some(vec![node]),
        };

        let html = render_with_runtime(&doc);
        assert!(html.contains(".h-\\[80vh\\]{height:80vh;}"));
        assert!(html.contains(".left-\\[40\\%\\]{left:40%;}"));
        assert!(html.contains(
            "@media (min-width: 1024px){.lg\\:px-6{padding-left:1.5rem;padding-right:1.5rem;}}"
        ));
        assert!(html.contains(".hover\\:bg-slate-50:hover{background-color:#f8fafc;}"));
    }

    #[test]
    fn test_theme_color_vars_injected_into_tw_css() {
        let mut node = empty_node("Stack");
        node.style = Some(
            [("tw".to_string(), json!("bg-sidebar text-primary"))]
                .into_iter()
                .collect(),
        );

        let mut theme = Theme::default();
        theme
            .colors
            .insert("sidebar".to_string(), "#123456".to_string());
        theme
            .colors
            .insert("primary".to_string(), "#abcdef".to_string());

        let doc = JosieDocument {
            version: Some("1.0".to_string()),
            theme: Some(theme),
            state: None,
            head: Some(HeadNode {
                title: Some("theme-vars".to_string()),
            }),
            body: Some(vec![node]),
        };

        let css = get_tailwind_css(&doc);
        assert!(css.contains(":root{"));
        assert!(css.contains("button,input,optgroup,select,textarea{font:inherit;"));
        assert!(css.contains("--josie-color-sidebar:#123456;"));
        assert!(css.contains("--josie-color-primary:#abcdef;"));
        assert!(css.contains(".bg-sidebar{background-color:var(--josie-color-sidebar,sidebar);}"));
        assert!(css.contains(".text-primary{color:var(--josie-color-primary,primary);}"));
    }

    #[test]
    fn test_footer_pattern_and_gradient_rules_do_not_conflict() {
        let mut node = empty_node("Stack");
        node.style = Some(
            [(
                "tw".to_string(),
                json!(
                    "bg-[url('/footer.svg')] bg-repeat-x bg-bottom bg-gradient-to-br from-sidebar to-sidebar-gradient"
                ),
            )]
            .into_iter()
            .collect(),
        );

        let mut theme = Theme::default();
        theme
            .colors
            .insert("sidebar".to_string(), "#1d75ca".to_string());
        theme
            .colors
            .insert("sidebar-gradient".to_string(), "#2387cb".to_string());

        let doc = JosieDocument {
            version: Some("1.0".to_string()),
            theme: Some(theme),
            state: None,
            head: Some(HeadNode {
                title: Some("bg-check".to_string()),
            }),
            body: Some(vec![node]),
        };

        let css = get_tailwind_css(&doc);
        assert!(css.contains("background-image:url('/footer.svg');"));
        assert!(!css.contains("background:url('/footer.svg');"));
        assert!(css.contains(".bg-repeat-x{background-repeat:repeat-x;}"));
        assert!(css.contains(".bg-bottom{background-position:bottom;}"));
        assert!(css.contains(".bg-gradient-to-br{background-image:linear-gradient(to bottom right,var(--tw-gradient-from),var(--tw-gradient-to));}"));
        assert!(css.contains(".from-sidebar{--tw-gradient-from:var(--josie-color-sidebar,sidebar);--tw-gradient-to:transparent;}"));
        assert!(css.contains(".to-sidebar-gradient{--tw-gradient-to:var(--josie-color-sidebar-gradient,sidebar-gradient);}"));
        assert!(!css.contains("color-mix("));
    }

    #[test]
    fn test_structural_tokens_are_not_misparsed_as_semantic_colors() {
        let mut node = empty_node("Stack");
        node.style = Some(
            [("tw".to_string(), json!("bg-repeat-x bg-bottom text-center"))]
                .into_iter()
                .collect(),
        );

        let doc = JosieDocument {
            version: Some("1.0".to_string()),
            theme: Some(Theme::default()),
            state: None,
            head: Some(HeadNode {
                title: Some("structural-token-check".to_string()),
            }),
            body: Some(vec![node]),
        };

        let css = get_tailwind_css(&doc);
        assert!(css.contains(".bg-repeat-x{background-repeat:repeat-x;}"));
        assert!(css.contains(".bg-bottom{background-position:bottom;}"));
        assert!(css.contains(".text-center{text-align:center;}"));
        assert!(!css.contains("var(--josie-color-repeat-x)"));
        assert!(!css.contains("var(--josie-color-bottom)"));
        assert!(!css.contains("var(--josie-color-center)"));
    }

    #[test]
    fn test_negative_inset_utilities_emit_expected_css() {
        let mut node = empty_node("Stack");
        node.style = Some(
            [("tw".to_string(), json!("absolute -top-4 left-4"))]
                .into_iter()
                .collect(),
        );

        let doc = JosieDocument {
            version: Some("1.0".to_string()),
            theme: Some(Theme::default()),
            state: None,
            head: Some(HeadNode {
                title: Some("negative-inset".to_string()),
            }),
            body: Some(vec![node]),
        };

        let css = get_tailwind_css(&doc);
        assert!(css.contains(".absolute{position:absolute;}"));
        assert!(css.contains(".-top-4{top:-1rem;}"));
        assert!(css.contains(".left-4{left:1rem;}"));
    }

    #[test]
    fn test_negative_spacing_translate_and_zindex_emit_expected_css() {
        let mut node = empty_node("Stack");
        node.style = Some(
            [("tw".to_string(), json!("-mt-4 -mx-2 -translate-y-1 -z-10"))]
                .into_iter()
                .collect(),
        );

        let doc = JosieDocument {
            version: Some("1.0".to_string()),
            theme: Some(Theme::default()),
            state: None,
            head: Some(HeadNode {
                title: Some("negative-family-check".to_string()),
            }),
            body: Some(vec![node]),
        };

        let css = get_tailwind_css(&doc);
        assert!(css.contains(".-mt-4{margin-top:-1rem;}"));
        assert!(css.contains(".-mx-2{margin-left:-0.5rem;margin-right:-0.5rem;}"));
        assert!(css.contains(".-translate-y-1{transform:translateY(-0.25rem);}"));
        assert!(css.contains(".-z-10{z-index:-10;}"));
    }

    #[test]
    fn test_rounded_sm_and_alpha_bg_emit_expected_css() {
        let mut node = empty_node("Stack");
        node.style = Some(
            [("tw".to_string(), json!("rounded-sm bg-white/90"))]
                .into_iter()
                .collect(),
        );

        let doc = JosieDocument {
            version: Some("1.0".to_string()),
            theme: Some(Theme::default()),
            state: None,
            head: Some(HeadNode {
                title: Some("rounded-sm-check".to_string()),
            }),
            body: Some(vec![node]),
        };

        let css = get_tailwind_css(&doc);
        assert!(css.contains(".rounded-sm{border-radius:0.25rem;}"));
        assert!(css.contains(".bg-white\\/90{background-color:rgba(255, 255, 255, 0.900);}"));
    }

    #[test]
    fn test_semantic_color_tokens_fallback_to_css_color_keyword() {
        let mut node = empty_node("Stack");
        node.style = Some(
            [("tw".to_string(), json!("bg-orange"))]
                .into_iter()
                .collect(),
        );

        let doc = JosieDocument {
            version: Some("1.0".to_string()),
            theme: Some(Theme::default()),
            state: None,
            head: Some(HeadNode {
                title: Some("semantic-color-fallback".to_string()),
            }),
            body: Some(vec![node]),
        };

        let css = get_tailwind_css(&doc);
        assert!(css.contains(".bg-orange{background-color:var(--josie-color-orange,orange);}"));
    }

    #[test]
    fn test_responsive_variant_rules_emitted_after_base_rules() {
        let mut node = empty_node("Text");
        node.style = Some(
            [("tw".to_string(), json!("text-center lg:text-left"))]
                .into_iter()
                .collect(),
        );
        node.props = Some(
            [("children".to_string(), json!("Hello"))]
                .into_iter()
                .collect(),
        );

        let doc = JosieDocument {
            version: Some("1.0".to_string()),
            theme: Some(Theme::default()),
            state: None,
            head: Some(HeadNode {
                title: Some("responsive-order".to_string()),
            }),
            body: Some(vec![node]),
        };

        let css = get_tailwind_css(&doc);
        let base_idx = css
            .find(".text-center{text-align:center;}")
            .expect("base class css should exist");
        let lg_idx = css
            .find("@media (min-width: 1024px){.lg\\:text-left{text-align:left;}}")
            .expect("responsive class css should exist");
        assert!(lg_idx > base_idx);
    }

    #[test]
    fn test_estimate_benefits_for_repeated_tokens() {
        let mut cards = Vec::new();
        for _ in 0..40 {
            let mut card = empty_node("Stack");
            card.props = Some(
                [
                    ("direction".to_string(), json!("column")),
                    ("gap".to_string(), json!(2)),
                ]
                .into_iter()
                .collect(),
            );
            card.style = Some(
                [(
                    "tw".to_string(),
                    json!("p-4 bg-white rounded-lg shadow-md border border-slate-200"),
                )]
                .into_iter()
                .collect(),
            );
            card.children = Some(vec![JosieNode::text("Card")]);
            cards.push(card);
        }

        let mut root = empty_node("Stack");
        root.props = Some(
            [
                ("direction".to_string(), json!("column")),
                ("gap".to_string(), json!(2)),
            ]
            .into_iter()
            .collect(),
        );
        root.children = Some(cards);

        let doc = JosieDocument {
            version: Some("1.0".to_string()),
            theme: Some(Theme::default()),
            state: None,
            head: Some(HeadNode {
                title: Some("estimate".to_string()),
            }),
            body: Some(vec![root]),
        };

        let est = estimate_tw_payload(&doc);
        eprintln!(
            "nodes_with_tw={} unique_style_signatures={} inline_bytes={} jit_bytes={} savings={} runtime_bytes={}",
            est.nodes_with_tw,
            est.unique_style_signatures,
            est.estimated_inline_bytes,
            est.estimated_jit_bytes,
            est.estimated_savings_bytes,
            est.runtime_bytes
        );
        assert_eq!(est.nodes_with_tw, 40);
        assert!(est.estimated_savings_bytes > 0);
    }
}
