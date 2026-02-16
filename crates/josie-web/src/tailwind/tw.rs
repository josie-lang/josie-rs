//! Tailwind-like token compiler used by `josie-web` Tailwind feature.
//!
//! # Purpose
//! This module compiles utility-class tokens (for example `p-4`, `lg:text-left`,
//! `bg-[url('/footer.svg')]`) into concrete CSS rules.
//!
//! # Design Constraints
//! - Deterministic output for a given token.
//! - Safe handling for arbitrary values (`[...]`) without allowing CSS injection.
//! - Fast repeated lookups via an internal LRU cache.
//! - Behavior parity with common Tailwind utility semantics for supported tokens.
//!
//! # High-level Pipeline
//! 1. `parse_tw_tokens` splits a `tw` string into tokens.
//! 2. Each token is compiled through `token_css_rule`.
//! 3. `token_css_rule`:
//!    - splits variants (`lg:hover:text-left`),
//!    - resolves utility declarations (`utility_rule`),
//!    - wraps selectors in pseudo/media variants.
//! 4. Unsupported tokens are retained as pass-through class names, but no CSS is emitted.
//!
//! # Porting Guide (for other languages)
//! Reimplement this module in three phases:
//! 1. **Tokenizer + variant splitter** that respects bracket nesting depth.
//! 2. **Utility resolver** for `prefix-*` and exact-token mappings.
//! 3. **Rule composer** that applies pseudo/media wrappers in deterministic order.
//!
//! Preserve these behavior rules:
//! - Variant separators (`:`) are ignored inside `[...]`.
//! - Structural tokens (e.g. `bg-repeat-x`) must be resolved before generic
//!   semantic-color fallback (`bg-*` -> color).
//! - For arbitrary `bg-[url(...)]`, emit `background-image` rather than `background`
//!   shorthand to avoid resetting repeat/position.
//! - Responsive variants should compile to `@media` wrappers; base-vs-responsive
//!   ordering is handled by the caller (`tailwind/mod.rs`).
//!
use serde_json::Value;
use std::collections::{HashMap, VecDeque};
use std::sync::{Mutex, OnceLock};

/// Parsed representation of a `tw` field.
///
/// This struct keeps both transform metadata (supported/ignored tokens)
/// and normalized tokens that should be kept as `className`.
#[derive(Debug, Clone, Default)]
pub(crate) struct ParsedTw {
    pub(crate) styles: HashMap<String, Value>,
    pub(crate) props: HashMap<String, Value>,
    pub(crate) total_tokens: usize,
    pub(crate) supported_tokens: usize,
    pub(crate) ignored_tokens: Vec<String>,
    pub(crate) applied_tokens: Vec<String>,
}

#[derive(Debug, Clone)]
struct UtilityRule {
    selector: String,
    declarations: String,
    prelude: Option<String>,
}

const TOKEN_RULE_CACHE_LIMIT: usize = 4096;
static TOKEN_RULE_CACHE: OnceLock<Mutex<TokenRuleCache>> = OnceLock::new();

#[derive(Debug)]
struct TokenRuleCache {
    limit: usize,
    map: HashMap<String, Option<String>>,
    order: VecDeque<String>,
}

impl TokenRuleCache {
    fn new(limit: usize) -> Self {
        Self {
            limit,
            map: HashMap::new(),
            order: VecDeque::new(),
        }
    }

    fn get(&mut self, key: &str) -> Option<Option<String>> {
        let value = self.map.get(key).cloned();
        if value.is_some() {
            self.touch(key);
        }
        value
    }

    fn insert(&mut self, key: String, value: Option<String>) {
        if self.map.contains_key(&key) {
            self.map.insert(key.clone(), value);
            self.touch(&key);
            return;
        }

        self.map.insert(key.clone(), value);
        self.order.push_back(key);
        self.evict_if_needed();
    }

    fn touch(&mut self, key: &str) {
        if let Some(pos) = self.order.iter().position(|existing| existing == key) {
            self.order.remove(pos);
        }
        self.order.push_back(key.to_string());
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

/// Compile a single utility token into CSS.
///
/// Returns:
/// - `Some(css_rule)` if the token is recognized and can emit CSS.
/// - `None` if unsupported.
///
/// Output is cached by token string for fast repeated renders.
pub(crate) fn token_css_rule(token: &str) -> Option<String> {
    let cache =
        TOKEN_RULE_CACHE.get_or_init(|| Mutex::new(TokenRuleCache::new(TOKEN_RULE_CACHE_LIMIT)));

    if let Ok(mut guard) = cache.lock() {
        if let Some(cached) = guard.get(token) {
            return cached;
        }
    }

    let computed = token_css_rule_uncached(token);

    if let Ok(mut guard) = cache.lock() {
        guard.insert(token.to_string(), computed.clone());
    }

    computed
}

/// Uncached compiler path for a single token.
fn token_css_rule_uncached(token: &str) -> Option<String> {
    let (variants, raw_utility) = split_variants(token)?;
    let selector = format!(".{}", escape_class_selector(token));

    let (important, utility) = if let Some(rest) = raw_utility.strip_prefix('!') {
        (true, rest)
    } else {
        (false, raw_utility.as_str())
    };

    let mut rule = utility_rule(utility, &selector, important)?;

    let mut medias = Vec::new();
    for variant in &variants {
        if let Some(media) = variant_media_query(variant) {
            medias.push(media);
            continue;
        }

        if variant == "peer-disabled" {
            rule.selector = format!(".peer:disabled ~ {}", rule.selector);
            continue;
        }

        if let Some(pseudo) = variant_pseudo(variant) {
            rule.selector.push_str(pseudo);
            continue;
        }

        return None;
    }

    let mut out = format!("{}{{{}}}", rule.selector, rule.declarations);
    for media in medias.into_iter().rev() {
        out = format!("@media {}{{{}}}", media, out);
    }

    if let Some(prelude) = rule.prelude.take() {
        return Some(format!("{}\n{}", prelude, out));
    }

    Some(out)
}

/// Parse and classify a whitespace-separated utility string.
///
/// The parser does not evaluate node type today (`_node_type`), but the
/// parameter is intentionally kept for future element-aware rules.
pub(crate) fn parse_tw_tokens(tw: &str, _node_type: &str) -> ParsedTw {
    let mut out = ParsedTw::default();

    for token in tw.split_whitespace() {
        if token.is_empty() {
            continue;
        }

        out.total_tokens += 1;

        if token_css_rule(token).is_some() || is_custom_passthrough_token(token) {
            out.supported_tokens += 1;
            out.applied_tokens.push(token.to_string());
        } else {
            out.ignored_tokens.push(token.to_string());
        }
    }

    if !out.applied_tokens.is_empty() {
        out.styles.insert(
            "tw".to_string(),
            Value::String(out.applied_tokens.join(" ")),
        );
    }

    out
}

fn is_custom_passthrough_token(token: &str) -> bool {
    if token.is_empty() || token.contains(char::is_whitespace) {
        return false;
    }
    token.chars().all(|c| {
        c.is_ascii_alphanumeric()
            || matches!(c, '-' | '_' | ':' | '/' | '[' | ']' | '.' | '%' | '!')
    })
}

/// Split a token into `(variants, utility)`.
///
/// Example:
/// - `lg:hover:text-left` -> `["lg", "hover"]`, `"text-left"`
///
/// Important: `:` separators inside bracket expressions are ignored:
/// `bg-[url('/x:y.svg')]` remains a single utility segment.
fn split_variants(token: &str) -> Option<(Vec<String>, String)> {
    let mut parts: Vec<String> = Vec::new();
    let mut buf = String::new();
    let mut depth = 0usize;

    for ch in token.chars() {
        match ch {
            '[' => {
                depth += 1;
                buf.push(ch);
            }
            ']' => {
                depth = depth.saturating_sub(1);
                buf.push(ch);
            }
            ':' if depth == 0 => {
                if buf.is_empty() {
                    return None;
                }
                parts.push(buf.clone());
                buf.clear();
            }
            _ => buf.push(ch),
        }
    }

    if buf.is_empty() {
        return None;
    }

    parts.push(buf);
    if parts.is_empty() {
        return None;
    }

    let utility = parts.pop()?;
    Some((parts, utility))
}

/// Map responsive variant names to media query strings.
fn variant_media_query(v: &str) -> Option<&'static str> {
    match v {
        "sm" => Some("(min-width: 640px)"),
        "md" => Some("(min-width: 768px)"),
        "lg" => Some("(min-width: 1024px)"),
        "xl" => Some("(min-width: 1280px)"),
        "2xl" => Some("(min-width: 1536px)"),
        _ => None,
    }
}

/// Map pseudo-state variants to selector suffixes.
fn variant_pseudo(v: &str) -> Option<&'static str> {
    match v {
        "hover" => Some(":hover"),
        "focus" => Some(":focus"),
        "focus-visible" => Some(":focus-visible"),
        "focus-within" => Some(":focus-within"),
        "active" => Some(":active"),
        "disabled" => Some(":disabled"),
        "last" => Some(":last-child"),
        _ => None,
    }
}

/// Resolve one utility segment (without variants) to a CSS rule.
///
/// Resolution strategy:
/// 1. Prefix handlers (for example `p-`, `text-`, `bg-`).
/// 2. Exact-token handlers (for example `flex`, `bg-repeat-x`).
/// 3. Fallback to `None`.
///
/// Keep fallback ordering deliberate. For example, `bg-repeat-x` must not be
/// consumed by generic `bg-*` color fallback.
fn utility_rule(utility: &str, base_selector: &str, important: bool) -> Option<UtilityRule> {
    if let Some(v) = utility.strip_prefix("brightness-") {
        let amount = v.parse::<f64>().ok()?;
        return Some(simple_rule(
            base_selector,
            &format!("filter:brightness({:.3});", amount / 100.0),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("line-clamp-") {
        let lines = v.parse::<u32>().ok()?;
        if lines == 0 {
            return None;
        }
        return Some(simple_rule(
            base_selector,
            &format!(
                "overflow:hidden;display:-webkit-box;-webkit-box-orient:vertical;-webkit-line-clamp:{};",
                lines
            ),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("space-y-") {
        let value = spacing_value(v)?;
        return Some(UtilityRule {
            selector: format!("{} > :not([hidden]) ~ :not([hidden])", base_selector),
            declarations: maybe_important(&format!("margin-top:{};", value), important),
            prelude: None,
        });
    }

    if let Some(v) = utility.strip_prefix("space-x-") {
        let value = spacing_value(v)?;
        return Some(UtilityRule {
            selector: format!("{} > :not([hidden]) ~ :not([hidden])", base_selector),
            declarations: maybe_important(&format!("margin-left:{};", value), important),
            prelude: None,
        });
    }

    if let Some(v) = utility.strip_prefix("grid-cols-") {
        let n = v.parse::<u32>().ok()?;
        if n == 0 {
            return None;
        }

        return Some(simple_rule(
            base_selector,
            &format!("grid-template-columns:repeat({}, minmax(0, 1fr));", n),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("col-span-") {
        let n = v.parse::<u32>().ok()?;
        if n == 0 {
            return None;
        }

        return Some(simple_rule(
            base_selector,
            &format!("grid-column:span {} / span {};", n, n),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("w-") {
        let value = size_value(v, SizeAxis::Width)?;
        return Some(simple_rule(
            base_selector,
            &format!("width:{};", value),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("h-") {
        let value = size_value(v, SizeAxis::Height)?;
        return Some(simple_rule(
            base_selector,
            &format!("height:{};", value),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("min-w-") {
        let value = minmax_size_value(v, SizeAxis::Width)?;
        return Some(simple_rule(
            base_selector,
            &format!("min-width:{};", value),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("max-w-") {
        let value = max_width_value(v)?;
        return Some(simple_rule(
            base_selector,
            &format!("max-width:{};", value),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("min-h-") {
        let value = minmax_size_value(v, SizeAxis::Height)?;
        return Some(simple_rule(
            base_selector,
            &format!("min-height:{};", value),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("max-h-") {
        let value = minmax_size_value(v, SizeAxis::Height)?;
        return Some(simple_rule(
            base_selector,
            &format!("max-height:{};", value),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("size-") {
        let value = size_value(v, SizeAxis::Width)?;
        return Some(simple_rule(
            base_selector,
            &format!("width:{};height:{};", value, value),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("translate-x-") {
        let value = inset_value(v)?;
        return Some(simple_rule(
            base_selector,
            &format!("transform:translateX({});", value),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("-translate-x-") {
        let value = inset_value(v)?;
        let neg = negate_css_value(&value)?;
        return Some(simple_rule(
            base_selector,
            &format!("transform:translateX({});", neg),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("translate-y-") {
        let value = inset_value(v)?;
        return Some(simple_rule(
            base_selector,
            &format!("transform:translateY({});", value),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("-translate-y-") {
        let value = inset_value(v)?;
        let neg = negate_css_value(&value)?;
        return Some(simple_rule(
            base_selector,
            &format!("transform:translateY({});", neg),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("-m-") {
        let value = spacing_value(v)?;
        let neg = negate_css_value(&value)?;
        return Some(simple_rule(
            base_selector,
            &format!("margin:{};", neg),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("p-") {
        let value = spacing_value(v)?;
        return Some(simple_rule(
            base_selector,
            &format!("padding:{};", value),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("px-") {
        let value = spacing_value(v)?;
        return Some(simple_rule(
            base_selector,
            &format!("padding-left:{};padding-right:{};", value, value),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("py-") {
        let value = spacing_value(v)?;
        return Some(simple_rule(
            base_selector,
            &format!("padding-top:{};padding-bottom:{};", value, value),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("pt-") {
        let value = spacing_value(v)?;
        return Some(simple_rule(
            base_selector,
            &format!("padding-top:{};", value),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("pr-") {
        let value = spacing_value(v)?;
        return Some(simple_rule(
            base_selector,
            &format!("padding-right:{};", value),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("pb-") {
        let value = spacing_value(v)?;
        return Some(simple_rule(
            base_selector,
            &format!("padding-bottom:{};", value),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("pl-") {
        let value = spacing_value(v)?;
        return Some(simple_rule(
            base_selector,
            &format!("padding-left:{};", value),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("m-") {
        let value = spacing_value(v)?;
        return Some(simple_rule(
            base_selector,
            &format!("margin:{};", value),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("-mx-") {
        let value = spacing_value(v)?;
        let neg = negate_css_value(&value)?;
        return Some(simple_rule(
            base_selector,
            &format!("margin-left:{};margin-right:{};", neg, neg),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("mx-") {
        if v == "auto" {
            return Some(simple_rule(
                base_selector,
                "margin-left:auto;margin-right:auto;",
                important,
            ));
        }
        let value = spacing_value(v)?;
        return Some(simple_rule(
            base_selector,
            &format!("margin-left:{};margin-right:{};", value, value),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("-my-") {
        let value = spacing_value(v)?;
        let neg = negate_css_value(&value)?;
        return Some(simple_rule(
            base_selector,
            &format!("margin-top:{};margin-bottom:{};", neg, neg),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("my-") {
        if v == "auto" {
            return Some(simple_rule(
                base_selector,
                "margin-top:auto;margin-bottom:auto;",
                important,
            ));
        }
        let value = spacing_value(v)?;
        return Some(simple_rule(
            base_selector,
            &format!("margin-top:{};margin-bottom:{};", value, value),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("-mt-") {
        let value = spacing_value(v)?;
        let neg = negate_css_value(&value)?;
        return Some(simple_rule(
            base_selector,
            &format!("margin-top:{};", neg),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("mt-") {
        if v == "auto" {
            return Some(simple_rule(base_selector, "margin-top:auto;", important));
        }
        let value = spacing_value(v)?;
        return Some(simple_rule(
            base_selector,
            &format!("margin-top:{};", value),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("-mr-") {
        let value = spacing_value(v)?;
        let neg = negate_css_value(&value)?;
        return Some(simple_rule(
            base_selector,
            &format!("margin-right:{};", neg),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("mr-") {
        if v == "auto" {
            return Some(simple_rule(base_selector, "margin-right:auto;", important));
        }
        let value = spacing_value(v)?;
        return Some(simple_rule(
            base_selector,
            &format!("margin-right:{};", value),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("-mb-") {
        let value = spacing_value(v)?;
        let neg = negate_css_value(&value)?;
        return Some(simple_rule(
            base_selector,
            &format!("margin-bottom:{};", neg),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("mb-") {
        if v == "auto" {
            return Some(simple_rule(base_selector, "margin-bottom:auto;", important));
        }
        let value = spacing_value(v)?;
        return Some(simple_rule(
            base_selector,
            &format!("margin-bottom:{};", value),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("-ml-") {
        let value = spacing_value(v)?;
        let neg = negate_css_value(&value)?;
        return Some(simple_rule(
            base_selector,
            &format!("margin-left:{};", neg),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("ml-") {
        if v == "auto" {
            return Some(simple_rule(base_selector, "margin-left:auto;", important));
        }
        let value = spacing_value(v)?;
        return Some(simple_rule(
            base_selector,
            &format!("margin-left:{};", value),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("gap-") {
        let value = spacing_value(v)?;
        return Some(simple_rule(
            base_selector,
            &format!("gap:{};", value),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("-top-") {
        let value = inset_value(v)?;
        let neg = negate_css_value(&value)?;
        return Some(simple_rule(
            base_selector,
            &format!("top:{};", neg),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("top-") {
        let value = inset_value(v)?;
        return Some(simple_rule(
            base_selector,
            &format!("top:{};", value),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("-right-") {
        let value = inset_value(v)?;
        let neg = negate_css_value(&value)?;
        return Some(simple_rule(
            base_selector,
            &format!("right:{};", neg),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("right-") {
        let value = inset_value(v)?;
        return Some(simple_rule(
            base_selector,
            &format!("right:{};", value),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("-bottom-") {
        let value = inset_value(v)?;
        let neg = negate_css_value(&value)?;
        return Some(simple_rule(
            base_selector,
            &format!("bottom:{};", neg),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("bottom-") {
        let value = inset_value(v)?;
        return Some(simple_rule(
            base_selector,
            &format!("bottom:{};", value),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("-left-") {
        let value = inset_value(v)?;
        let neg = negate_css_value(&value)?;
        return Some(simple_rule(
            base_selector,
            &format!("left:{};", neg),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("left-") {
        let value = inset_value(v)?;
        return Some(simple_rule(
            base_selector,
            &format!("left:{};", value),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("-inset-") {
        let value = inset_value(v)?;
        let neg = negate_css_value(&value)?;
        return Some(simple_rule(
            base_selector,
            &format!("inset:{};", neg),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("inset-") {
        let value = inset_value(v)?;
        return Some(simple_rule(
            base_selector,
            &format!("inset:{};", value),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("-z-") {
        if let Some(raw) = arbitrary_value(v) {
            let neg = negate_css_value(&raw)?;
            return Some(simple_rule(
                base_selector,
                &format!("z-index:{};", neg),
                important,
            ));
        }
        let z = v.parse::<i64>().ok()?;
        return Some(simple_rule(
            base_selector,
            &format!("z-index:{};", -z),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("z-") {
        if v == "auto" {
            return Some(simple_rule(base_selector, "z-index:auto;", important));
        }
        if let Some(raw) = arbitrary_value(v) {
            return Some(simple_rule(
                base_selector,
                &format!("z-index:{};", raw),
                important,
            ));
        }
        let z = v.parse::<i64>().ok()?;
        return Some(simple_rule(
            base_selector,
            &format!("z-index:{};", z),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("opacity-") {
        let num = v.parse::<u32>().ok()?.min(100);
        let alpha = (num as f64) / 100.0;
        return Some(simple_rule(
            base_selector,
            &format!("opacity:{:.3};", alpha),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("duration-") {
        let ms = v.parse::<u32>().ok()?;
        return Some(simple_rule(
            base_selector,
            &format!("transition-duration:{}ms;", ms),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("text-") {
        if !matches!(v, "left" | "center" | "right" | "justify") {
            if let Some(raw) = arbitrary_value(v) {
                if is_size_like(&raw) {
                    return Some(simple_rule(
                        base_selector,
                        &format!("font-size:{};", raw),
                        important,
                    ));
                }
            }
            if let Some(size) = text_size_value(v) {
                return Some(simple_rule(
                    base_selector,
                    &format!("font-size:{};", size),
                    important,
                ));
            }

            if let Some(color) = color_value(v) {
                return Some(simple_rule(
                    base_selector,
                    &format!("color:{};", color),
                    important,
                ));
            }
        }
    }

    if let Some(v) = utility.strip_prefix("font-") {
        if !matches!(v, "normal" | "medium" | "semibold" | "bold") {
            if v == "mono" {
                return Some(simple_rule(
                    base_selector,
                    "font-family:ui-monospace,SFMono-Regular,Menlo,monospace;",
                    important,
                ));
            }
            if v == "serif" {
                return Some(simple_rule(
                    base_selector,
                    "font-family:ui-serif,Georgia,Cambria,serif;",
                    important,
                ));
            }
            let slug = v
                .chars()
                .map(|c| {
                    if c.is_ascii_alphanumeric() || c == '-' || c == '_' {
                        c.to_ascii_lowercase()
                    } else {
                        '-'
                    }
                })
                .collect::<String>();
            return Some(simple_rule(
                base_selector,
                &format!("font-family:var(--josie-font-{}, '{}');", slug, v),
                important,
            ));
        }
    }

    if let Some(v) = utility.strip_prefix("bg-") {
        if !matches!(
            v,
            "center"
                | "bottom"
                | "repeat"
                | "repeat-x"
                | "repeat-y"
                | "no-repeat"
                | "cover"
                | "contain"
                | "gradient-to-r"
                | "gradient-to-b"
                | "gradient-to-br"
        ) {
            if let Some(bg) = background_value(v) {
                let decl = background_declaration(&bg);
                return Some(simple_rule(base_selector, &decl, important));
            }

            if let Some(color) = color_value(v) {
                return Some(simple_rule(
                    base_selector,
                    &format!("background-color:{};", color),
                    important,
                ));
            }
        }
    }

    if let Some(v) = utility.strip_prefix("from-") {
        if let Some(color) = color_value(v) {
            return Some(simple_rule(
                base_selector,
                &format!("--tw-gradient-from:{};--tw-gradient-to:transparent;", color),
                important,
            ));
        }
    }

    if let Some(v) = utility.strip_prefix("to-") {
        if let Some(color) = color_value(v) {
            return Some(simple_rule(
                base_selector,
                &format!("--tw-gradient-to:{};", color),
                important,
            ));
        }
    }

    if let Some(v) = utility.strip_prefix("via-") {
        if let Some(color) = color_value(v) {
            return Some(simple_rule(
                base_selector,
                &format!("--tw-gradient-via:{};", color),
                important,
            ));
        }
    }

    if let Some(v) = utility.strip_prefix("border-b-") {
        let color = color_value(v)?;
        return Some(simple_rule(
            base_selector,
            &format!("border-bottom-color:{};", color),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("border-t-") {
        let color = color_value(v)?;
        return Some(simple_rule(
            base_selector,
            &format!("border-top-color:{};", color),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("border-l-") {
        let color = color_value(v)?;
        return Some(simple_rule(
            base_selector,
            &format!("border-left-color:{};", color),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("border-r-") {
        let color = color_value(v)?;
        return Some(simple_rule(
            base_selector,
            &format!("border-right-color:{};", color),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("border-") {
        if let Some(decl) = border_rule(v) {
            return Some(simple_rule(base_selector, &decl, important));
        }
    }

    if let Some(v) = utility.strip_prefix("outline-") {
        if let Some(decl) = outline_rule(v) {
            return Some(simple_rule(base_selector, &decl, important));
        }
    }

    if let Some(v) = utility.strip_prefix("ring-") {
        if let Some(decl) = ring_rule(v) {
            return Some(simple_rule(base_selector, &decl, important));
        }
    }

    if let Some(v) = utility.strip_prefix("rounded-") {
        if v == "none" {
            return Some(simple_rule(base_selector, "border-radius:0;", important));
        }
        if v == "xs" {
            return Some(simple_rule(
                base_selector,
                "border-radius:0.125rem;",
                important,
            ));
        }
        if v == "sm" {
            return Some(simple_rule(
                base_selector,
                "border-radius:0.25rem;",
                important,
            ));
        }
        if v == "md" {
            return Some(simple_rule(
                base_selector,
                "border-radius:0.375rem;",
                important,
            ));
        }
        if v == "lg" {
            return Some(simple_rule(
                base_selector,
                "border-radius:0.5rem;",
                important,
            ));
        }
        if v == "xl" {
            return Some(simple_rule(
                base_selector,
                "border-radius:0.75rem;",
                important,
            ));
        }
        if v == "2xl" {
            return Some(simple_rule(base_selector, "border-radius:1rem;", important));
        }
        if v == "3xl" {
            return Some(simple_rule(
                base_selector,
                "border-radius:1.5rem;",
                important,
            ));
        }
        if v == "4xl" {
            return Some(simple_rule(base_selector, "border-radius:2rem;", important));
        }
        if let Some(raw) = arbitrary_value(v) {
            return Some(simple_rule(
                base_selector,
                &format!("border-radius:{};", raw),
                important,
            ));
        }
    }

    if let Some(v) = utility.strip_prefix("order-") {
        let order = v.parse::<i32>().ok()?;
        return Some(simple_rule(
            base_selector,
            &format!("order:{};", order),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("shrink-") {
        let shrink = v.parse::<i32>().ok()?;
        return Some(simple_rule(
            base_selector,
            &format!("flex-shrink:{};", shrink),
            important,
        ));
    }

    if let Some(v) = utility.strip_prefix("flex-shrink-") {
        let shrink = v.parse::<i32>().ok()?;
        return Some(simple_rule(
            base_selector,
            &format!("flex-shrink:{};", shrink),
            important,
        ));
    }

    match utility {
        "flex" => Some(simple_rule(base_selector, "display:flex;", important)),
        "grid" => Some(simple_rule(base_selector, "display:grid;", important)),
        "block" => Some(simple_rule(base_selector, "display:block;", important)),
        "inline" => Some(simple_rule(base_selector, "display:inline;", important)),
        "inline-block" => Some(simple_rule(base_selector, "display:inline-block;", important)),
        "hidden" => Some(simple_rule(base_selector, "display:none;", important)),
        "flex-col" => Some(simple_rule(base_selector, "flex-direction:column;", important)),
        "flex-row" => Some(simple_rule(base_selector, "flex-direction:row;", important)),
        "flex-wrap" => Some(simple_rule(base_selector, "flex-wrap:wrap;", important)),
        "flex-1" => Some(simple_rule(base_selector, "flex:1 1 0%;", important)),
        "flex-0" => Some(simple_rule(base_selector, "flex:0 0 auto;", important)),
        "flex-none" => Some(simple_rule(base_selector, "flex:none;", important)),
        "shrink-0" => Some(simple_rule(base_selector, "flex-shrink:0;", important)),
        "basis-0" => Some(simple_rule(base_selector, "flex-basis:0;", important)),
        "items-start" => Some(simple_rule(base_selector, "align-items:flex-start;", important)),
        "items-center" => Some(simple_rule(base_selector, "align-items:center;", important)),
        "items-end" => Some(simple_rule(base_selector, "align-items:flex-end;", important)),
        "items-stretch" => Some(simple_rule(base_selector, "align-items:stretch;", important)),
        "items-baseline" => Some(simple_rule(base_selector, "align-items:baseline;", important)),
        "align-start" => Some(simple_rule(base_selector, "align-items:flex-start;", important)),
        "justify-start" => Some(simple_rule(base_selector, "justify-content:flex-start;", important)),
        "justify-center" => Some(simple_rule(base_selector, "justify-content:center;", important)),
        "justify-end" => Some(simple_rule(base_selector, "justify-content:flex-end;", important)),
        "justify-between" => Some(simple_rule(
            base_selector,
            "justify-content:space-between;",
            important,
        )),
        "justify-around" => Some(simple_rule(base_selector, "justify-content:space-around;", important)),
        "justify-evenly" => Some(simple_rule(base_selector, "justify-content:space-evenly;", important)),
        "justify-stretch" => Some(simple_rule(base_selector, "justify-content:stretch;", important)),
        "rounded" => Some(simple_rule(base_selector, "border-radius:0.25rem;", important)),
        "rounded-sm" => Some(simple_rule(base_selector, "border-radius:0.25rem;", important)),
        "rounded-md" => Some(simple_rule(base_selector, "border-radius:0.375rem;", important)),
        "rounded-lg" => Some(simple_rule(base_selector, "border-radius:0.5rem;", important)),
        "rounded-xl" => Some(simple_rule(base_selector, "border-radius:0.75rem;", important)),
        "rounded-2xl" => Some(simple_rule(base_selector, "border-radius:1rem;", important)),
        "rounded-3xl" => Some(simple_rule(base_selector, "border-radius:1.5rem;", important)),
        "rounded-4xl" => Some(simple_rule(base_selector, "border-radius:2rem;", important)),
        "rounded-full" => Some(simple_rule(base_selector, "border-radius:9999px;", important)),
        "rounded-none" => Some(simple_rule(base_selector, "border-radius:0;", important)),
        "rounded-xs" => Some(simple_rule(base_selector, "border-radius:0.125rem;", important)),
        "shadow" | "shadow-sm" => Some(simple_rule(
            base_selector,
            "box-shadow:0 1px 2px rgba(0,0,0,0.05);",
            important,
        )),
        "shadow-md" => Some(simple_rule(
            base_selector,
            "box-shadow:0 4px 12px rgba(0,0,0,0.08);",
            important,
        )),
        "shadow-lg" => Some(simple_rule(
            base_selector,
            "box-shadow:0 12px 32px rgba(0,0,0,0.12);",
            important,
        )),
        "shadow-2xl" => Some(simple_rule(
            base_selector,
            "box-shadow:0 20px 48px rgba(0,0,0,0.2);",
            important,
        )),
        "shadow-xs" => Some(simple_rule(
            base_selector,
            "box-shadow:0 1px 1px rgba(0,0,0,0.04);",
            important,
        )),
        "font-normal" => Some(simple_rule(base_selector, "font-weight:400;", important)),
        "font-medium" => Some(simple_rule(base_selector, "font-weight:500;", important)),
        "font-semibold" => Some(simple_rule(base_selector, "font-weight:600;", important)),
        "font-bold" => Some(simple_rule(base_selector, "font-weight:700;", important)),
        "italic" => Some(simple_rule(base_selector, "font-style:italic;", important)),
        "text-left" => Some(simple_rule(base_selector, "text-align:left;", important)),
        "text-center" => Some(simple_rule(base_selector, "text-align:center;", important)),
        "text-right" => Some(simple_rule(base_selector, "text-align:right;", important)),
        "text-justify" => Some(simple_rule(base_selector, "text-align:justify;", important)),
        "leading-none" => Some(simple_rule(base_selector, "line-height:1;", important)),
        "leading-tight" => Some(simple_rule(base_selector, "line-height:1.25;", important)),
        "leading-snug" => Some(simple_rule(base_selector, "line-height:1.375;", important)),
        "leading-normal" => Some(simple_rule(base_selector, "line-height:1.5;", important)),
        "leading-relaxed" => Some(simple_rule(base_selector, "line-height:1.625;", important)),
        "tracking-tight" => Some(simple_rule(base_selector, "letter-spacing:-0.025em;", important)),
        "tracking-normal" => Some(simple_rule(base_selector, "letter-spacing:0;", important)),
        "tracking-wide" => Some(simple_rule(base_selector, "letter-spacing:0.025em;", important)),
        "tracking-wider" => Some(simple_rule(base_selector, "letter-spacing:0.05em;", important)),
        "tracking-widest" => Some(simple_rule(base_selector, "letter-spacing:0.1em;", important)),
        "border" => Some(simple_rule(base_selector, "border-width:1px;border-style:solid;", important)),
        "border-0" => Some(simple_rule(base_selector, "border-width:0;", important)),
        "border-2" => Some(simple_rule(base_selector, "border-width:2px;border-style:solid;", important)),
        "border-4" => Some(simple_rule(base_selector, "border-width:4px;border-style:solid;", important)),
        "border-t" => Some(simple_rule(base_selector, "border-top-width:1px;border-top-style:solid;", important)),
        "border-r" => Some(simple_rule(base_selector, "border-right-width:1px;border-right-style:solid;", important)),
        "border-b" => Some(simple_rule(base_selector, "border-bottom-width:1px;border-bottom-style:solid;", important)),
        "border-l" => Some(simple_rule(base_selector, "border-left-width:1px;border-left-style:solid;", important)),
        "border-x" => Some(simple_rule(
            base_selector,
            "border-left-width:1px;border-right-width:1px;border-left-style:solid;border-right-style:solid;",
            important,
        )),
        "border-y" => Some(simple_rule(
            base_selector,
            "border-top-width:1px;border-bottom-width:1px;border-top-style:solid;border-bottom-style:solid;",
            important,
        )),
        "border-dashed" => Some(simple_rule(base_selector, "border-style:dashed;", important)),
        "border-solid" => Some(simple_rule(base_selector, "border-style:solid;", important)),
        "relative" => Some(simple_rule(base_selector, "position:relative;", important)),
        "absolute" => Some(simple_rule(base_selector, "position:absolute;", important)),
        "fixed" => Some(simple_rule(base_selector, "position:fixed;", important)),
        "sticky" => Some(simple_rule(base_selector, "position:sticky;", important)),
        "min-h-screen" => Some(simple_rule(base_selector, "min-height:100vh;", important)),
        "h-full" => Some(simple_rule(base_selector, "height:100%;", important)),
        "w-full" => Some(simple_rule(base_selector, "width:100%;", important)),
        "w-auto" => Some(simple_rule(base_selector, "width:auto;", important)),
        "h-auto" => Some(simple_rule(base_selector, "height:auto;", important)),
        "overflow-hidden" => Some(simple_rule(base_selector, "overflow:hidden;", important)),
        "overflow-auto" => Some(simple_rule(base_selector, "overflow:auto;", important)),
        "overflow-scroll" => Some(simple_rule(base_selector, "overflow:scroll;", important)),
        "overflow-visible" => Some(simple_rule(base_selector, "overflow:visible;", important)),
        "overflow-x-auto" => Some(simple_rule(base_selector, "overflow-x:auto;", important)),
        "overflow-y-auto" => Some(simple_rule(base_selector, "overflow-y:auto;", important)),
        "overflow-x-hidden" => Some(simple_rule(base_selector, "overflow-x:hidden;", important)),
        "overflow-y-hidden" => Some(simple_rule(base_selector, "overflow-y:hidden;", important)),
        "whitespace-normal" => Some(simple_rule(base_selector, "white-space:normal;", important)),
        "whitespace-nowrap" => Some(simple_rule(base_selector, "white-space:nowrap;", important)),
        "transition" => Some(simple_rule(
            base_selector,
            "transition-property:all;transition-duration:150ms;transition-timing-function:cubic-bezier(0.4,0,0.2,1);",
            important,
        )),
        "transition-all" => Some(simple_rule(
            base_selector,
            "transition-property:all;transition-duration:150ms;transition-timing-function:cubic-bezier(0.4,0,0.2,1);",
            important,
        )),
        "transition-colors" => Some(simple_rule(
            base_selector,
            "transition-property:background-color,border-color,color,fill,stroke;transition-duration:150ms;transition-timing-function:cubic-bezier(0.4,0,0.2,1);",
            important,
        )),
        "transition-none" => Some(simple_rule(
            base_selector,
            "transition-property:none;",
            important,
        )),
        "transition-opacity" => Some(simple_rule(
            base_selector,
            "transition-property:opacity;transition-duration:150ms;transition-timing-function:cubic-bezier(0.4,0,0.2,1);",
            important,
        )),
        "transition-transform" => Some(simple_rule(
            base_selector,
            "transition-property:transform;transition-duration:150ms;transition-timing-function:cubic-bezier(0.4,0,0.2,1);",
            important,
        )),
        "cursor-pointer" => Some(simple_rule(base_selector, "cursor:pointer;", important)),
        "cursor-default" => Some(simple_rule(base_selector, "cursor:default;", important)),
        "uppercase" => Some(simple_rule(base_selector, "text-transform:uppercase;", important)),
        "lowercase" => Some(simple_rule(base_selector, "text-transform:lowercase;", important)),
        "capitalize" => Some(simple_rule(base_selector, "text-transform:capitalize;", important)),
        "underline" => Some(simple_rule(base_selector, "text-decoration:underline;", important)),
        "inline-flex" => Some(simple_rule(base_selector, "display:inline-flex;", important)),
        "list-disc" => Some(simple_rule(base_selector, "list-style-type:disc;", important)),
        "list-inside" => Some(simple_rule(base_selector, "list-style-position:inside;", important)),
        "break-words" => Some(simple_rule(base_selector, "overflow-wrap:break-word;", important)),
        "appearance-none" => Some(simple_rule(base_selector, "appearance:none;", important)),
        "backdrop-blur-sm" => Some(simple_rule(base_selector, "backdrop-filter:blur(4px);", important)),
        "antialiased" => Some(simple_rule(
            base_selector,
            "-webkit-font-smoothing:antialiased;-moz-osx-font-smoothing:grayscale;",
            important,
        )),
        "pointer-events-none" => Some(simple_rule(base_selector, "pointer-events:none;", important)),
        "pointer-events-auto" => Some(simple_rule(base_selector, "pointer-events:auto;", important)),
        "select-none" => Some(simple_rule(base_selector, "user-select:none;", important)),
        "fill-current" => Some(simple_rule(base_selector, "fill:currentColor;", important)),
        "align-top" => Some(simple_rule(base_selector, "vertical-align:top;", important)),
        "align-middle" => Some(simple_rule(base_selector, "vertical-align:middle;", important)),
        "resize-y" => Some(simple_rule(base_selector, "resize:vertical;", important)),
        "touch-pan-y" => Some(simple_rule(base_selector, "touch-action:pan-y;", important)),
        "tabular-nums" => Some(simple_rule(base_selector, "font-variant-numeric:tabular-nums;", important)),
        "sr-only" => Some(simple_rule(base_selector, "position:absolute;width:1px;height:1px;padding:0;margin:-1px;overflow:hidden;clip:rect(0,0,0,0);white-space:nowrap;border-width:0;", important)),
        "prose-sm" => Some(simple_rule(base_selector, "font-size:0.875rem;line-height:1.7142857;", important)),
        "bg-center" => Some(simple_rule(base_selector, "background-position:center;", important)),
        "bg-bottom" => Some(simple_rule(base_selector, "background-position:bottom;", important)),
        "bg-repeat" => Some(simple_rule(base_selector, "background-repeat:repeat;", important)),
        "bg-repeat-x" => Some(simple_rule(base_selector, "background-repeat:repeat-x;", important)),
        "bg-repeat-y" => Some(simple_rule(base_selector, "background-repeat:repeat-y;", important)),
        "bg-no-repeat" => Some(simple_rule(base_selector, "background-repeat:no-repeat;", important)),
        "bg-cover" => Some(simple_rule(base_selector, "background-size:cover;", important)),
        "bg-contain" => Some(simple_rule(base_selector, "background-size:contain;", important)),
        "bg-gradient-to-r" => Some(simple_rule(
            base_selector,
            "background-image:linear-gradient(to right,var(--tw-gradient-from),var(--tw-gradient-to));",
            important,
        )),
        "bg-gradient-to-b" => Some(simple_rule(
            base_selector,
            "background-image:linear-gradient(to bottom,var(--tw-gradient-from),var(--tw-gradient-to));",
            important,
        )),
        "bg-gradient-to-br" => Some(simple_rule(
            base_selector,
            "background-image:linear-gradient(to bottom right,var(--tw-gradient-from),var(--tw-gradient-to));",
            important,
        )),
        "outline-none" => Some(simple_rule(base_selector, "outline:2px solid transparent;outline-offset:2px;", important)),
        "outline-hidden" => Some(simple_rule(base_selector, "outline:none;", important)),
        "ring" => Some(simple_rule(base_selector, "box-shadow:0 0 0 1px rgba(59,130,246,0.5);", important)),
        "ring-1" => Some(simple_rule(base_selector, "box-shadow:0 0 0 1px rgba(59,130,246,0.5);", important)),
        "ring-2" => Some(simple_rule(base_selector, "box-shadow:0 0 0 2px rgba(59,130,246,0.5);", important)),
        "ring-4" => Some(simple_rule(base_selector, "box-shadow:0 0 0 4px rgba(59,130,246,0.5);", important)),
        "max-w-none" => Some(simple_rule(base_selector, "max-width:none;", important)),
        "w-px" => Some(simple_rule(base_selector, "width:1px;", important)),
        "size-full" => Some(simple_rule(base_selector, "width:100%;height:100%;", important)),
        "list-decimal" => Some(simple_rule(base_selector, "list-style-type:decimal;", important)),
        "animate-spin" => Some(UtilityRule {
            selector: base_selector.to_string(),
            declarations: maybe_important("animation:josie-spin 1s linear infinite;", important),
            prelude: Some("@keyframes josie-spin{to{transform:rotate(360deg);}}".to_string()),
        }),
        "animate-ping" => Some(UtilityRule {
            selector: base_selector.to_string(),
            declarations: maybe_important("animation:josie-ping 1s cubic-bezier(0,0,0.2,1) infinite;", important),
            prelude: Some("@keyframes josie-ping{75%,100%{transform:scale(2);opacity:0;}}".to_string()),
        }),
        "animate-pulse" => Some(UtilityRule {
            selector: base_selector.to_string(),
            declarations: maybe_important("animation:josie-pulse 2s cubic-bezier(0.4,0,0.6,1) infinite;", important),
            prelude: Some("@keyframes josie-pulse{0%,100%{opacity:1;}50%{opacity:.5;}}".to_string()),
        }),
        "animate-bounce" => Some(UtilityRule {
            selector: base_selector.to_string(),
            declarations: maybe_important("animation:josie-bounce 1s infinite;", important),
            prelude: Some("@keyframes josie-bounce{0%,100%{transform:translateY(-25%);animation-timing-function:cubic-bezier(.8,0,1,1);}50%{transform:none;animation-timing-function:cubic-bezier(0,0,.2,1);}}".to_string()),
        }),
        _ => None,
    }
}

fn simple_rule(selector: &str, declarations: &str, important: bool) -> UtilityRule {
    UtilityRule {
        selector: selector.to_string(),
        declarations: maybe_important(declarations, important),
        prelude: None,
    }
}

fn maybe_important(declarations: &str, important: bool) -> String {
    if !important {
        return declarations.to_string();
    }

    let mut out = String::new();
    for part in declarations.split(';') {
        let trimmed = part.trim();
        if trimmed.is_empty() {
            continue;
        }

        if let Some((k, v)) = trimmed.split_once(':') {
            out.push_str(k.trim());
            out.push(':');
            out.push_str(v.trim());
            out.push_str(" !important;");
        }
    }

    out
}

fn spacing_value(v: &str) -> Option<String> {
    if v == "px" {
        return Some("1px".to_string());
    }

    if let Some(raw) = arbitrary_value(v) {
        return Some(raw);
    }

    let parsed = v.parse::<f64>().ok()?;
    if (parsed - 0.0).abs() < f64::EPSILON {
        return Some("0".to_string());
    }

    Some(format_rem(parsed * 0.25))
}

/// Parse inset utility values (`top-*`, `left-*`, etc.).
///
/// Supports:
/// - numeric spacing scale (`4` -> `1rem`)
/// - fractions (`1/2` -> `50%`)
/// - arbitrary values (`[40%]`)
/// - keywords (`auto`, `full`, `0`)
fn inset_value(v: &str) -> Option<String> {
    if v == "0" {
        return Some("0".to_string());
    }

    if v == "auto" {
        return Some("auto".to_string());
    }

    if v == "full" {
        return Some("100%".to_string());
    }

    if let Some(raw) = arbitrary_value(v) {
        return Some(raw);
    }

    if let Some(frac) = fraction_to_percent(v) {
        return Some(frac);
    }

    spacing_value(v)
}

/// Negate a CSS value for negative utilities (for example `-top-4`).
///
/// Rules:
/// - `0` stays `0`
/// - function values become `calc(<value> * -1)`
/// - other values are prefixed with `-`
fn negate_css_value(v: &str) -> Option<String> {
    let trimmed = v.trim();
    if trimmed.is_empty() || trimmed == "auto" {
        return None;
    }
    if trimmed == "0" || trimmed == "0px" || trimmed == "0rem" || trimmed == "0%" {
        return Some("0".to_string());
    }
    if trimmed.starts_with('-') {
        return Some(trimmed.to_string());
    }
    if trimmed.starts_with("var(")
        || trimmed.starts_with("calc(")
        || trimmed.starts_with("min(")
        || trimmed.starts_with("max(")
        || trimmed.starts_with("clamp(")
    {
        return Some(format!("calc({} * -1)", trimmed));
    }

    Some(format!("-{}", trimmed))
}

#[derive(Debug, Clone, Copy)]
enum SizeAxis {
    Width,
    Height,
}

fn size_value(v: &str, axis: SizeAxis) -> Option<String> {
    match v {
        "full" => return Some("100%".to_string()),
        "auto" => return Some("auto".to_string()),
        "min" => return Some("min-content".to_string()),
        "max" => return Some("max-content".to_string()),
        "fit" => return Some("fit-content".to_string()),
        "screen" => {
            return Some(match axis {
                SizeAxis::Width => "100vw".to_string(),
                SizeAxis::Height => "100vh".to_string(),
            });
        }
        "px" => return Some("1px".to_string()),
        _ => {}
    }

    if let Some(raw) = arbitrary_value(v) {
        return Some(raw);
    }

    if let Some(frac) = fraction_to_percent(v) {
        return Some(frac);
    }

    if has_direct_css_unit(v) {
        return Some(v.to_string());
    }

    spacing_value(v)
}

fn minmax_size_value(v: &str, axis: SizeAxis) -> Option<String> {
    if let Some(mapped) = size_value(v, axis) {
        return Some(mapped);
    }

    if v == "none" {
        return Some("none".to_string());
    }

    None
}

fn max_width_value(v: &str) -> Option<String> {
    match v {
        "sm" => Some("24rem".to_string()),
        "md" => Some("28rem".to_string()),
        "lg" => Some("32rem".to_string()),
        "xl" => Some("36rem".to_string()),
        "2xl" => Some("42rem".to_string()),
        "3xl" => Some("48rem".to_string()),
        "4xl" => Some("56rem".to_string()),
        "5xl" => Some("64rem".to_string()),
        "6xl" => Some("72rem".to_string()),
        "screen-md" => Some("768px".to_string()),
        "screen-lg" => Some("1024px".to_string()),
        _ => minmax_size_value(v, SizeAxis::Width),
    }
}

fn text_size_value(v: &str) -> Option<&'static str> {
    match v {
        "xs" => Some("0.75rem"),
        "sm" => Some("0.875rem"),
        "base" => Some("1rem"),
        "lg" => Some("1.125rem"),
        "xl" => Some("1.25rem"),
        "2xl" => Some("1.5rem"),
        "3xl" => Some("1.875rem"),
        "4xl" => Some("2.25rem"),
        "5xl" => Some("3rem"),
        _ => None,
    }
}

fn border_rule(v: &str) -> Option<String> {
    match v {
        "0" => Some("border-width:0;".to_string()),
        "2" => Some("border-width:2px;border-style:solid;".to_string()),
        "4" => Some("border-width:4px;border-style:solid;".to_string()),
        "t" => Some("border-top-width:1px;border-top-style:solid;".to_string()),
        "r" => Some("border-right-width:1px;border-right-style:solid;".to_string()),
        "b" => Some("border-bottom-width:1px;border-bottom-style:solid;".to_string()),
        "l" => Some("border-left-width:1px;border-left-style:solid;".to_string()),
        "x" => Some(
            "border-left-width:1px;border-right-width:1px;border-left-style:solid;border-right-style:solid;"
                .to_string(),
        ),
        "y" => Some(
            "border-top-width:1px;border-bottom-width:1px;border-top-style:solid;border-bottom-style:solid;"
                .to_string(),
        ),
        "dashed" => Some("border-style:dashed;".to_string()),
        "solid" => Some("border-style:solid;".to_string()),
        _ => {
            let color = color_value(v)?;
            Some(format!("border-color:{};", color))
        }
    }
}

fn outline_rule(v: &str) -> Option<String> {
    match v {
        "none" => Some("outline:2px solid transparent;outline-offset:2px;".to_string()),
        "hidden" => Some("outline:none;".to_string()),
        _ => {
            if let Ok(px) = v.parse::<u32>() {
                return Some(format!("outline-width:{}px;outline-style:solid;", px));
            }

            let color = color_value(v)?;
            Some(format!("outline-color:{};outline-style:solid;", color))
        }
    }
}

fn ring_rule(v: &str) -> Option<String> {
    if let Some(raw) = arbitrary_value(v) {
        if raw.ends_with("px") || raw.ends_with("rem") || raw.ends_with("em") {
            return Some(format!("box-shadow:0 0 0 {} rgba(59,130,246,0.5);", raw));
        }
        if let Ok(px) = raw.parse::<u32>() {
            return Some(format!("box-shadow:0 0 0 {}px rgba(59,130,246,0.5);", px));
        }
    }

    if let Ok(px) = v.parse::<u32>() {
        return Some(format!("box-shadow:0 0 0 {}px rgba(59,130,246,0.5);", px));
    }

    let color = color_value(v)?;
    Some(format!("box-shadow:0 0 0 3px {};", color))
}

/// Parse arbitrary background payload from `bg-[...]`.
fn background_value(v: &str) -> Option<String> {
    if let Some(raw) = arbitrary_value(v) {
        return Some(raw);
    }

    None
}

/// Choose safe background declaration kind.
///
/// Image-like values must emit `background-image` instead of `background`
/// shorthand, so companion utilities (`bg-repeat-x`, `bg-bottom`) are preserved.
fn background_declaration(value: &str) -> String {
    let trimmed = value.trim();
    let is_image = trimmed.starts_with("url(")
        || trimmed.starts_with("linear-gradient(")
        || trimmed.starts_with("radial-gradient(")
        || trimmed.starts_with("conic-gradient(")
        || trimmed.starts_with("image(");

    if is_image {
        format!("background-image:{};", trimmed)
    } else {
        format!("background:{};", trimmed)
    }
}

/// Resolve color token into CSS color string.
///
/// Resolution order:
/// 1. arbitrary `[... ]`
/// 2. built-ins (`white`, `black`, etc.)
/// 3. known Tailwind palette subset
/// 4. semantic tokens (`var(--josie-color-*)`)
fn color_value(v: &str) -> Option<String> {
    if let Some(raw) = arbitrary_value(v) {
        return Some(raw);
    }

    let (base, alpha) = split_color_alpha(v);

    let color = if base == "current" {
        "currentColor".to_string()
    } else if base == "transparent" {
        "transparent".to_string()
    } else if base == "black" {
        "#000000".to_string()
    } else if base == "white" {
        "#ffffff".to_string()
    } else if let Some(hex) = tw_color_hex(base) {
        hex.to_string()
    } else if is_semantic_color_token(base) {
        let key = base.replace('_', "-");
        format!("var(--josie-color-{},{})", key, key)
    } else {
        return None;
    };

    if let Some(alpha) = alpha {
        return Some(apply_alpha(&color, alpha));
    }

    Some(color)
}

fn split_color_alpha(v: &str) -> (&str, Option<f64>) {
    if let Some((base, alpha_raw)) = v.split_once('/') {
        if let Some(alpha) = parse_alpha(alpha_raw) {
            return (base, Some(alpha));
        }
        return (base, None);
    }

    (v, None)
}

fn parse_alpha(v: &str) -> Option<f64> {
    if let Some(raw) = v.strip_prefix('[').and_then(|s| s.strip_suffix(']')) {
        return raw.parse::<f64>().ok().map(|n| n.clamp(0.0, 1.0));
    }

    if let Ok(num) = v.parse::<u32>() {
        return Some((num.min(100) as f64) / 100.0);
    }

    v.parse::<f64>().ok().map(|n| n.clamp(0.0, 1.0))
}

fn apply_alpha(color: &str, alpha: f64) -> String {
    if color == "transparent" {
        return color.to_string();
    }

    if let Some((r, g, b)) = hex_to_rgb(color) {
        return format!("rgba({}, {}, {}, {:.3})", r, g, b, alpha);
    }

    let pct = (alpha * 100.0).clamp(0.0, 100.0);
    format!("color-mix(in srgb, {} {:.1}%, transparent)", color, pct)
}

fn hex_to_rgb(v: &str) -> Option<(u8, u8, u8)> {
    let hex = v.strip_prefix('#')?;
    match hex.len() {
        6 => {
            let r = u8::from_str_radix(&hex[0..2], 16).ok()?;
            let g = u8::from_str_radix(&hex[2..4], 16).ok()?;
            let b = u8::from_str_radix(&hex[4..6], 16).ok()?;
            Some((r, g, b))
        }
        3 => {
            let r = u8::from_str_radix(&hex[0..1].repeat(2), 16).ok()?;
            let g = u8::from_str_radix(&hex[1..2].repeat(2), 16).ok()?;
            let b = u8::from_str_radix(&hex[2..3].repeat(2), 16).ok()?;
            Some((r, g, b))
        }
        _ => None,
    }
}

fn is_semantic_color_token(v: &str) -> bool {
    if v.is_empty() {
        return false;
    }

    v.chars()
        .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '-' || c == '_')
}

fn is_size_like(v: &str) -> bool {
    if v == "0" || v == "auto" {
        return true;
    }
    has_direct_css_unit(v)
}

fn tw_color_hex(v: &str) -> Option<&'static str> {
    match v {
        "slate-50" => Some("#f8fafc"),
        "slate-100" => Some("#f1f5f9"),
        "slate-200" => Some("#e2e8f0"),
        "slate-300" => Some("#cbd5e1"),
        "slate-400" => Some("#94a3b8"),
        "slate-500" => Some("#64748b"),
        "slate-600" => Some("#475569"),
        "slate-700" => Some("#334155"),
        "slate-800" => Some("#1e293b"),
        "slate-900" => Some("#0f172a"),
        "gray-50" => Some("#f9fafb"),
        "gray-100" => Some("#f3f4f6"),
        "gray-200" => Some("#e5e7eb"),
        "gray-300" => Some("#d1d5db"),
        "gray-400" => Some("#9ca3af"),
        "gray-500" => Some("#6b7280"),
        "gray-600" => Some("#4b5563"),
        "gray-700" => Some("#374151"),
        "gray-800" => Some("#1f2937"),
        "gray-900" => Some("#111827"),
        "blue-400" => Some("#60a5fa"),
        "blue-500" => Some("#3b82f6"),
        "blue-600" => Some("#2563eb"),
        "blue-700" => Some("#1d4ed8"),
        "green-50" => Some("#f0fdf4"),
        "green-500" => Some("#22c55e"),
        "green-600" => Some("#16a34a"),
        "red-400" => Some("#f87171"),
        "red-500" => Some("#ef4444"),
        "red-600" => Some("#dc2626"),
        "yellow-50" => Some("#fefce8"),
        "yellow-500" => Some("#eab308"),
        "amber-50" => Some("#fffbeb"),
        "amber-100" => Some("#fef3c7"),
        "amber-500" => Some("#f59e0b"),
        "purple-500" => Some("#a855f7"),
        _ => None,
    }
}

/// Parse arbitrary payload in brackets.
///
/// Security notes:
/// - blocks `;`, `{`, `}` to prevent declaration/context escape
/// - allows a conservative safe character set
/// - converts `_` to space to match Tailwind arbitrary-value behavior
fn arbitrary_value(v: &str) -> Option<String> {
    let raw = v.strip_prefix('[')?.strip_suffix(']')?;
    if raw.is_empty() {
        return None;
    }

    let safe = raw.chars().all(|c| {
        c.is_ascii_alphanumeric()
            || matches!(
                c,
                '#' | '.' | ',' | '%' | '/' | '_' | '-' | '(' | ')' | ':' | '\'' | '"' | ' '
            )
    });
    if !safe || raw.contains(';') || raw.contains('{') || raw.contains('}') {
        return None;
    }

    Some(raw.replace('_', " "))
}

fn fraction_to_percent(v: &str) -> Option<String> {
    let (a, b) = v.split_once('/')?;
    let num = a.parse::<f64>().ok()?;
    let den = b.parse::<f64>().ok()?;
    if den == 0.0 {
        return None;
    }

    let pct = (num / den) * 100.0;
    let mut s = format!("{:.6}", pct);
    while s.ends_with('0') {
        s.pop();
    }
    if s.ends_with('.') {
        s.pop();
    }
    if s.is_empty() {
        s.push('0');
    }
    Some(format!("{}%", s))
}

fn has_direct_css_unit(v: &str) -> bool {
    let lowered = v.to_ascii_lowercase();
    ["px", "rem", "em", "%", "vh", "vw", "svh", "dvh", "ch"]
        .iter()
        .any(|unit| lowered.ends_with(unit))
}

fn format_rem(v: f64) -> String {
    let mut s = format!("{:.6}", v);
    while s.ends_with('0') {
        s.pop();
    }
    if s.ends_with('.') {
        s.pop();
    }
    if s.is_empty() {
        "0rem".to_string()
    } else {
        format!("{}rem", s)
    }
}

/// Escape class token into a valid CSS selector fragment.
///
/// Example:
/// - `lg:text-left` -> `.lg\:text-left`
/// - `bg-[url('/x.svg')]` -> `.bg-\[url\(\'\/x\.svg\'\)\]`
fn escape_class_selector(token: &str) -> String {
    let mut out = String::new();
    for ch in token.chars() {
        if ch.is_ascii_alphanumeric() || ch == '-' || ch == '_' {
            out.push(ch);
        } else if ch == '\\' {
            out.push_str("\\\\");
        } else {
            out.push('\\');
            out.push(ch);
        }
    }
    out
}
