//! JOSIEML parser/renderer for `josie-web` Tailwind runtime.
//!
//! This module converts HTML-like JOSIEML syntax into runtime-compatible
//! attributes and emits page-scoped Tailwind CSS for used classes.
//!
//! Supported sugar:
//! - `j-text="client.path"` -> `data-josie-binds` children var expression
//! - `j-val="client.path"` -> `data-josie-binds` value var expression
//! - `j-model="client.path"` -> value bind + `input` set expression (`w.event.value`)
//! - `@click="action"` -> `data-josie-ons` event expression from script logic
//! - `j-attr:class="client.path"` -> className bind
//!
//! Source config is read from:
//! `<script type="application/josie+json"> ... </script>`

use crate::{JOSIE_RUNTIME_JS, JosieDocument, Theme};
use serde_json::{Map, Value, json};
use std::collections::HashSet;

pub fn render_josieml_with_runtime(source: &str) -> Result<String, String> {
    let (template, config) = extract_josie_config(source)?;
    let mut html = ensure_document_shell(&template);

    let logic = config
        .get("logic")
        .and_then(Value::as_object)
        .cloned()
        .unwrap_or_default();

    html = rewrite_josieml_attributes(&html, &logic)?;

    let theme = config
        .get("theme")
        .cloned()
        .map(parse_theme)
        .transpose()?
        .unwrap_or_default();

    let css = build_css_for_html_classes(&html, &theme);
    html = inject_style(html, &css);

    let state_payload = extract_client_state(config.get("state"));
    let bootstrap = json!({
        "state": state_payload,
        "theme": theme
    });
    let state_script = format!(
        "<script>window.__JOSIE__ = {};</script>",
        serde_json::to_string(&bootstrap).unwrap_or_else(|_| "{}".to_string())
    );

    if let Some(idx) = html.rfind("</body>") {
        let before = &html[..idx];
        let after = &html[idx..];
        Ok(format!(
            "{}{}<script>{}</script>{}",
            before, state_script, JOSIE_RUNTIME_JS, after
        ))
    } else {
        Ok(format!(
            "{}{}<script>{}</script>",
            html, state_script, JOSIE_RUNTIME_JS
        ))
    }
}

fn parse_theme(value: Value) -> Result<Theme, String> {
    serde_json::from_value(value).map_err(|e| format!("invalid josieml theme: {e}"))
}

fn extract_client_state(state: Option<&Value>) -> Value {
    let Some(state) = state else {
        return Value::Object(Map::new());
    };

    let Some(obj) = state.as_object() else {
        return Value::Object(Map::new());
    };

    if let Some(client) = obj.get("client") {
        return client.clone();
    }

    Value::Object(obj.clone())
}

fn ensure_document_shell(markup: &str) -> String {
    let lower = markup.to_ascii_lowercase();
    if lower.contains("<html") {
        return markup.to_string();
    }

    format!(
        "<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"utf-8\">\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n</head>\n<body>\n{}\n</body>\n</html>",
        markup
    )
}

fn extract_josie_config(source: &str) -> Result<(String, Value), String> {
    let mut cursor = 0usize;
    let lower = source.to_ascii_lowercase();

    while let Some(rel_idx) = lower[cursor..].find("<script") {
        let start = cursor + rel_idx;
        let open_end =
            find_tag_end(source, start).ok_or_else(|| "unterminated <script> tag".to_string())?;
        let open_tag = &source[start..=open_end];
        let attrs_text = &source[start + "<script".len()..open_end];
        let attrs = parse_attributes(attrs_text);
        let script_type = attrs
            .iter()
            .find(|a| a.name.eq_ignore_ascii_case("type"))
            .and_then(|a| a.value.as_ref())
            .map(|s| s.trim().to_ascii_lowercase());

        if script_type.as_deref() == Some("application/josie+json") {
            let close_start = find_close_tag(source, open_end + 1, "script")
                .ok_or_else(|| "missing </script> for application/josie+json block".to_string())?;
            let close_end = close_start + "</script>".len();
            let payload = source[open_end + 1..close_start].trim();
            let parsed = if payload.is_empty() {
                json!({})
            } else {
                serde_json::from_str::<Value>(payload)
                    .map_err(|e| format!("invalid application/josie+json payload: {e}"))?
            };

            let mut template = String::new();
            template.push_str(&source[..start]);
            template.push_str(&source[close_end..]);
            return Ok((template, parsed));
        }

        let _ = open_tag;
        cursor = open_end + 1;
    }

    Ok((source.to_string(), json!({})))
}

fn rewrite_josieml_attributes(input: &str, logic: &Map<String, Value>) -> Result<String, String> {
    let mut out = String::new();
    let mut i = 0usize;

    while i < input.len() {
        let Some(rel_lt) = input[i..].find('<') else {
            out.push_str(&input[i..]);
            break;
        };

        let lt = i + rel_lt;
        out.push_str(&input[i..lt]);

        let rest = &input[lt..];
        if rest.starts_with("<!--") {
            if let Some(rel_end) = rest.find("-->") {
                let end = lt + rel_end + 3;
                out.push_str(&input[lt..end]);
                i = end;
                continue;
            }
            return Err("unterminated HTML comment".to_string());
        }

        if rest.starts_with("</") || rest.starts_with("<!") || rest.starts_with("<?") {
            let end = find_tag_end(input, lt).ok_or_else(|| "unterminated tag".to_string())?;
            out.push_str(&input[lt..=end]);
            i = end + 1;
            continue;
        }

        let end = find_tag_end(input, lt).ok_or_else(|| "unterminated start tag".to_string())?;
        let inside = &input[lt + 1..end];
        let (tag_name, attrs_text, self_closing) = split_start_tag(inside);

        if tag_name.eq_ignore_ascii_case("script") || tag_name.eq_ignore_ascii_case("style") {
            let close_start = find_close_tag(input, end + 1, tag_name)
                .ok_or_else(|| format!("missing closing tag for <{}>", tag_name))?;
            let close_end = close_start + tag_name.len() + 3;
            out.push_str(&input[lt..close_end]);
            i = close_end;
            continue;
        }

        let attrs = parse_attributes(attrs_text);
        let rewritten = rewrite_attr_set(attrs, logic)?;
        out.push('<');
        out.push_str(tag_name);
        for attr in rewritten {
            out.push(' ');
            out.push_str(&attr.name);
            if let Some(value) = attr.value {
                out.push_str("=\"");
                out.push_str(&escape_attr(&value));
                out.push('"');
            }
        }
        if self_closing {
            out.push_str(" />");
        } else {
            out.push('>');
        }
        i = end + 1;
    }

    Ok(out)
}

fn rewrite_attr_set(
    attrs: Vec<ParsedAttr>,
    logic: &Map<String, Value>,
) -> Result<Vec<ParsedAttr>, String> {
    let mut out = Vec::new();
    let mut binds: Map<String, Value> = Map::new();
    let mut ons: Map<String, Value> = Map::new();

    for attr in attrs {
        let name = attr.name.trim();
        let value = attr.value.clone().unwrap_or_default().trim().to_string();

        if name.eq_ignore_ascii_case("data-josie-binds") {
            if !value.is_empty() {
                if let Ok(parsed) = serde_json::from_str::<Value>(&value) {
                    if let Some(obj) = parsed.as_object() {
                        for (k, v) in obj {
                            binds.insert(k.clone(), v.clone());
                        }
                    }
                }
            }
            continue;
        }

        if name.eq_ignore_ascii_case("data-josie-ons") {
            if !value.is_empty() {
                if let Ok(parsed) = serde_json::from_str::<Value>(&value) {
                    if let Some(obj) = parsed.as_object() {
                        for (k, v) in obj {
                            ons.insert(k.clone(), v.clone());
                        }
                    }
                }
            }
            continue;
        }

        if name.eq_ignore_ascii_case("j-text") {
            if !value.is_empty() {
                binds.insert("children".to_string(), json!(["var", value]));
            }
            continue;
        }

        if name.eq_ignore_ascii_case("j-val") {
            if !value.is_empty() {
                binds.insert("value".to_string(), json!(["var", value]));
            }
            continue;
        }

        if name.eq_ignore_ascii_case("j-model") {
            if !value.is_empty() {
                binds.insert("value".to_string(), json!(["var", value]));
                ons.insert("input".to_string(), json!(["set", value, ["w.event.value"]]));
            }
            continue;
        }

        if let Some(rest) = name.strip_prefix("j-attr:") {
            if !value.is_empty() {
                let prop = if rest == "class" { "className" } else { rest };
                binds.insert(prop.to_string(), json!(["var", value]));
            }
            continue;
        }

        if let Some(event_name) = name.strip_prefix('@') {
            if !event_name.is_empty() && !value.is_empty() {
                let expr = resolve_event_expr(&value, logic)?;
                ons.insert(event_name.to_string(), expr);
            }
            continue;
        }

        out.push(attr);
    }

    if !binds.is_empty() {
        let payload = serde_json::to_string(&Value::Object(binds))
            .map_err(|e| format!("failed to serialize data-josie-binds: {e}"))?;
        out.push(ParsedAttr::with_value("data-josie-binds", &payload));
    }

    if !ons.is_empty() {
        let payload = serde_json::to_string(&Value::Object(ons))
            .map_err(|e| format!("failed to serialize data-josie-ons: {e}"))?;
        out.push(ParsedAttr::with_value("data-josie-ons", &payload));
    }

    Ok(out)
}

fn resolve_event_expr(action: &str, logic: &Map<String, Value>) -> Result<Value, String> {
    if let Some(expr) = logic.get(action) {
        return Ok(expr.clone());
    }

    if action.starts_with('[') || action.starts_with('{') {
        let parsed = serde_json::from_str::<Value>(action)
            .map_err(|e| format!("invalid inline event expression '{action}': {e}"))?;
        return Ok(parsed);
    }

    Ok(Value::String(action.to_string()))
}

fn build_css_for_html_classes(html: &str, theme: &Theme) -> String {
    let mut seen = HashSet::<String>::new();
    collect_class_tokens(html, &mut seen);

    let mut tokens: Vec<String> = seen.into_iter().collect();
    tokens.sort_unstable_by_key(|token| (super::token_responsive_rank(token), token.clone()));

    let mut css = String::new();
    css.push_str(super::TAILWIND_PREFLIGHT_CSS);
    css.push('\n');
    css.push_str(&super::theme_color_var_css(&JosieDocument {
        version: None,
        theme: Some(theme.clone()),
        state: None,
        head: None,
        body: None,
    }));

    for token in tokens {
        if let Some(rule) = super::tw::token_css_rule(&token) {
            css.push_str(&rule);
            css.push('\n');
        }
    }

    css
}

fn collect_class_tokens(input: &str, out: &mut HashSet<String>) {
    let mut i = 0usize;
    while i < input.len() {
        let Some(rel_lt) = input[i..].find('<') else {
            break;
        };
        let lt = i + rel_lt;
        let rest = &input[lt..];

        if rest.starts_with("</")
            || rest.starts_with("<!--")
            || rest.starts_with("<!")
            || rest.starts_with("<?")
        {
            if let Some(end) = find_tag_end(input, lt) {
                i = end + 1;
                continue;
            }
            break;
        }

        let Some(end) = find_tag_end(input, lt) else {
            break;
        };
        let inside = &input[lt + 1..end];
        let (_, attrs_text, _) = split_start_tag(inside);
        for attr in parse_attributes(attrs_text) {
            if attr.name.eq_ignore_ascii_case("class") {
                if let Some(v) = attr.value {
                    for token in v.split_whitespace() {
                        if !token.is_empty() {
                            out.insert(token.to_string());
                        }
                    }
                }
            }
        }
        i = end + 1;
    }
}

fn inject_style(mut html: String, css: &str) -> String {
    if css.trim().is_empty() {
        return html;
    }

    if let Some(idx) = html.find("</head>") {
        let before = &html[..idx];
        let after = &html[idx..];
        let style_tag = format!("<style data-josie-tw>\n{}\n</style>", css);
        html = format!("{}{}{}", before, style_tag, after);
        return html;
    }

    if let Some(idx) = html.find("<body") {
        let style_tag = format!("<head><style data-josie-tw>\n{}\n</style></head>", css);
        let before = &html[..idx];
        let after = &html[idx..];
        return format!("{}{}{}", before, style_tag, after);
    }

    format!("<style data-josie-tw>\n{}\n</style>{}", css, html)
}

#[derive(Debug, Clone)]
struct ParsedAttr {
    name: String,
    value: Option<String>,
}

impl ParsedAttr {
    fn with_value(name: &str, value: &str) -> Self {
        Self {
            name: name.to_string(),
            value: Some(value.to_string()),
        }
    }
}

fn split_start_tag(inside: &str) -> (&str, &str, bool) {
    let trimmed = inside.trim();
    let self_closing = trimmed.ends_with('/');
    let core = if self_closing {
        trimmed[..trimmed.len().saturating_sub(1)].trim_end()
    } else {
        trimmed
    };

    let mut name_end = core.len();
    for (idx, ch) in core.char_indices() {
        if ch.is_whitespace() {
            name_end = idx;
            break;
        }
    }

    let name = core[..name_end].trim();
    let attrs = core[name_end..].trim_start();
    (name, attrs, self_closing)
}

fn parse_attributes(input: &str) -> Vec<ParsedAttr> {
    let mut out = Vec::new();
    let mut i = 0usize;
    let bytes = input.as_bytes();

    while i < bytes.len() {
        while i < bytes.len() && bytes[i].is_ascii_whitespace() {
            i += 1;
        }
        if i >= bytes.len() {
            break;
        }

        let name_start = i;
        while i < bytes.len()
            && !bytes[i].is_ascii_whitespace()
            && bytes[i] != b'='
            && bytes[i] != b'/'
        {
            i += 1;
        }
        if i == name_start {
            i += 1;
            continue;
        }
        let name = input[name_start..i].to_string();

        while i < bytes.len() && bytes[i].is_ascii_whitespace() {
            i += 1;
        }

        if i >= bytes.len() || bytes[i] != b'=' {
            out.push(ParsedAttr { name, value: None });
            continue;
        }

        i += 1;
        while i < bytes.len() && bytes[i].is_ascii_whitespace() {
            i += 1;
        }

        if i >= bytes.len() {
            out.push(ParsedAttr {
                name,
                value: Some(String::new()),
            });
            break;
        }

        let value = if bytes[i] == b'"' || bytes[i] == b'\'' {
            let quote = bytes[i];
            i += 1;
            let val_start = i;
            while i < bytes.len() && bytes[i] != quote {
                i += 1;
            }
            let v = input[val_start..i].to_string();
            if i < bytes.len() {
                i += 1;
            }
            v
        } else {
            let val_start = i;
            while i < bytes.len() && !bytes[i].is_ascii_whitespace() {
                i += 1;
            }
            input[val_start..i].to_string()
        };

        out.push(ParsedAttr {
            name,
            value: Some(value),
        });
    }

    out
}

fn find_tag_end(input: &str, lt_idx: usize) -> Option<usize> {
    let bytes = input.as_bytes();
    let mut i = lt_idx + 1;
    let mut quote: Option<u8> = None;

    while i < bytes.len() {
        let b = bytes[i];
        if let Some(q) = quote {
            if b == q {
                quote = None;
            }
            i += 1;
            continue;
        }

        if b == b'"' || b == b'\'' {
            quote = Some(b);
            i += 1;
            continue;
        }

        if b == b'>' {
            return Some(i);
        }
        i += 1;
    }

    None
}

fn find_close_tag(input: &str, start: usize, tag: &str) -> Option<usize> {
    let needle = format!("</{}>", tag.to_ascii_lowercase());
    let lower = input[start..].to_ascii_lowercase();
    lower.find(&needle).map(|idx| start + idx)
}

fn escape_attr(value: &str) -> String {
    value
        .replace('&', "&amp;")
        .replace('"', "&quot;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_render_josieml_shorthand_and_logic_mapping() {
        let source = r##"<!DOCTYPE html>
<html>
<head><title>Demo</title></head>
<body>
  <div class="text-center p-4">
    <input j-model="client.identifier" />
    <button @click="sign_in">Sign in</button>
    <p j-text="client.status"></p>
  </div>
  <script type="application/josie+json">
  {
    "theme": {
      "colors": {"sidebar": "#1d75ca"},
      "space": [0,4,8,12,16,24,32,48,64],
      "radius": [0,2,4,8,16,9999],
      "font_size": {"xs":12,"sm":14,"base":16,"lg":20,"xl":24},
      "font_family": {"sans":"Inter, system-ui, sans-serif"},
      "shadows": {},
      "gradients": {},
      "transitions": {},
      "recipes": {},
      "variants": {}
    },
    "state": {"client": {"identifier": "", "status": ""}},
    "logic": {
      "sign_in": ["set", "client.status", "ok"]
    }
  }
  </script>
</body>
</html>"##;

        let html = render_josieml_with_runtime(source).expect("josieml should render");
        assert!(html.contains("data-josie-binds="));
        assert!(html.contains("client.identifier"));
        assert!(html.contains("w.event.value"));
        assert!(html.contains("data-josie-ons="));
        assert!(html.contains("client.status"));
        assert!(html.contains(".text-center{text-align:center;}"));
        assert!(html.contains("window.__JOSIE__"));
        assert!(html.contains("data-josie-tw"));
    }
}
