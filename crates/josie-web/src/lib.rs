#[cfg(feature = "axum")]
pub mod adapters;
pub mod engine;
mod rui;
pub mod runtime;
#[cfg(feature = "tailwind")]
pub mod tailwind;

#[cfg(feature = "axum")]
pub use adapters::axum::{AxumJosieAdapter, AxumJosieDataResolver};
pub use engine::{
    AssetRef, CachePolicy, Diagnostic, DiagnosticLevel, GroupConfig, JosieWebCore, JosieWebEngine,
    JosieWebServer, RenderOutput, ResourceDefinition, RouteJob, RouteJobConfig, SystemConfig,
    TemplateSlot, ViewTemplate, WebRequestContext,
};
pub use josie_core::State as CoreState;
pub use josie_core::{EvalError, EvalResult};
pub use rui::{
    HeadNode as JosieHeadNode, JosieDocument, JosieNode, Theme as JosieTheme, render_document,
    render_to_string,
};
pub use rui::{HeadNode, Theme};
pub use runtime::{
    WebCookieMutation, WebHttpPolicy, WebLogicOutput, WebResponseContext, WebRuntimeContext,
    clear_x_operator_registry, evaluate_web_logic, register_web_operators, register_x_operator,
    runtime_from_request, unregister_x_operator,
};

pub const JOSIE_RUNTIME_JS: &str = include_str!("josie-runtime.js");

pub fn render(doc: &JosieDocument) -> String {
    render_document(doc)
}

fn base_web_css(theme: &Theme) -> String {
    let font_sans = theme
        .font_family
        .get("sans")
        .cloned()
        .unwrap_or_else(|| "Inter, system-ui, sans-serif".to_string());
    let text = theme
        .colors
        .get("text")
        .cloned()
        .unwrap_or_else(|| "#0f172a".to_string());
    let text_inv = theme
        .colors
        .get("text-inv")
        .cloned()
        .unwrap_or_else(|| "#ffffff".to_string());
    let muted = theme
        .colors
        .get("muted")
        .cloned()
        .unwrap_or_else(|| "#64748b".to_string());
    let surface = theme
        .colors
        .get("surface")
        .cloned()
        .unwrap_or_else(|| "#ffffff".to_string());
    let surface_alt = theme
        .colors
        .get("surface-alt")
        .cloned()
        .unwrap_or_else(|| "#e2e8f0".to_string());
    let brand = theme
        .colors
        .get("brand")
        .cloned()
        .unwrap_or_else(|| "#2563eb".to_string());
    let brand_dark = theme
        .colors
        .get("brand-dark")
        .cloned()
        .unwrap_or_else(|| "#1d4ed8".to_string());

    format!(
        r#"
html, body {{
  margin: 0;
  padding: 0;
  width: 100%;
  min-height: 100%;
}}

#app, #app * {{
  box-sizing: border-box;
}}

#app {{
  margin: 0;
  padding: 0;
  width: 100%;
  min-height: 100vh;
  font-family: {};
  color: {};
  -webkit-font-smoothing: antialiased;
  text-rendering: optimizeLegibility;
}}

#app button,
#app input,
#app select,
#app textarea {{
  font: inherit;
  color: inherit;
  border: 1px solid {};
  border-radius: 10px;
  background: {};
  min-height: 40px;
  padding: 10px 12px;
  transition: border-color 0.15s ease, box-shadow 0.15s ease, background-color 0.15s ease;
}}

#app button {{
  background: {};
  border-color: {};
  color: {};
  font-weight: 600;
  padding: 10px 14px;
  cursor: pointer;
  transition: background-color 0.15s ease, border-color 0.15s ease, box-shadow 0.15s ease;
}}

#app button:hover {{
  background: {};
  border-color: {};
}}

#app input::placeholder,
#app textarea::placeholder {{
  color: {};
}}

#app button:focus-visible,
#app input:focus-visible,
#app select:focus-visible,
#app textarea:focus-visible {{
  border-color: {};
  outline: 2px solid {};
  outline-offset: 1px;
}}
"#,
        font_sans,
        text,
        surface_alt,
        surface,
        brand,
        brand,
        text_inv,
        brand_dark,
        brand_dark,
        muted,
        brand,
        brand
    )
}

pub fn render_with_runtime(doc: &JosieDocument) -> String {
    let mut html = render_document(doc);
    let theme_value = doc.theme.clone().unwrap_or_default();

    if let Some(head_pos) = html.find("</head>") {
        let before = &html[..head_pos];
        let after = &html[head_pos..];
        let style_tag = format!("<style>{}</style>", base_web_css(&theme_value));
        html = format!("{}{}{}", before, style_tag, after);
    }

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

    let runtime_pos = html.find("</body>");
    if let Some(pos) = runtime_pos {
        let before = &html[..pos];
        let after = &html[pos..];
        html = format!(
            "{}{}<script>{}</script>{}",
            before, state_script, JOSIE_RUNTIME_JS, after
        );
    }

    html
}

pub fn create_hello_world() -> JosieDocument {
    JosieDocument {
        version: Some("1.0".to_string()),
        theme: Some(Theme::default()),
        state: None,
        head: Some(HeadNode {
            title: Some("Hello World".to_string()),
        }),
        body: Some(vec![JosieNode::text("Hello, World!")]),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_render_with_runtime() {
        let doc = create_hello_world();
        let html = render_with_runtime(&doc);

        assert!(html.contains("<!DOCTYPE html>"));
        assert!(html.contains("<script>"));
        assert!(html.contains("</script>"));
    }

    #[test]
    fn test_hello_world() {
        let doc = create_hello_world();
        let html = doc.render();

        assert!(html.contains("Hello, World!"));
        assert!(html.contains("<title>Hello World</title>"));
    }

    #[test]
    fn test_josie_runtime_js_exists() {
        assert!(!JOSIE_RUNTIME_JS.is_empty());
        assert!(JOSIE_RUNTIME_JS.contains("JOSIE"));
        assert!(JOSIE_RUNTIME_JS.contains("def: (args, scope) => this.opDef(args, scope)"));
        assert!(JOSIE_RUNTIME_JS.contains("call: (args, scope) => this.opCall(args, scope)"));
        assert!(JOSIE_RUNTIME_JS.contains("effect: (args, scope) => this.opEffect(args, scope)"));
        assert!(JOSIE_RUNTIME_JS.contains("pipe: (args, scope) => this.opPipe(args, scope)"));
        assert!(!JOSIE_RUNTIME_JS.contains("$pipe"));
    }

    #[test]
    fn test_json_web_files() {
        let crate_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
        let examples_dir = crate_dir.join("../../examples");

        let read_dir = std::fs::read_dir(&examples_dir);
        if read_dir.is_err() {
            println!("Failed to read examples dir: {:?}", read_dir.err());
            return;
        }

        for entry in read_dir.unwrap() {
            let entry = entry.unwrap();
            let path = entry.path();
            if path.extension().map_or(false, |ext| ext == "json")
                && path
                    .file_name()
                    .map_or(false, |n| n.to_string_lossy().starts_with("web-"))
            {
                println!("Testing web: {:?}", path.file_name());
                let content = std::fs::read_to_string(&path).unwrap();
                let json: serde_json::Value = serde_json::from_str(&content).unwrap();

                if let Some(obj) = json.as_object() {
                    for (key, value) in obj {
                        println!("  Rendering {}...", key);

                        if let Ok(doc) = serde_json::from_value::<JosieDocument>(value.clone()) {
                            // Test basic render
                            let html = doc.render();
                            assert!(html.contains("<!DOCTYPE html>"), "Should have DOCTYPE");

                            // Test render with runtime
                            let html_with_runtime = render_with_runtime(&doc);
                            assert!(
                                html_with_runtime.contains("<script>"),
                                "Should have script tag"
                            );
                            assert!(
                                html_with_runtime.contains("</script>"),
                                "Should close script"
                            );

                            println!("    HTML length: {}", html.len());
                            println!("    HTML with runtime length: {}", html_with_runtime.len());
                        }
                    }
                }
            }
        }
    }
}
