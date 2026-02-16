use josie_core::{Context, Operators, State, evaluate};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Theme {
    pub colors: HashMap<String, String>,
    pub space: Vec<i32>,
    pub radius: Vec<i32>,
    pub font_size: HashMap<String, i32>,
    pub font_family: HashMap<String, String>,
    pub shadows: HashMap<String, String>,
    pub gradients: HashMap<String, Vec<String>>,
    pub transitions: HashMap<String, String>,
    pub recipes: HashMap<String, HashMap<String, Value>>,
    pub variants: HashMap<String, HashMap<String, Value>>,
}

impl Default for Theme {
    fn default() -> Self {
        let mut colors = HashMap::new();
        colors.insert("brand".to_string(), "#2563eb".to_string());
        colors.insert("brand-dark".to_string(), "#1d4ed8".to_string());
        colors.insert("danger".to_string(), "#dc2626".to_string());
        colors.insert("muted".to_string(), "#64748b".to_string());
        colors.insert("surface".to_string(), "#f8fafc".to_string());
        colors.insert("text".to_string(), "#0f172a".to_string());
        colors.insert("text-inv".to_string(), "#ffffff".to_string());

        let space = vec![0, 4, 8, 12, 16, 24, 32, 48, 64];
        let radius = vec![0, 2, 4, 8, 16, 9999];

        let mut font_size = HashMap::new();
        font_size.insert("xs".to_string(), 12);
        font_size.insert("sm".to_string(), 14);
        font_size.insert("base".to_string(), 16);
        font_size.insert("lg".to_string(), 20);
        font_size.insert("xl".to_string(), 24);

        let mut font_family = HashMap::new();
        font_family.insert(
            "sans".to_string(),
            "Inter, system-ui, sans-serif".to_string(),
        );

        let mut shadows = HashMap::new();
        shadows.insert("sm".to_string(), "0 1px 2px rgba(0,0,0,0.05)".to_string());
        shadows.insert("md".to_string(), "0 4px 12px rgba(0,0,0,0.08)".to_string());
        shadows.insert("lg".to_string(), "0 12px 32px rgba(0,0,0,0.12)".to_string());

        Self {
            colors,
            space,
            radius,
            font_size,
            font_family,
            shadows,
            gradients: HashMap::new(),
            transitions: HashMap::new(),
            recipes: HashMap::new(),
            variants: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JosieNode {
    #[serde(rename = "type")]
    pub node_type: String,
    pub props: Option<HashMap<String, Value>>,
    pub style: Option<HashMap<String, Value>>,
    pub bind: Option<HashMap<String, Value>>,
    pub on: Option<HashMap<String, Value>>,
    pub children: Option<Vec<JosieNode>>,
    #[serde(rename = "ssr")]
    pub is_ssr: Option<bool>,
    pub client: Option<bool>,
    pub source: Option<Value>,
    #[serde(rename = "each")]
    pub each_item: Option<Box<JosieNode>>,
    pub filter: Option<Value>,
}

impl JosieNode {
    pub fn text(content: &str) -> Self {
        Self {
            node_type: "Text".to_string(),
            props: Some(
                [("children".to_string(), Value::String(content.to_string()))]
                    .into_iter()
                    .collect(),
            ),
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
}

pub struct RenderContext<'a> {
    pub state: &'a mut State,
    pub operators: &'a Operators,
    pub theme: &'a Theme,
}

pub fn render_to_string(node: &JosieNode, ctx: &mut RenderContext) -> String {
    match node.node_type.as_str() {
        "Text" => render_text(node, ctx),
        "Button" => render_button(node, ctx),
        "Input" => render_input(node, ctx),
        "Image" => render_image(node, ctx),
        "Stack" => render_stack(node, ctx),
        "Grid" => render_grid(node, ctx),
        "Fragment" => render_fragment(node, ctx),
        "map" => render_map(node, ctx),
        _ => format!("<!-- unknown node type: {} -->", node.node_type),
    }
}

fn render_text(node: &JosieNode, ctx: &mut RenderContext) -> String {
    let mut content = String::new();

    if let Some(props) = &node.props {
        if let Some(children) = props.get("children") {
            let mut eval_ctx = Context {
                state: ctx.state,
                operators: ctx.operators,
                event: None,
            };
            let result = evaluate(children, &mut eval_ctx).unwrap_or(children.clone());
            content = value_to_string(&result);
        }
    }

    if let Some(bind) = &node.bind {
        if let Some(children_expr) = bind.get("children") {
            let mut eval_ctx = Context {
                state: ctx.state,
                operators: ctx.operators,
                event: None,
            };
            let result = evaluate(children_expr, &mut eval_ctx).unwrap_or(Value::Null);
            content = value_to_string(&result);
        }
    }

    let style = render_style(node, ctx);
    let josie_attrs = render_josie_attrs(node);
    let class_attr = render_class_attr(node);

    if josie_attrs.is_empty() {
        format!(
            "<span style=\"{}\"{}>{}</span>",
            style,
            class_attr,
            escape_html(&content)
        )
    } else {
        format!(
            "<span style=\"{}\"{}{}>{}</span>",
            style,
            class_attr,
            josie_attrs,
            escape_html(&content)
        )
    }
}

fn value_to_string(v: &Value) -> String {
    match v {
        Value::String(s) => s.clone(),
        Value::Number(n) => n.to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Null => String::new(),
        _ => v.to_string(),
    }
}

fn render_josie_attrs(node: &JosieNode) -> String {
    let mut attrs = String::new();

    if let Some(bind) = &node.bind {
        let bind_json = serde_json::to_string(bind).unwrap_or_else(|_| "{}".to_string());
        attrs.push_str(&format!(
            " data-josie-binds=\"{}\"",
            escape_html(&bind_json)
        ));
    }

    if let Some(on) = &node.on {
        let on_json = serde_json::to_string(on).unwrap_or_else(|_| "{}".to_string());
        attrs.push_str(&format!(" data-josie-ons=\"{}\"", escape_html(&on_json)));
    }

    attrs
}

fn render_class_attr(node: &JosieNode) -> String {
    node.props
        .as_ref()
        .and_then(|props| props.get("className"))
        .and_then(Value::as_str)
        .map(|class_name| class_name.trim())
        .filter(|class_name| !class_name.is_empty())
        .map(|class_name| format!(" class=\"{}\"", escape_html(class_name)))
        .unwrap_or_default()
}

fn render_button(node: &JosieNode, ctx: &mut RenderContext) -> String {
    let style_str = render_style(node, ctx);
    let josie_attrs = render_josie_attrs(node);
    let class_attr = render_class_attr(node);

    // Try children field first, then props.children
    let children = node
        .children
        .as_ref()
        .map(|c| {
            c.iter()
                .map(|n| render_to_string(n, ctx))
                .collect::<Vec<_>>()
                .join("")
        })
        .or_else(|| {
            node.props
                .as_ref()
                .and_then(|p| p.get("children").map(|v| value_to_string(v)))
        })
        .unwrap_or_default();

    if style_str.is_empty() {
        format!("<button{}{}>{}</button>", class_attr, josie_attrs, children)
    } else {
        format!(
            "<button style=\"{}\"{}{}>{}</button>",
            style_str, class_attr, josie_attrs, children
        )
    }
}

fn render_input(node: &JosieNode, ctx: &mut RenderContext) -> String {
    let mut attrs = String::new();
    let style_str = render_style(node, ctx);
    let josie_attrs = render_josie_attrs(node);
    let class_attr = render_class_attr(node);

    if let Some(props) = &node.props {
        if let Some(placeholder) = props.get("placeholder") {
            if let Some(s) = placeholder.as_str() {
                attrs.push_str(&format!(" placeholder=\"{}\"", escape_html(s)));
            }
        }
        if let Some(value) = props.get("value") {
            let val = value_to_string(value);
            attrs.push_str(&format!(" value=\"{}\"", escape_html(&val)));
        }
    }

    if let Some(bind) = &node.bind {
        if let Some(value_expr) = bind.get("value") {
            let mut eval_ctx = Context {
                state: ctx.state,
                operators: ctx.operators,
                event: None,
            };
            let result = evaluate(value_expr, &mut eval_ctx).unwrap_or(Value::Null);
            let val = value_to_string(&result);
            attrs.push_str(&format!(" value=\"{}\"", escape_html(&val)));
        }
    }

    if style_str.is_empty() {
        format!("<input{}{}{} />", class_attr, attrs, josie_attrs)
    } else {
        format!(
            "<input style=\"{}\"{}{}{} />",
            style_str, class_attr, attrs, josie_attrs
        )
    }
}

fn render_image(node: &JosieNode, ctx: &mut RenderContext) -> String {
    let mut attrs = String::new();
    let style_str = render_style(node, ctx);
    let josie_attrs = render_josie_attrs(node);
    let class_attr = render_class_attr(node);

    if let Some(props) = &node.props {
        if let Some(src) = props.get("src").and_then(Value::as_str) {
            attrs.push_str(&format!(" src=\"{}\"", escape_html(src)));
        }
        if let Some(alt) = props.get("alt").and_then(Value::as_str) {
            attrs.push_str(&format!(" alt=\"{}\"", escape_html(alt)));
        } else {
            attrs.push_str(" alt=\"\"");
        }

        if let Some(width) = props.get("width") {
            let width_str = value_to_string(width);
            if !width_str.is_empty() {
                attrs.push_str(&format!(" width=\"{}\"", escape_html(&width_str)));
            }
        }

        if let Some(height) = props.get("height") {
            let height_str = value_to_string(height);
            if !height_str.is_empty() {
                attrs.push_str(&format!(" height=\"{}\"", escape_html(&height_str)));
            }
        }
    } else {
        attrs.push_str(" alt=\"\"");
    }

    if style_str.is_empty() {
        format!("<img{}{}{} />", class_attr, attrs, josie_attrs)
    } else {
        format!(
            "<img style=\"{}\"{}{}{} />",
            style_str, class_attr, attrs, josie_attrs
        )
    }
}

fn render_stack(node: &JosieNode, ctx: &mut RenderContext) -> String {
    let mut style = String::new();
    let josie_attrs = render_josie_attrs(node);
    let class_attr = render_class_attr(node);

    if let Some(props) = &node.props {
        if let Some(direction) = props.get("direction") {
            let dir = direction.as_str().unwrap_or("row");
            style.push_str(&format!("display: flex; flex-direction: {}; ", dir));
        }
        if let Some(gap) = props.get("gap") {
            if let Some(gap_val) = gap.as_i64() {
                let gap_px = ctx.theme.space.get(gap_val as usize).unwrap_or(&8);
                style.push_str(&format!("gap: {}px; ", gap_px));
            }
        }
        if let Some(align) = props.get("align") {
            style.push_str(&format!(
                "align-items: {}; ",
                align.as_str().unwrap_or("stretch")
            ));
        }
        if let Some(justify) = props.get("justify") {
            style.push_str(&format!(
                "justify-content: {}; ",
                justify.as_str().unwrap_or("start")
            ));
        }
    }

    style.push_str(&render_style(node, ctx));

    let children = node
        .children
        .as_ref()
        .map(|c| {
            c.iter()
                .map(|n| render_to_string(n, ctx))
                .collect::<Vec<_>>()
                .join("")
        })
        .unwrap_or_default();

    if style.is_empty() {
        format!("<div{}{}>{}</div>", class_attr, josie_attrs, children)
    } else {
        format!(
            "<div style=\"{}\"{}{}>{}</div>",
            style, class_attr, josie_attrs, children
        )
    }
}

fn render_grid(node: &JosieNode, ctx: &mut RenderContext) -> String {
    let mut style = String::new();
    let josie_attrs = render_josie_attrs(node);
    let class_attr = render_class_attr(node);
    style.push_str("display: grid; ");

    if let Some(props) = &node.props {
        if let Some(cols) = props.get("cols") {
            style.push_str(&format!(
                "grid-template-columns: repeat({}, 1fr); ",
                cols.as_i64().unwrap_or(1)
            ));
        }
        if let Some(gap) = props.get("gap") {
            if let Some(gap_val) = gap.as_i64() {
                let gap_px = ctx.theme.space.get(gap_val as usize).unwrap_or(&8);
                style.push_str(&format!("gap: {}px; ", gap_px));
            }
        }
    }

    style.push_str(&render_style(node, ctx));

    let children = node
        .children
        .as_ref()
        .map(|c| {
            c.iter()
                .map(|n| render_to_string(n, ctx))
                .collect::<Vec<_>>()
                .join("")
        })
        .unwrap_or_default();

    format!(
        "<div style=\"{}\"{}{}>{}</div>",
        style, class_attr, josie_attrs, children
    )
}

fn render_fragment(node: &JosieNode, ctx: &mut RenderContext) -> String {
    node.children
        .as_ref()
        .map(|c| {
            c.iter()
                .map(|n| render_to_string(n, ctx))
                .collect::<Vec<_>>()
                .join("")
        })
        .unwrap_or_default()
}

fn render_map(node: &JosieNode, ctx: &mut RenderContext) -> String {
    let source = match &node.source {
        Some(s) => s.clone(),
        None => return String::new(),
    };

    let mut eval_ctx = Context {
        state: ctx.state,
        operators: ctx.operators,
        event: None,
    };

    let list = match evaluate(&source, &mut eval_ctx) {
        Ok(Value::Array(arr)) => arr,
        _ => return String::new(),
    };

    let filter = node.filter.as_ref();
    let each_template = node.each_item.as_ref();

    let mut results = Vec::new();
    for item in list {
        if let Some(filter_expr) = filter {
            let mut filter_ctx = Context {
                state: ctx.state,
                operators: ctx.operators,
                event: None,
            };
            if let Some(prev_item) = filter_ctx.state.client.get("item").cloned() {
                filter_ctx
                    .state
                    .client
                    .insert("parentItem".to_string(), prev_item);
            }
            filter_ctx
                .state
                .client
                .insert("item".to_string(), item.clone());
            let matched = evaluate(filter_expr, &mut filter_ctx).unwrap_or(Value::Bool(false));
            if !matched.as_bool().unwrap_or(false) {
                continue;
            }
        }

        if let Some(template) = each_template {
            let item_ctx = Context {
                state: ctx.state,
                operators: ctx.operators,
                event: None,
            };
            if let Some(prev_item) = item_ctx.state.client.get("item").cloned() {
                item_ctx
                    .state
                    .client
                    .insert("parentItem".to_string(), prev_item);
            }
            item_ctx.state.client.insert("item".to_string(), item);
            let mut render_ctx = RenderContext {
                state: item_ctx.state,
                operators: ctx.operators,
                theme: ctx.theme,
            };
            results.push(render_to_string(template, &mut render_ctx));
        }
    }

    let mut map_config = serde_json::Map::new();
    map_config.insert("source".to_string(), source);
    if let Some(filter_expr) = filter {
        map_config.insert("filter".to_string(), filter_expr.clone());
    }
    if let Some(template) = each_template {
        map_config.insert(
            "each".to_string(),
            serde_json::to_value(template).unwrap_or(Value::Null),
        );
    }

    let map_json =
        serde_json::to_string(&Value::Object(map_config)).unwrap_or_else(|_| "{}".to_string());
    let style = render_style(node, ctx);
    let josie_attrs = render_josie_attrs(node);
    let class_attr = render_class_attr(node);

    if style.is_empty() {
        format!(
            "<div{} data-josie-map=\"{}\"{}>{}</div>",
            class_attr,
            escape_html(&map_json),
            josie_attrs,
            results.join("")
        )
    } else {
        format!(
            "<div style=\"{}\"{} data-josie-map=\"{}\"{}>{}</div>",
            style,
            class_attr,
            escape_html(&map_json),
            josie_attrs,
            results.join("")
        )
    }
}

fn render_style(node: &JosieNode, ctx: &mut RenderContext) -> String {
    let mut style = String::new();
    let theme = ctx.theme;

    if let Some(style_obj) = &node.style {
        for (k, v) in style_obj {
            style.push_str(&format_style_prop(k, v, theme));
        }
    }

    style
}

fn format_style_prop(key: &str, value: &Value, theme: &Theme) -> String {
    fn space_px(value: &Value, theme: &Theme) -> Option<String> {
        value
            .as_i64()
            .and_then(|i| theme.space.get(i as usize).copied())
            .map(|px| format!("{}px", px))
    }

    let val_str = match value {
        Value::String(s) => s.clone(),
        Value::Number(n) => n.to_string(),
        Value::Bool(b) => b.to_string(),
        _ => value.to_string(),
    };

    match key {
        "bg" => {
            if let Some(color) = theme.colors.get(&val_str) {
                format!("background-color: {}; ", color)
            } else {
                format!("background-color: {}; ", val_str)
            }
        }
        "fg" => {
            if let Some(color) = theme.colors.get(&val_str) {
                format!("color: {}; ", color)
            } else {
                format!("color: {}; ", val_str)
            }
        }
        "fontSize" => {
            if let Some(size) = theme.font_size.get(&val_str) {
                format!("font-size: {}px; ", size)
            } else {
                format!("font-size: {}; ", val_str)
            }
        }
        "fontWeight" => format!("font-weight: {}; ", val_str),
        "radius" => {
            if let Some(r) = value.as_i64() {
                let radius = theme.radius.get(r as usize).unwrap_or(&0);
                if *radius == 9999 {
                    format!("border-radius: 9999px; ")
                } else {
                    format!("border-radius: {}px; ", radius)
                }
            } else {
                format!("border-radius: {}; ", val_str)
            }
        }
        "opacity" => format!("opacity: {}; ", val_str),
        "p" => space_px(value, theme)
            .map(|v| format!("padding: {}; ", v))
            .unwrap_or_default(),
        "px" => space_px(value, theme)
            .map(|v| format!("padding-left: {}; padding-right: {}; ", v, v))
            .unwrap_or_default(),
        "py" => space_px(value, theme)
            .map(|v| format!("padding-top: {}; padding-bottom: {}; ", v, v))
            .unwrap_or_default(),
        "pt" => space_px(value, theme)
            .map(|v| format!("padding-top: {}; ", v))
            .unwrap_or_default(),
        "pr" => space_px(value, theme)
            .map(|v| format!("padding-right: {}; ", v))
            .unwrap_or_default(),
        "pb" => space_px(value, theme)
            .map(|v| format!("padding-bottom: {}; ", v))
            .unwrap_or_default(),
        "pl" => space_px(value, theme)
            .map(|v| format!("padding-left: {}; ", v))
            .unwrap_or_default(),
        "m" => space_px(value, theme)
            .map(|v| format!("margin: {}; ", v))
            .unwrap_or_default(),
        "mx" => space_px(value, theme)
            .map(|v| format!("margin-left: {}; margin-right: {}; ", v, v))
            .unwrap_or_default(),
        "my" => space_px(value, theme)
            .map(|v| format!("margin-top: {}; margin-bottom: {}; ", v, v))
            .unwrap_or_default(),
        "mt" => space_px(value, theme)
            .map(|v| format!("margin-top: {}; ", v))
            .unwrap_or_default(),
        "mr" => space_px(value, theme)
            .map(|v| format!("margin-right: {}; ", v))
            .unwrap_or_default(),
        "mb" => space_px(value, theme)
            .map(|v| format!("margin-bottom: {}; ", v))
            .unwrap_or_default(),
        "ml" => space_px(value, theme)
            .map(|v| format!("margin-left: {}; ", v))
            .unwrap_or_default(),
        "gap" => space_px(value, theme)
            .map(|v| format!("gap: {}; ", v))
            .unwrap_or_default(),
        "hidden" => {
            if value.as_bool().unwrap_or(false) {
                "display: none; ".to_string()
            } else {
                String::new()
            }
        }
        "maxWidth" => format!("max-width: {}px; ", val_str),
        "width" => {
            if value.is_number() {
                format!("width: {}px; ", val_str)
            } else {
                format!("width: {}; ", val_str)
            }
        }
        "height" => {
            if value.is_number() {
                format!("height: {}px; ", val_str)
            } else {
                format!("height: {}; ", val_str)
            }
        }
        "minHeight" => {
            if value.is_number() {
                format!("min-height: {}px; ", val_str)
            } else {
                format!("min-height: {}; ", val_str)
            }
        }
        "textAlign" => format!("text-align: {}; ", val_str),
        "display" => format!("display: {}; ", val_str),
        "position" => format!("position: {}; ", val_str),
        "z" => format!("z-index: {}; ", val_str),
        "cursor" => format!("cursor: {}; ", val_str),
        "tracking" => format!("letter-spacing: {}; ", val_str),
        "leading" => format!("line-height: {}; ", val_str),
        "animation" => format!("animation: {}; ", val_str),
        "animationDuration" => format!("animation-duration: {}; ", val_str),
        "animationTimingFunction" => format!("animation-timing-function: {}; ", val_str),
        "animationIterationCount" => format!("animation-iteration-count: {}; ", val_str),
        "transform" => format!("text-transform: {}; ", val_str),
        "aspect" => format!("aspect-ratio: {}; ", val_str),
        "transition" => {
            if let Some(t) = theme.transitions.get(&val_str) {
                format!("transition: {}; ", t)
            } else {
                format!("transition: {}; ", val_str)
            }
        }
        "border" => {
            if let Some(color) = theme.colors.get(&val_str) {
                format!("border: 1px solid {}; ", color)
            } else {
                format!("border: {}; ", val_str)
            }
        }
        "overflow" => format!("overflow: {}; ", val_str),
        "flex" => format!("flex: {}; ", val_str),
        _ => String::new(),
    }
}

fn escape_html(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&#39;")
}

pub fn render_document(doc: &JosieDocument) -> String {
    let operators = Operators::new();
    let mut state = doc.state.clone().unwrap_or_else(State::new);
    let theme = doc.theme.clone().unwrap_or_default();

    let mut ctx = RenderContext {
        state: &mut state,
        operators: &operators,
        theme: &theme,
    };

    let mut html = String::new();
    html.push_str("<!DOCTYPE html>\n<html>\n<head>\n");
    html.push_str("<meta charset=\"utf-8\">\n");
    html.push_str("<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n");

    if let Some(head) = &doc.head {
        if let Some(title) = &head.title {
            html.push_str(&format!("<title>{}</title>\n", escape_html(title)));
        }
    }

    html.push_str("</head>\n<body>\n");

    if let Some(body) = &doc.body {
        html.push_str("<div id=\"app\">\n");
        for child in body {
            html.push_str(&render_to_string(child, &mut ctx));
        }
        html.push_str("</div>\n");
    }

    html.push_str("</body>\n</html>");

    html
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HeadNode {
    pub title: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JosieDocument {
    pub version: Option<String>,
    pub theme: Option<Theme>,
    pub state: Option<State>,
    pub head: Option<HeadNode>,
    pub body: Option<Vec<JosieNode>>,
}

impl JosieDocument {
    pub fn new() -> Self {
        Self {
            version: Some("1.0".to_string()),
            theme: Some(Theme::default()),
            state: None,
            head: None,
            body: None,
        }
    }

    pub fn from_json(json: &str) -> Result<Self, serde_json::Error> {
        serde_json::from_str(json)
    }

    pub fn from_file(path: &std::path::Path) -> Result<Self, std::io::Error> {
        let content = std::fs::read_to_string(path)?;
        Ok(serde_json::from_str(&content).unwrap())
    }

    pub fn render(&self) -> String {
        render_document(self)
    }
}

impl Default for JosieDocument {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_render_text() {
        let node = JosieNode::text("Hello World");
        let theme = Theme::default();
        let mut state = State::new();
        let operators = Operators::new();

        let mut ctx = RenderContext {
            state: &mut state,
            operators: &operators,
            theme: &theme,
        };

        let html = render_to_string(&node, &mut ctx);
        assert!(html.contains("<span"));
        assert!(html.contains("Hello World"));
    }

    #[test]
    fn test_render_button() {
        let node = JosieNode {
            node_type: "Button".to_string(),
            props: Some(
                [("children".to_string(), json!("Click me"))]
                    .into_iter()
                    .collect(),
            ),
            style: None,
            bind: None,
            on: None,
            children: None,
            is_ssr: None,
            client: None,
            source: None,
            each_item: None,
            filter: None,
        };

        let theme = Theme::default();
        let mut state = State::new();
        let operators = Operators::new();

        let mut ctx = RenderContext {
            state: &mut state,
            operators: &operators,
            theme: &theme,
        };

        let html = render_to_string(&node, &mut ctx);
        assert!(html.contains("<button"));
        assert!(html.contains("Click me"));
    }

    #[test]
    fn test_render_stack() {
        let node = JosieNode {
            node_type: "Stack".to_string(),
            props: Some(
                [
                    ("direction".to_string(), json!("column")),
                    ("gap".to_string(), json!(2)),
                ]
                .into_iter()
                .collect(),
            ),
            style: None,
            bind: None,
            on: None,
            children: Some(vec![JosieNode::text("Item 1"), JosieNode::text("Item 2")]),
            is_ssr: None,
            client: None,
            source: None,
            each_item: None,
            filter: None,
        };

        let theme = Theme::default();
        let mut state = State::new();
        let operators = Operators::new();

        let mut ctx = RenderContext {
            state: &mut state,
            operators: &operators,
            theme: &theme,
        };

        let html = render_to_string(&node, &mut ctx);
        assert!(html.contains("flex-direction: column"));
        assert!(html.contains("gap: 8px"));
    }

    #[test]
    fn test_render_image() {
        let node = JosieNode {
            node_type: "Image".to_string(),
            props: Some(
                [
                    ("src".to_string(), json!("/logo.svg")),
                    ("alt".to_string(), json!("Logo")),
                    ("width".to_string(), json!(48)),
                ]
                .into_iter()
                .collect(),
            ),
            style: None,
            bind: None,
            on: None,
            children: None,
            is_ssr: None,
            client: None,
            source: None,
            each_item: None,
            filter: None,
        };

        let theme = Theme::default();
        let mut state = State::new();
        let operators = Operators::new();

        let mut ctx = RenderContext {
            state: &mut state,
            operators: &operators,
            theme: &theme,
        };

        let html = render_to_string(&node, &mut ctx);
        assert!(html.contains("<img"));
        assert!(html.contains("src=\"/logo.svg\""));
        assert!(html.contains("alt=\"Logo\""));
        assert!(html.contains("width=\"48\""));
    }

    #[test]
    fn test_document_render() {
        let doc = JosieDocument {
            version: Some("1.0".to_string()),
            theme: Some(Theme::default()),
            state: None,
            head: Some(HeadNode {
                title: Some("Test Page".to_string()),
            }),
            body: Some(vec![JosieNode::text("Hello World")]),
        };

        let html = doc.render();
        assert!(html.contains("<!DOCTYPE html>"));
        assert!(html.contains("<title>Test Page</title>"));
        assert!(html.contains("Hello World"));
    }

    #[test]
    fn test_json_ui_files() {
        let crate_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
        let examples_dir = crate_dir.join("../../examples");

        let read_dir = std::fs::read_dir(&examples_dir);
        if read_dir.is_err() {
            println!("Failed to read examples dir: {:?}", read_dir.err());
            return;
        }

        let theme = Theme::default();
        let mut state = State::new();
        let operators = Operators::new();

        for entry in read_dir.unwrap() {
            let entry = entry.unwrap();
            let path = entry.path();
            if path.extension().map_or(false, |ext| ext == "json")
                && path
                    .file_name()
                    .map_or(false, |n| n.to_string_lossy().starts_with("ui-"))
            {
                println!("Testing UI: {:?}", path.file_name());
                let content = std::fs::read_to_string(&path).unwrap();
                let json: serde_json::Value = serde_json::from_str(&content).unwrap();

                if let Some(obj) = json.as_object() {
                    for (key, value) in obj {
                        println!("  Rendering {}...", key);

                        // Try to parse as JosieDocument first
                        if let Ok(doc) = serde_json::from_value::<JosieDocument>(value.clone()) {
                            let html = doc.render();
                            println!("    Document HTML length: {}", html.len());
                            assert!(
                                html.contains("<!DOCTYPE html>"),
                                "Document should have DOCTYPE"
                            );
                            continue;
                        }

                        // Try to parse as JosieNode
                        if let Ok(node) = serde_json::from_value::<JosieNode>(value.clone()) {
                            let mut ctx = RenderContext {
                                state: &mut state,
                                operators: &operators,
                                theme: &theme,
                            };
                            let html = render_to_string(&node, &mut ctx);
                            println!("    Node HTML: {}", html);
                            assert!(!html.is_empty(), "Node should render non-empty HTML");
                        }
                    }
                }
            }
        }
    }
}
