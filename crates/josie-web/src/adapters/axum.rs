use crate::{
    CoreState, JosieWebCore, JosieWebEngine, RouteJobConfig, WebHttpPolicy, WebRequestContext,
    WebRuntimeContext, evaluate_web_logic, runtime_from_request,
};
use axum::Json;
use axum::http::{HeaderMap, HeaderValue, StatusCode, Uri};
use axum::response::{Html, IntoResponse, Redirect, Response};
use serde_json::{Map, Value, json};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

pub type AxumJosieDataResolver =
    Arc<dyn Fn(&crate::RouteJob, &WebRequestContext) -> Value + Send + Sync + 'static>;

#[derive(Clone)]
pub struct AxumJosieAdapter {
    engine: Arc<Mutex<JosieWebEngine>>,
    default_user: String,
    default_group: String,
    include_source_header: bool,
    data_resolver: AxumJosieDataResolver,
}

impl AxumJosieAdapter {
    pub fn new(engine: Arc<Mutex<JosieWebEngine>>) -> Self {
        Self {
            engine,
            default_user: "default".to_string(),
            default_group: "default".to_string(),
            include_source_header: true,
            data_resolver: Arc::new(default_data_resolver),
        }
    }

    pub fn with_defaults(mut self, user: impl Into<String>, group: impl Into<String>) -> Self {
        self.default_user = user.into();
        self.default_group = group.into();
        self
    }

    pub fn with_source_header(mut self, enabled: bool) -> Self {
        self.include_source_header = enabled;
        self
    }

    pub fn with_data_resolver<F>(mut self, resolver: F) -> Self
    where
        F: Fn(&crate::RouteJob, &WebRequestContext) -> Value + Send + Sync + 'static,
    {
        self.data_resolver = Arc::new(resolver);
        self
    }

    pub fn render_path(&self, method: &str, path: &str, headers: &HeaderMap) -> Response {
        let uri = path
            .parse::<Uri>()
            .unwrap_or_else(|_| Uri::from_static("/"));
        self.render_request(method, &uri, headers)
    }

    pub fn render_request(&self, method: &str, uri: &Uri, headers: &HeaderMap) -> Response {
        let user = header_or_default(headers, "x-josie-user", &self.default_user);
        let group = header_or_default(headers, "x-josie-group", &self.default_group);
        let route_path = uri.path().to_string();
        let mut req_ctx = WebRequestContext {
            method: method.to_ascii_uppercase(),
            path: route_path.clone(),
            body: Value::Null,
            user: user.clone(),
            group: group.clone(),
            params: HashMap::new(),
            query: parse_query(uri.query()),
            headers: headers_to_map(headers),
            cookies: parse_cookies(headers),
        };

        let mut engine = match self.engine.lock() {
            Ok(v) => v,
            Err(_) => {
                return (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    "failed to lock josie web engine",
                )
                    .into_response();
            }
        };

        let job = match engine.match_route(&user, &group, &route_path) {
            Ok(v) => v,
            Err(_) => return (StatusCode::NOT_FOUND, "not found").into_response(),
        };
        req_ctx.params = job.params.clone();

        let raw_data = (self.data_resolver)(&job, &req_ctx);
        let render_data = with_default_server_context(raw_data, &req_ctx);
        let view = match engine.compile_view(&job) {
            Ok(v) => v,
            Err(e) => {
                return (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    format!("route compile failed: {e}"),
                )
                    .into_response();
            }
        };

        let output = match engine.render_view(&view, &render_data) {
            Ok(out) => out,
            Err(e) => {
                return (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    format!("route render failed: {e}"),
                )
                    .into_response();
            }
        };

        let mut response = Html(output.html).into_response();
        if self.include_source_header {
            if let Ok(v) = HeaderValue::from_str(&job.file_path.display().to_string()) {
                response.headers_mut().insert("x-josie-source", v);
            }
        }
        response
    }

    pub fn execute_logic(
        &self,
        expression: &Value,
        request: WebRequestContext,
        state: Option<CoreState>,
        policy: Option<WebHttpPolicy>,
        http_mocks: Option<HashMap<String, Value>>,
    ) -> Result<crate::WebLogicOutput, String> {
        let mut runtime = runtime_from_request(request);
        if let Some(p) = policy.or_else(|| self.policy_for_request(&runtime.request)) {
            runtime.http_policy = p;
        }
        if let Some(mocks) = http_mocks {
            runtime.http_mocks = mocks;
        }
        let state = state.unwrap_or_default();
        evaluate_web_logic(expression, state, runtime).map_err(|e| e.message)
    }

    fn policy_for_request(&self, request: &WebRequestContext) -> Option<WebHttpPolicy> {
        let engine = self.engine.lock().ok()?;
        let job = engine
            .match_route(&request.user, &request.group, &request.path)
            .ok()?;
        Some(policy_from_route_config(&job.config))
    }

    pub fn web_response_to_axum(runtime: &WebRuntimeContext) -> Response {
        if let Some(to) = runtime.response.redirect.as_deref() {
            let mut response = Redirect::temporary(to).into_response();
            if runtime.response.status != 302 {
                *response.status_mut() =
                    StatusCode::from_u16(runtime.response.status).unwrap_or(StatusCode::FOUND);
            }
            apply_response_headers_and_cookies(&mut response, runtime);
            return response;
        }

        let mut response = match &runtime.response.body {
            Value::Null => "".into_response(),
            Value::String(s) => s.clone().into_response(),
            other => Json(other.clone()).into_response(),
        };

        *response.status_mut() =
            StatusCode::from_u16(runtime.response.status).unwrap_or(StatusCode::OK);
        apply_response_headers_and_cookies(&mut response, runtime);
        response
    }
}

fn default_data_resolver(_job: &crate::RouteJob, _ctx: &WebRequestContext) -> Value {
    Value::Object(Map::new())
}

fn policy_from_route_config(cfg: &RouteJobConfig) -> WebHttpPolicy {
    WebHttpPolicy {
        allow_patterns: cfg.http_allow_patterns.clone(),
        allow_methods: cfg.http_allow_methods.clone(),
        timeout_ms: cfg.http_timeout_ms,
    }
}

fn with_default_server_context(data: Value, req: &WebRequestContext) -> Value {
    let mut root = data.as_object().cloned().unwrap_or_default();

    if !root.contains_key("server") {
        root.insert(
            "server".to_string(),
            json!({
                "user": req.user,
                "group": req.group,
                "route": req.path,
                "method": req.method,
                "params": req.params,
                "query": req.query,
                "headers": req.headers,
                "cookies": req.cookies,
            }),
        );
    }

    Value::Object(root)
}

fn header_or_default(headers: &HeaderMap, key: &str, fallback: &str) -> String {
    headers
        .get(key)
        .and_then(|v| v.to_str().ok())
        .filter(|v| !v.trim().is_empty())
        .map(|v| v.to_string())
        .unwrap_or_else(|| fallback.to_string())
}

fn headers_to_map(headers: &HeaderMap) -> HashMap<String, String> {
    let mut out = HashMap::new();
    for (name, value) in headers {
        if let Ok(v) = value.to_str() {
            out.insert(name.as_str().to_string(), v.to_string());
        }
    }
    out
}

fn parse_query(raw: Option<&str>) -> HashMap<String, String> {
    let mut out = HashMap::new();
    let Some(query) = raw else {
        return out;
    };
    for pair in query.split('&') {
        if pair.is_empty() {
            continue;
        }
        let (key, value) = pair
            .split_once('=')
            .map(|(k, v)| (k, v))
            .unwrap_or((pair, ""));
        out.insert(key.to_string(), value.to_string());
    }
    out
}

fn parse_cookies(headers: &HeaderMap) -> HashMap<String, String> {
    let mut out = HashMap::new();
    let Some(raw) = headers.get("cookie").and_then(|v| v.to_str().ok()) else {
        return out;
    };

    for part in raw.split(';') {
        let trimmed = part.trim();
        if trimmed.is_empty() {
            continue;
        }
        let (name, value) = trimmed
            .split_once('=')
            .map(|(n, v)| (n.trim(), v.trim()))
            .unwrap_or((trimmed, ""));
        if !name.is_empty() {
            out.insert(name.to_string(), value.to_string());
        }
    }
    out
}

fn apply_response_headers_and_cookies(response: &mut Response, runtime: &WebRuntimeContext) {
    for (name, value) in &runtime.response.headers {
        if let (Ok(header_name), Ok(header_value)) = (
            axum::http::header::HeaderName::try_from(name.as_str()),
            HeaderValue::from_str(value),
        ) {
            response.headers_mut().insert(header_name, header_value);
        }
    }

    for cookie in &runtime.response.cookies {
        let mut serialized = String::new();
        if cookie.action == "clear" {
            serialized.push_str(&format!("{}=; Max-Age=0", cookie.name));
        } else {
            serialized.push_str(&format!("{}={}", cookie.name, cookie.value));
        }

        for (k, v) in &cookie.options {
            let key = k.trim();
            if key.is_empty() {
                continue;
            }
            match v {
                Value::Bool(true) => serialized.push_str(&format!("; {}", key)),
                Value::Bool(false) => {}
                Value::String(s) => serialized.push_str(&format!("; {}={}", key, s)),
                Value::Number(n) => serialized.push_str(&format!("; {}={}", key, n)),
                _ => {}
            }
        }
        if let Ok(v) = HeaderValue::from_str(&serialized) {
            response
                .headers_mut()
                .append(axum::http::header::SET_COOKIE, v);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{SystemConfig, WebRequestContext};
    use std::fs;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};

    #[test]
    fn adapter_default_injects_server_context() {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let root = std::env::temp_dir().join(format!("josie-axum-adapter-{nanos}"));
        fs::create_dir_all(&root).unwrap();
        fs::write(
            root.join("index.json"),
            r#"{
                "head": {"title":"Adapter Test"},
                "body":[{"type":"Text","props":{"children":"OK"}}]
            }"#,
        )
        .unwrap();

        let mut engine = JosieWebEngine::new();
        engine
            .init(SystemConfig::minimal(PathBuf::from(&root)))
            .unwrap();
        let adapter = AxumJosieAdapter::new(Arc::new(Mutex::new(engine)));
        let headers = HeaderMap::new();
        let response = adapter.render_path("GET", "/", &headers);
        assert_eq!(response.status(), StatusCode::OK);

        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn execute_logic_maps_runtime_response_to_axum() {
        let root = std::env::temp_dir().join("josie-axum-adapter-logic");
        let _ = fs::create_dir_all(&root);
        let mut engine = JosieWebEngine::new();
        engine
            .init(SystemConfig::minimal(PathBuf::from(&root)))
            .unwrap();
        let adapter = AxumJosieAdapter::new(Arc::new(Mutex::new(engine)));

        let expr = json!([
            "do",
            ["res.status", 201],
            ["cookie.set", "sid", "abc", {"Path": "/", "HttpOnly": true}],
            ["res.json", {"ok": true}]
        ]);
        let req = WebRequestContext {
            method: "POST".to_string(),
            path: "/api/login".to_string(),
            body: json!({"id":"u"}),
            user: "default".to_string(),
            group: "default".to_string(),
            params: HashMap::new(),
            query: HashMap::new(),
            headers: HashMap::new(),
            cookies: HashMap::new(),
        };
        let out = adapter
            .execute_logic(&expr, req, None, None, None)
            .expect("logic execution should succeed");
        let response = AxumJosieAdapter::web_response_to_axum(&out.runtime);
        assert_eq!(response.status(), StatusCode::CREATED);
        assert!(
            response
                .headers()
                .get("set-cookie")
                .and_then(|v| v.to_str().ok())
                .unwrap_or_default()
                .contains("sid=abc")
        );

        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn execute_logic_uses_route_merged_http_policy_by_default() {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let root = std::env::temp_dir().join(format!("josie-axum-adapter-policy-{nanos}"));
        fs::create_dir_all(root.join("default")).unwrap();
        fs::write(
            root.join("default").join("index.josieml"),
            "<!DOCTYPE html><html><head><title>T</title></head><body>OK</body></html>",
        )
        .unwrap();

        let mut cfg = SystemConfig::minimal(root.clone());
        cfg.groups
            .get_mut("default")
            .expect("default group should exist")
            .http_allow_patterns = vec!["https://api.example.com/*".to_string()];

        let mut engine = JosieWebEngine::new();
        engine.init(cfg).unwrap();
        let adapter = AxumJosieAdapter::new(Arc::new(Mutex::new(engine)));

        let req = WebRequestContext {
            method: "GET".to_string(),
            path: "/".to_string(),
            body: Value::Null,
            user: "default".to_string(),
            group: "default".to_string(),
            params: HashMap::new(),
            query: HashMap::new(),
            headers: HashMap::new(),
            cookies: HashMap::new(),
        };

        let mut mocks = HashMap::new();
        mocks.insert(
            "GET https://api.example.com/ping".to_string(),
            json!({"ok": true, "source": "mock"}),
        );

        let expr = json!(["http.get", "https://api.example.com/ping"]);
        let out = adapter
            .execute_logic(&expr, req, None, None, Some(mocks))
            .expect("logic execution should apply route-derived policy");
        assert_eq!(out.value["ok"], Value::Bool(true));

        let _ = fs::remove_dir_all(&root);
    }
}
