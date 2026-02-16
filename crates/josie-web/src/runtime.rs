use crate::WebRequestContext;
use josie_core::{Context, EvalError, EvalResult, Operator, Operators, State, evaluate};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value, json};
use std::collections::HashMap;
use std::sync::{OnceLock, RwLock};
use std::time::Duration;

const WEB_RUNTIME_KEY: &str = "__web";
static X_OPERATOR_REGISTRY: OnceLock<RwLock<HashMap<String, Operator>>> = OnceLock::new();

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct WebHttpPolicy {
    #[serde(default)]
    pub allow_patterns: Vec<String>,
    #[serde(default = "default_http_methods")]
    pub allow_methods: Vec<String>,
    #[serde(default = "default_http_timeout_ms")]
    pub timeout_ms: u64,
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

impl Default for WebHttpPolicy {
    fn default() -> Self {
        Self {
            allow_patterns: Vec::new(),
            allow_methods: default_http_methods(),
            timeout_ms: default_http_timeout_ms(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct WebCookieMutation {
    pub action: String,
    pub name: String,
    #[serde(default)]
    pub value: String,
    #[serde(default)]
    pub options: Map<String, Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct WebResponseContext {
    #[serde(default = "default_status_ok")]
    pub status: u16,
    #[serde(default)]
    pub headers: HashMap<String, String>,
    #[serde(default)]
    pub body: Value,
    pub redirect: Option<String>,
    #[serde(default)]
    pub cookies: Vec<WebCookieMutation>,
}

fn default_status_ok() -> u16 {
    200
}

impl Default for WebResponseContext {
    fn default() -> Self {
        Self {
            status: default_status_ok(),
            headers: HashMap::new(),
            body: Value::Null,
            redirect: None,
            cookies: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct WebRuntimeContext {
    #[serde(default)]
    pub request: WebRequestContext,
    #[serde(default)]
    pub response: WebResponseContext,
    #[serde(default)]
    pub http_policy: WebHttpPolicy,
    #[serde(default)]
    pub http_mocks: HashMap<String, Value>,
}

#[derive(Debug, Clone)]
pub struct WebLogicOutput {
    pub value: Value,
    pub state: State,
    pub runtime: WebRuntimeContext,
}

pub fn evaluate_web_logic(
    expression: &Value,
    mut state: State,
    mut runtime: WebRuntimeContext,
) -> Result<WebLogicOutput, EvalError> {
    set_runtime_on_state(&mut state, &runtime)?;
    let mut operators = Operators::new();
    register_web_operators(&mut operators);
    let mut ctx = Context {
        state: &mut state,
        operators: &operators,
        event: None,
    };
    let value = evaluate(expression, &mut ctx)?;
    runtime = get_runtime_from_state(ctx.state)?;
    Ok(WebLogicOutput {
        value,
        state,
        runtime,
    })
}

pub fn runtime_from_request(request: WebRequestContext) -> WebRuntimeContext {
    WebRuntimeContext {
        request,
        response: WebResponseContext::default(),
        http_policy: WebHttpPolicy::default(),
        http_mocks: HashMap::new(),
    }
}

pub fn register_web_operators(operators: &mut Operators) {
    operators.register("req.method", op_req_method as Operator);
    operators.register("req.path", op_req_path as Operator);
    operators.register("req.body", op_req_body as Operator);
    operators.register("req.param", op_req_param as Operator);
    operators.register("req.query", op_req_query as Operator);
    operators.register("req.header", op_req_header as Operator);
    operators.register("req.cookie", op_req_cookie as Operator);

    operators.register("res.status", op_res_status as Operator);
    operators.register("res.header", op_res_header as Operator);
    operators.register("res.json", op_res_json as Operator);
    operators.register("res.text", op_res_text as Operator);
    operators.register("res.redirect", op_res_redirect as Operator);
    operators.register("cookie.set", op_cookie_set as Operator);
    operators.register("cookie.clear", op_cookie_clear as Operator);

    operators.register("http.request", op_http_request as Operator);
    operators.register("http.get", op_http_get as Operator);
    operators.register("http.post", op_http_post as Operator);
    operators.register("http.put", op_http_put as Operator);
    operators.register("http.delete", op_http_delete as Operator);

    if let Ok(registry) = x_operator_registry().read() {
        for (name, op) in registry.iter() {
            operators.register(name.clone(), *op);
        }
    }
}

fn x_operator_registry() -> &'static RwLock<HashMap<String, Operator>> {
    X_OPERATOR_REGISTRY.get_or_init(|| RwLock::new(HashMap::new()))
}

pub fn register_x_operator(name: &str, operator: Operator) -> Result<(), String> {
    if !name.starts_with("x.") {
        return Err("x operators must use 'x.' namespace".to_string());
    }
    let mut registry = x_operator_registry()
        .write()
        .map_err(|_| "x operator registry is poisoned".to_string())?;
    registry.insert(name.to_string(), operator);
    Ok(())
}

pub fn unregister_x_operator(name: &str) -> bool {
    if let Ok(mut registry) = x_operator_registry().write() {
        return registry.remove(name).is_some();
    }
    false
}

pub fn clear_x_operator_registry() {
    if let Ok(mut registry) = x_operator_registry().write() {
        registry.clear();
    }
}

fn get_runtime_from_state(state: &State) -> Result<WebRuntimeContext, EvalError> {
    let v = state
        .server
        .get(WEB_RUNTIME_KEY)
        .ok_or_else(|| EvalError::new("web runtime context missing in server state"))?
        .clone();
    serde_json::from_value(v)
        .map_err(|e| EvalError::new(format!("invalid web runtime context: {e}")))
}

fn set_runtime_on_state(state: &mut State, runtime: &WebRuntimeContext) -> Result<(), EvalError> {
    let v = serde_json::to_value(runtime)
        .map_err(|e| EvalError::new(format!("failed to encode web runtime context: {e}")))?;
    state.server.insert(WEB_RUNTIME_KEY.to_string(), v);
    Ok(())
}

fn read_optional_key(args: &[Value], ctx: &mut Context) -> Result<Option<String>, EvalError> {
    if args.is_empty() {
        return Ok(None);
    }
    let key = evaluate(&args[0], ctx)?;
    let key = key
        .as_str()
        .ok_or_else(|| EvalError::new("lookup key must be string"))?;
    Ok(Some(key.to_string()))
}

fn req_map_lookup(map: &HashMap<String, String>, key: Option<String>) -> Value {
    if let Some(key) = key {
        map.get(&key)
            .map(|s| Value::String(s.clone()))
            .unwrap_or(Value::Null)
    } else {
        serde_json::to_value(map).unwrap_or(Value::Null)
    }
}

fn op_req_method(args: &[Value], ctx: &mut Context) -> EvalResult {
    let _ = args;
    let runtime = get_runtime_from_state(ctx.state)?;
    Ok(Value::String(runtime.request.method))
}

fn op_req_path(args: &[Value], ctx: &mut Context) -> EvalResult {
    let _ = args;
    let runtime = get_runtime_from_state(ctx.state)?;
    Ok(Value::String(runtime.request.path))
}

fn op_req_body(args: &[Value], ctx: &mut Context) -> EvalResult {
    let _ = args;
    let runtime = get_runtime_from_state(ctx.state)?;
    Ok(runtime.request.body)
}

fn op_req_param(args: &[Value], ctx: &mut Context) -> EvalResult {
    let key = read_optional_key(args, ctx)?;
    let runtime = get_runtime_from_state(ctx.state)?;
    Ok(req_map_lookup(&runtime.request.params, key))
}

fn op_req_query(args: &[Value], ctx: &mut Context) -> EvalResult {
    let key = read_optional_key(args, ctx)?;
    let runtime = get_runtime_from_state(ctx.state)?;
    Ok(req_map_lookup(&runtime.request.query, key))
}

fn op_req_header(args: &[Value], ctx: &mut Context) -> EvalResult {
    let key = read_optional_key(args, ctx)?;
    let runtime = get_runtime_from_state(ctx.state)?;
    Ok(req_map_lookup(&runtime.request.headers, key))
}

fn op_req_cookie(args: &[Value], ctx: &mut Context) -> EvalResult {
    let key = read_optional_key(args, ctx)?;
    let runtime = get_runtime_from_state(ctx.state)?;
    Ok(req_map_lookup(&runtime.request.cookies, key))
}

fn op_res_status(args: &[Value], ctx: &mut Context) -> EvalResult {
    if args.is_empty() {
        let runtime = get_runtime_from_state(ctx.state)?;
        return Ok(Value::Number(serde_json::Number::from(
            runtime.response.status,
        )));
    }

    let status = evaluate(&args[0], ctx)?;
    let status = status
        .as_u64()
        .ok_or_else(|| EvalError::new("res.status value must be integer"))? as u16;
    let mut runtime = get_runtime_from_state(ctx.state)?;
    runtime.response.status = status;
    set_runtime_on_state(ctx.state, &runtime)?;
    Ok(Value::Number(serde_json::Number::from(status)))
}

fn op_res_header(args: &[Value], ctx: &mut Context) -> EvalResult {
    if args.is_empty() {
        let runtime = get_runtime_from_state(ctx.state)?;
        return Ok(serde_json::to_value(&runtime.response.headers).unwrap_or(Value::Null));
    }

    let key = evaluate(&args[0], ctx)?;
    let key = key
        .as_str()
        .ok_or_else(|| EvalError::new("res.header key must be string"))?
        .to_ascii_lowercase();

    if args.len() == 1 {
        let runtime = get_runtime_from_state(ctx.state)?;
        return Ok(runtime
            .response
            .headers
            .get(&key)
            .map(|v| Value::String(v.clone()))
            .unwrap_or(Value::Null));
    }

    let value = evaluate(&args[1], ctx)?;
    let value = match value {
        Value::String(s) => s,
        _ => value.to_string(),
    };
    let mut runtime = get_runtime_from_state(ctx.state)?;
    runtime.response.headers.insert(key, value.clone());
    set_runtime_on_state(ctx.state, &runtime)?;
    Ok(Value::String(value))
}

fn op_res_json(args: &[Value], ctx: &mut Context) -> EvalResult {
    if args.is_empty() {
        return Err(EvalError::new("res.json requires payload argument"));
    }
    let payload = evaluate(&args[0], ctx)?;
    let mut runtime = get_runtime_from_state(ctx.state)?;
    runtime.response.body = payload.clone();
    runtime
        .response
        .headers
        .insert("content-type".to_string(), "application/json".to_string());
    set_runtime_on_state(ctx.state, &runtime)?;
    Ok(payload)
}

fn op_res_text(args: &[Value], ctx: &mut Context) -> EvalResult {
    if args.is_empty() {
        return Err(EvalError::new("res.text requires text argument"));
    }
    let text = evaluate(&args[0], ctx)?;
    let text = match text {
        Value::String(s) => s,
        _ => text.to_string(),
    };
    let mut runtime = get_runtime_from_state(ctx.state)?;
    runtime.response.body = Value::String(text.clone());
    runtime.response.headers.insert(
        "content-type".to_string(),
        "text/plain; charset=utf-8".to_string(),
    );
    set_runtime_on_state(ctx.state, &runtime)?;
    Ok(Value::String(text))
}

fn op_res_redirect(args: &[Value], ctx: &mut Context) -> EvalResult {
    if args.is_empty() {
        return Err(EvalError::new("res.redirect requires target url argument"));
    }
    let to = evaluate(&args[0], ctx)?;
    let to = to
        .as_str()
        .ok_or_else(|| EvalError::new("res.redirect target must be string"))?
        .to_string();
    let mut runtime = get_runtime_from_state(ctx.state)?;
    runtime.response.redirect = Some(to.clone());
    if runtime.response.status == 200 {
        runtime.response.status = 302;
    }
    set_runtime_on_state(ctx.state, &runtime)?;
    Ok(Value::String(to))
}

fn op_cookie_set(args: &[Value], ctx: &mut Context) -> EvalResult {
    if args.len() < 2 {
        return Err(EvalError::new(
            "cookie.set requires cookie name and value arguments",
        ));
    }
    let name = evaluate(&args[0], ctx)?;
    let name = name
        .as_str()
        .ok_or_else(|| EvalError::new("cookie.set name must be string"))?
        .to_string();
    let value = evaluate(&args[1], ctx)?;
    let value = match value {
        Value::String(s) => s,
        _ => value.to_string(),
    };
    let options = if args.len() > 2 {
        evaluate(&args[2], ctx)?
            .as_object()
            .cloned()
            .unwrap_or_default()
    } else {
        Map::new()
    };
    let mut runtime = get_runtime_from_state(ctx.state)?;
    runtime.response.cookies.push(WebCookieMutation {
        action: "set".to_string(),
        name: name.clone(),
        value: value.clone(),
        options,
    });
    set_runtime_on_state(ctx.state, &runtime)?;
    Ok(json!({ "name": name, "value": value }))
}

fn op_cookie_clear(args: &[Value], ctx: &mut Context) -> EvalResult {
    if args.is_empty() {
        return Err(EvalError::new("cookie.clear requires cookie name argument"));
    }
    let name = evaluate(&args[0], ctx)?;
    let name = name
        .as_str()
        .ok_or_else(|| EvalError::new("cookie.clear name must be string"))?
        .to_string();
    let options = if args.len() > 1 {
        evaluate(&args[1], ctx)?
            .as_object()
            .cloned()
            .unwrap_or_default()
    } else {
        Map::new()
    };
    let mut runtime = get_runtime_from_state(ctx.state)?;
    runtime.response.cookies.push(WebCookieMutation {
        action: "clear".to_string(),
        name: name.clone(),
        value: String::new(),
        options,
    });
    set_runtime_on_state(ctx.state, &runtime)?;
    Ok(json!({ "name": name, "cleared": true }))
}

fn wildcard_match(pattern: &str, value: &str) -> bool {
    if pattern == "*" {
        return true;
    }
    if !pattern.contains('*') {
        return pattern == value;
    }
    let mut pos = 0usize;
    let mut first = true;
    for chunk in pattern.split('*') {
        if chunk.is_empty() {
            continue;
        }
        if first && !pattern.starts_with('*') {
            if !value[pos..].starts_with(chunk) {
                return false;
            }
            pos += chunk.len();
            first = false;
            continue;
        }
        if let Some(found) = value[pos..].find(chunk) {
            pos += found + chunk.len();
        } else {
            return false;
        }
        first = false;
    }
    if !pattern.ends_with('*') {
        if let Some(last_chunk) = pattern.rsplit('*').next() {
            return value.ends_with(last_chunk);
        }
    }
    true
}

fn validate_http_policy(policy: &WebHttpPolicy, method: &str, url: &str) -> Result<(), EvalError> {
    let method_ok = policy
        .allow_methods
        .iter()
        .any(|allowed| allowed.eq_ignore_ascii_case(method));
    if !method_ok {
        return Err(EvalError::new(format!(
            "http method '{}' is not allowed by policy",
            method
        )));
    }
    if policy.allow_patterns.is_empty() {
        return Err(EvalError::new(
            "http allow_patterns is empty; request denied",
        ));
    }
    let allowed = policy
        .allow_patterns
        .iter()
        .any(|pattern| wildcard_match(pattern, url));
    if !allowed {
        return Err(EvalError::new(format!(
            "url '{}' is not allowed by policy",
            url
        )));
    }
    Ok(())
}

fn parse_request_map(v: Value) -> Result<(String, String, Value, Map<String, Value>), EvalError> {
    let obj = v
        .as_object()
        .ok_or_else(|| EvalError::new("http.request object form requires map argument"))?;
    let method = obj
        .get("method")
        .and_then(|v| v.as_str())
        .unwrap_or("GET")
        .to_ascii_uppercase();
    let url = obj
        .get("url")
        .and_then(|v| v.as_str())
        .ok_or_else(|| EvalError::new("http.request requires 'url' field"))?
        .to_string();
    let body = obj.get("body").cloned().unwrap_or(Value::Null);
    let headers = obj
        .get("headers")
        .and_then(|v| v.as_object())
        .cloned()
        .unwrap_or_default();
    Ok((method, url, body, headers))
}

fn perform_http_request(
    method: &str,
    url: &str,
    body: Value,
    headers: Map<String, Value>,
    ctx: &mut Context,
) -> EvalResult {
    let runtime = get_runtime_from_state(ctx.state)?;
    validate_http_policy(&runtime.http_policy, method, url)?;
    let mock_key = format!("{} {}", method.to_ascii_uppercase(), url);
    if let Some(mock) = runtime.http_mocks.get(&mock_key) {
        return Ok(mock.clone());
    }

    let timeout = Duration::from_millis(runtime.http_policy.timeout_ms.max(1));
    let agent = ureq::AgentBuilder::new()
        .timeout_connect(timeout)
        .timeout_read(timeout)
        .timeout_write(timeout)
        .build();

    let mut request = agent.request(method, url);
    let has_content_type = headers
        .keys()
        .any(|k| k.eq_ignore_ascii_case("content-type"));
    for (key, value) in &headers {
        if let Some(s) = header_value_to_string(value) {
            request = request.set(key, &s);
        }
    }
    if !has_content_type && !body.is_null() && method != "GET" && method != "DELETE" {
        request = request.set("content-type", "application/json");
    }

    let response = if body.is_null() || method == "GET" || method == "DELETE" {
        request.call()
    } else {
        request.send_string(&body.to_string())
    };

    match response {
        Ok(resp) => Ok(response_to_json(true, resp)),
        Err(ureq::Error::Status(_code, resp)) => Ok(response_to_json(false, resp)),
        Err(ureq::Error::Transport(err)) => Err(EvalError::new(format!(
            "http transport error for {} {}: {}",
            method, url, err
        ))),
    }
}

fn header_value_to_string(value: &Value) -> Option<String> {
    match value {
        Value::String(s) => Some(s.clone()),
        Value::Number(n) => Some(n.to_string()),
        Value::Bool(b) => Some(b.to_string()),
        _ => None,
    }
}

fn response_to_json(ok_hint: bool, resp: ureq::Response) -> Value {
    let status = resp.status();
    let mut headers = HashMap::<String, String>::new();
    for name in resp.headers_names() {
        if let Some(value) = resp.header(&name) {
            headers.insert(name.to_ascii_lowercase(), value.to_string());
        }
    }
    let body_str = resp.into_string().unwrap_or_default();
    let body_value = if body_str.trim().is_empty() {
        Value::Null
    } else {
        serde_json::from_str::<Value>(&body_str).unwrap_or(Value::String(body_str))
    };

    json!({
        "ok": ok_hint && (200..300).contains(&status),
        "status": status,
        "headers": headers,
        "body": body_value
    })
}

fn op_http_request(args: &[Value], ctx: &mut Context) -> EvalResult {
    if args.is_empty() {
        return Err(EvalError::new("http.request requires arguments"));
    }
    if args.len() == 1 {
        let req_map = evaluate(&args[0], ctx)?;
        let (method, url, body, headers) = parse_request_map(req_map)?;
        return perform_http_request(&method, &url, body, headers, ctx);
    }
    let method = evaluate(&args[0], ctx)?;
    let method = method
        .as_str()
        .ok_or_else(|| EvalError::new("http.request method must be string"))?
        .to_ascii_uppercase();
    let url = evaluate(&args[1], ctx)?;
    let url = url
        .as_str()
        .ok_or_else(|| EvalError::new("http.request url must be string"))?
        .to_string();
    let body = if args.len() > 2 {
        evaluate(&args[2], ctx)?
    } else {
        Value::Null
    };
    let headers = if args.len() > 3 {
        evaluate(&args[3], ctx)?
            .as_object()
            .cloned()
            .unwrap_or_default()
    } else {
        Map::new()
    };
    perform_http_request(&method, &url, body, headers, ctx)
}

fn op_http_get(args: &[Value], ctx: &mut Context) -> EvalResult {
    if args.is_empty() {
        return Err(EvalError::new("http.get requires url argument"));
    }
    let url = evaluate(&args[0], ctx)?;
    let url = url
        .as_str()
        .ok_or_else(|| EvalError::new("http.get url must be string"))?
        .to_string();
    perform_http_request("GET", &url, Value::Null, Map::new(), ctx)
}

fn op_http_post(args: &[Value], ctx: &mut Context) -> EvalResult {
    if args.is_empty() {
        return Err(EvalError::new("http.post requires url argument"));
    }
    let url = evaluate(&args[0], ctx)?;
    let url = url
        .as_str()
        .ok_or_else(|| EvalError::new("http.post url must be string"))?
        .to_string();
    let body = if args.len() > 1 {
        evaluate(&args[1], ctx)?
    } else {
        Value::Null
    };
    let headers = if args.len() > 2 {
        evaluate(&args[2], ctx)?
            .as_object()
            .cloned()
            .unwrap_or_default()
    } else {
        Map::new()
    };
    perform_http_request("POST", &url, body, headers, ctx)
}

fn op_http_put(args: &[Value], ctx: &mut Context) -> EvalResult {
    if args.is_empty() {
        return Err(EvalError::new("http.put requires url argument"));
    }
    let url = evaluate(&args[0], ctx)?;
    let url = url
        .as_str()
        .ok_or_else(|| EvalError::new("http.put url must be string"))?
        .to_string();
    let body = if args.len() > 1 {
        evaluate(&args[1], ctx)?
    } else {
        Value::Null
    };
    let headers = if args.len() > 2 {
        evaluate(&args[2], ctx)?
            .as_object()
            .cloned()
            .unwrap_or_default()
    } else {
        Map::new()
    };
    perform_http_request("PUT", &url, body, headers, ctx)
}

fn op_http_delete(args: &[Value], ctx: &mut Context) -> EvalResult {
    if args.is_empty() {
        return Err(EvalError::new("http.delete requires url argument"));
    }
    let url = evaluate(&args[0], ctx)?;
    let url = url
        .as_str()
        .ok_or_else(|| EvalError::new("http.delete url must be string"))?
        .to_string();
    perform_http_request("DELETE", &url, Value::Null, Map::new(), ctx)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn web_runtime_cookie_and_response() {
        let expr = json!([
            "do",
            ["res.status", 201],
            ["res.header", "x-test", "1"],
            ["cookie.set", "sid", "abc", {"httpOnly": true}],
            ["res.json", {"ok": true}]
        ]);
        let state = State::new();
        let request = WebRequestContext {
            method: "POST".to_string(),
            path: "/login".to_string(),
            body: json!({"u":"a"}),
            user: "default".to_string(),
            group: "default".to_string(),
            params: HashMap::new(),
            query: HashMap::new(),
            headers: HashMap::new(),
            cookies: HashMap::new(),
        };
        let runtime = runtime_from_request(request);
        let out = evaluate_web_logic(&expr, state, runtime).unwrap();
        assert_eq!(out.runtime.response.status, 201);
        assert_eq!(
            out.runtime.response.headers.get("content-type"),
            Some(&"application/json".to_string())
        );
        assert_eq!(out.runtime.response.cookies.len(), 1);
    }

    #[test]
    fn web_runtime_http_mock() {
        let expr = json!(["http.get", "https://api.example.com/a"]);
        let state = State::new();
        let request = WebRequestContext::default();
        let mut runtime = runtime_from_request(request);
        runtime.http_policy.allow_patterns = vec!["https://api.example.com/*".to_string()];
        runtime.http_mocks.insert(
            "GET https://api.example.com/a".to_string(),
            json!({"ok": true}),
        );
        let out = evaluate_web_logic(&expr, state, runtime).unwrap();
        assert_eq!(out.value, json!({"ok": true}));
    }

    #[test]
    fn web_runtime_http_real_transport() {
        let expr = json!(["http.get", "http://127.0.0.1:1/ping"]);
        let state = State::new();
        let request = WebRequestContext::default();
        let mut runtime = runtime_from_request(request);
        runtime.http_policy.allow_patterns = vec!["http://127.0.0.1:*".to_string()];
        let err =
            evaluate_web_logic(&expr, state, runtime).expect_err("http.get should hit transport");
        assert!(err.message.contains("http transport error"));
    }

    fn op_x_echo(args: &[Value], _ctx: &mut Context) -> EvalResult {
        if args.is_empty() {
            return Ok(Value::Null);
        }
        Ok(args[0].clone())
    }

    #[test]
    fn web_runtime_x_registry_registers_operator() {
        clear_x_operator_registry();
        register_x_operator("x.echo", op_x_echo as Operator).expect("register x operator");

        let expr = json!(["x.echo", {"ok": true}]);
        let state = State::new();
        let runtime = runtime_from_request(WebRequestContext::default());
        let out = evaluate_web_logic(&expr, state, runtime).expect("x operator should execute");
        assert_eq!(out.value, json!({"ok": true}));

        assert!(unregister_x_operator("x.echo"));
        clear_x_operator_registry();
    }
}
