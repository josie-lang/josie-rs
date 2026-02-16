use axum::Router;
use axum::extract::Path as AxumPath;
use axum::extract::State as AxumState;
use axum::extract::ws::{Message, WebSocket, WebSocketUpgrade};
use axum::http::{HeaderMap, HeaderValue, StatusCode, header};
use axum::response::{Html, IntoResponse, Response};
use axum::routing::get;
use josie_core::{Context, Operators, State, evaluate};
use josie_web::{JosieWebCore, JosieWebEngine, JosieWebServer, RouteJob, SystemConfig};
use notify::{Event, RecommendedWatcher, RecursiveMode, Watcher};
use std::collections::hash_map::DefaultHasher;
use std::env;
use std::fs;
use std::hash::{Hash, Hasher};
use std::path::{Component, Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use tokio::sync::broadcast;
use tokio::sync::mpsc;
use tokio::time::{Duration, Instant};

#[derive(Debug, Clone)]
enum CliCommand {
    Dev {
        path: PathBuf,
        port: u16,
        watch: bool,
    },
    Serve {
        dir: PathBuf,
        port: u16,
        watch: bool,
    },
    Run {
        file: PathBuf,
        data: Option<String>,
        entry: Option<String>,
    },
    InitClient {
        out: PathBuf,
    },
}

#[derive(Clone)]
struct AppState {
    root: PathBuf,
    watch: bool,
    version: Arc<AtomicU64>,
    reload_tx: broadcast::Sender<u64>,
    server: Arc<Mutex<CliJosieWebServer>>,
}

#[derive(Debug)]
struct CliJosieWebServer {
    core: JosieWebEngine,
    is_running: bool,
}

impl CliJosieWebServer {
    fn new(config: SystemConfig) -> Result<Self, String> {
        let mut core = JosieWebEngine::new();
        core.init(config)?;
        Ok(Self {
            core,
            is_running: false,
        })
    }
}

impl JosieWebCore for CliJosieWebServer {
    fn init(&mut self, config: SystemConfig) -> Result<(), String> {
        self.core.init(config)
    }

    fn gc(&mut self) {
        self.core.gc();
    }

    fn match_route(&self, user: &str, group: &str, url: &str) -> Result<RouteJob, String> {
        self.core.match_route(user, group, url)
    }

    fn compile_view(&mut self, job: &RouteJob) -> Result<josie_web::ViewTemplate, String> {
        self.core.compile_view(job)
    }

    fn render_view(
        &self,
        view: &josie_web::ViewTemplate,
        data: &serde_json::Value,
    ) -> Result<josie_web::RenderOutput, String> {
        self.core.render_view(view, data)
    }

    fn invalidate(&mut self, keys: &[String]) {
        self.core.invalidate(keys);
    }

    fn prewarm(&mut self, route_keys: &[String]) -> Result<(), String> {
        self.core.prewarm(route_keys)
    }
}

impl JosieWebServer for CliJosieWebServer {
    fn serve(&mut self, _bind: &str, _port: u16) -> Result<(), String> {
        self.is_running = true;
        Ok(())
    }

    fn dev_watch(&mut self, _paths: &[PathBuf]) -> Result<(), String> {
        Ok(())
    }

    fn shutdown(&mut self) -> Result<(), String> {
        self.is_running = false;
        Ok(())
    }
}

pub async fn run_from_env() -> Result<(), String> {
    run_from_args(env::args().skip(1).collect()).await
}

pub async fn run_from_args(args: Vec<String>) -> Result<(), String> {
    let command = parse_command(args)?;

    match command {
        CliCommand::Dev { path, port, watch } => run_server(path, port, watch, true).await,
        CliCommand::Serve { dir, port, watch } => run_server(dir, port, watch, false).await,
        CliCommand::Run { file, data, entry } => run_headless(file, data, entry),
        CliCommand::InitClient { out } => {
            fs::write(&out, josie_web::JOSIE_RUNTIME_JS)
                .map_err(|e| format!("failed to write {}: {e}", out.display()))?;
            println!("wrote {}", out.display());
            Ok(())
        }
    }
}

fn parse_command(args: Vec<String>) -> Result<CliCommand, String> {
    if args.is_empty() {
        return Err(help_text());
    }

    let cmd = args[0].as_str();
    match cmd {
        "dev" => parse_dev_or_serve(args, true),
        "serve" => parse_dev_or_serve(args, false),
        "run" => parse_run(args),
        "init-client" => parse_init_client(args),
        "help" | "--help" | "-h" => Err(help_text()),
        _ => Err(format!("unknown command: {cmd}\n\n{}", help_text())),
    }
}

fn parse_dev_or_serve(args: Vec<String>, is_dev: bool) -> Result<CliCommand, String> {
    let mut path: Option<PathBuf> = None;
    let mut port: u16 = if is_dev { 3000 } else { 8080 };
    let mut watch = true;

    let mut i = 1usize;
    while i < args.len() {
        let token = &args[i];
        match token.as_str() {
            "--port" => {
                i += 1;
                let value = args
                    .get(i)
                    .ok_or_else(|| "--port requires a value".to_string())?;
                port = value
                    .parse::<u16>()
                    .map_err(|_| format!("invalid port: {value}"))?;
            }
            "--watch" => watch = true,
            "--no-watch" => watch = false,
            x if x.starts_with("--") => return Err(format!("unknown flag: {x}")),
            _ => {
                if path.is_some() {
                    return Err("only one PATH/DIR positional argument is allowed".to_string());
                }
                path = Some(PathBuf::from(token));
            }
        }
        i += 1;
    }

    if is_dev {
        Ok(CliCommand::Dev {
            path: path.unwrap_or_else(|| PathBuf::from(".")),
            port,
            watch,
        })
    } else {
        Ok(CliCommand::Serve {
            dir: path.unwrap_or_else(|| PathBuf::from(".")),
            port,
            watch,
        })
    }
}

fn parse_run(args: Vec<String>) -> Result<CliCommand, String> {
    let mut file: Option<PathBuf> = None;
    let mut data: Option<String> = None;
    let mut entry: Option<String> = None;

    let mut i = 1usize;
    while i < args.len() {
        let token = &args[i];
        match token.as_str() {
            "--data" => {
                i += 1;
                data = Some(
                    args.get(i)
                        .ok_or_else(|| "--data requires a value".to_string())?
                        .to_string(),
                );
            }
            "--entry" => {
                i += 1;
                entry = Some(
                    args.get(i)
                        .ok_or_else(|| "--entry requires a value".to_string())?
                        .to_string(),
                );
            }
            x if x.starts_with("--") => return Err(format!("unknown flag: {x}")),
            _ => {
                if file.is_some() {
                    return Err("only one FILE positional argument is allowed".to_string());
                }
                file = Some(PathBuf::from(token));
            }
        }
        i += 1;
    }

    let file = file.ok_or_else(|| "run requires FILE".to_string())?;
    Ok(CliCommand::Run { file, data, entry })
}

fn parse_init_client(args: Vec<String>) -> Result<CliCommand, String> {
    let mut out = PathBuf::from("josie-vm.js");
    let mut i = 1usize;

    while i < args.len() {
        let token = &args[i];
        match token.as_str() {
            "--out" => {
                i += 1;
                out = PathBuf::from(
                    args.get(i)
                        .ok_or_else(|| "--out requires a value".to_string())?
                        .to_string(),
                );
            }
            x if x.starts_with("--") => return Err(format!("unknown flag: {x}")),
            _ => return Err("init-client does not accept positional args".to_string()),
        }
        i += 1;
    }

    Ok(CliCommand::InitClient { out })
}

fn help_text() -> String {
    [
        "josie CLI",
        "",
        "Commands:",
        "  josie dev [PATH] [--port 3000] [--watch|--no-watch]",
        "  josie serve [DIR] [--port 8080] [--watch|--no-watch]",
        "  josie run FILE [--data JSON_OR_PATH] [--entry NAME]",
        "  josie init-client [--out josie-vm.js]",
    ]
    .join("\n")
}

fn run_headless(file: PathBuf, data: Option<String>, entry: Option<String>) -> Result<(), String> {
    let source =
        fs::read_to_string(&file).map_err(|e| format!("failed to read {}: {e}", file.display()))?;
    let root: serde_json::Value = serde_json::from_str(&source)
        .map_err(|e| format!("failed to parse {}: {e}", file.display()))?;

    let expression = resolve_run_expression(&root, entry.as_deref())?;
    let data_value = load_data_payload(data)?;

    let mut state = state_from_data(data_value);
    let operators = Operators::new();
    let mut ctx = Context {
        state: &mut state,
        operators: &operators,
        event: None,
    };

    let output = evaluate(&expression, &mut ctx).map_err(|e| e.message)?;
    let pretty = serde_json::to_string_pretty(&output).map_err(|e| e.to_string())?;
    println!("{pretty}");
    Ok(())
}

fn resolve_run_expression(
    root: &serde_json::Value,
    entry: Option<&str>,
) -> Result<serde_json::Value, String> {
    if root.is_array()
        || root.is_string()
        || root.is_number()
        || root.is_boolean()
        || root.is_null()
    {
        return Ok(root.clone());
    }

    let obj = root
        .as_object()
        .ok_or_else(|| "run input must be JSON expression or object".to_string())?;

    if let Some(program) = obj.get("program") {
        return Ok(program.clone());
    }

    if let Some(expr) = obj.get("expr") {
        return Ok(expr.clone());
    }

    if let Some(logic) = obj.get("logic").and_then(|v| v.as_object()) {
        let wanted = entry.unwrap_or("main");
        if let Some(found) = logic.get(wanted) {
            return Ok(found.clone());
        }
        let known = logic.keys().cloned().collect::<Vec<_>>().join(", ");
        return Err(format!(
            "logic entry '{wanted}' not found. available: [{}]",
            known
        ));
    }

    Err("no runnable expression found (expected top-level expression, program, expr, or logic entry)"
        .to_string())
}

fn load_data_payload(data: Option<String>) -> Result<Option<serde_json::Value>, String> {
    let Some(input) = data else {
        return Ok(None);
    };

    let as_path = PathBuf::from(&input);
    if as_path.exists() {
        let content = fs::read_to_string(&as_path)
            .map_err(|e| format!("failed to read {}: {e}", as_path.display()))?;
        let parsed = serde_json::from_str::<serde_json::Value>(&content)
            .map_err(|e| format!("invalid JSON in {}: {e}", as_path.display()))?;
        return Ok(Some(parsed));
    }

    let parsed = serde_json::from_str::<serde_json::Value>(&input)
        .map_err(|e| format!("invalid --data JSON: {e}"))?;
    Ok(Some(parsed))
}

fn state_from_data(data: Option<serde_json::Value>) -> State {
    let mut state = State::new();
    let Some(value) = data else {
        return state;
    };

    if let Some(obj) = value.as_object() {
        if let Some(server) = obj.get("server").and_then(|v| v.as_object()) {
            state.server = server.clone();
        }
        if let Some(client) = obj.get("client").and_then(|v| v.as_object()) {
            state.client = client.clone();
        } else {
            state.client = obj.clone();
        }
    }

    state
}

fn load_system_config(root: &Path) -> Result<SystemConfig, String> {
    let path = root.join("system.json");
    if !path.is_file() {
        return Ok(SystemConfig::minimal(root.to_path_buf()));
    }

    let content =
        fs::read_to_string(&path).map_err(|e| format!("failed to read {}: {e}", path.display()))?;
    let mut cfg = serde_json::from_str::<SystemConfig>(&content)
        .map_err(|e| format!("failed to parse {} as SystemConfig: {e}", path.display()))?;

    if cfg.root_path.as_os_str().is_empty() {
        cfg.root_path = root.to_path_buf();
    }
    if cfg.system_path.as_os_str().is_empty() {
        cfg.system_path = root.to_path_buf();
    }
    if !cfg.root_path.is_absolute() {
        cfg.root_path = root.join(&cfg.root_path);
    }
    if !cfg.system_path.is_absolute() {
        cfg.system_path = root.join(&cfg.system_path);
    }
    if cfg.max_memory_mb == 0 {
        cfg.max_memory_mb = 128;
    }

    Ok(cfg)
}

async fn run_server(root: PathBuf, port: u16, watch: bool, is_dev: bool) -> Result<(), String> {
    let root = root
        .canonicalize()
        .map_err(|e| format!("failed to resolve {}: {e}", root.display()))?;

    let config = load_system_config(&root)?;
    let mut server = CliJosieWebServer::new(config)?;
    server.serve("0.0.0.0", port)?;
    if watch {
        server.dev_watch(std::slice::from_ref(&root))?;
    }

    let state = Arc::new(AppState {
        root: root.clone(),
        watch,
        version: Arc::new(AtomicU64::new(0)),
        reload_tx: broadcast::channel(256).0,
        server: Arc::new(Mutex::new(server)),
    });

    if watch {
        let watcher_state = Arc::clone(&state);
        tokio::spawn(async move {
            watch_loop(watcher_state).await;
        });
    }

    let state_for_app = Arc::clone(&state);
    let app = Router::new()
        .route("/__josie_ws", get(ws_reload))
        .route("/", get(route_index))
        .route("/{*path}", get(route_any))
        .with_state(state_for_app);

    let host = format!("0.0.0.0:{port}");
    println!("JOSIE {}", if is_dev { "dev" } else { "serve" });
    println!("Root: {}", root.display());
    println!("URL:  http://localhost:{port}");
    println!("Watch: {}", if watch { "on" } else { "off" });

    let listener = tokio::net::TcpListener::bind(&host)
        .await
        .map_err(|e| format!("failed to bind {host}: {e}"))?;
    axum::serve(listener, app)
        .await
        .map_err(|e| format!("server failed: {e}"))?;

    if let Ok(mut guard) = state.server.lock() {
        let _ = guard.shutdown();
    }

    Ok(())
}

async fn watch_loop(state: Arc<AppState>) {
    let mut last = compute_tree_fingerprint(&state.root);
    let mut fallback_interval = tokio::time::interval(Duration::from_millis(1500));
    let mut notify = start_fs_watcher(&state.root).ok();

    if notify.is_some() {
        println!("[watch] filesystem watcher active (event-driven + fallback sweep)");
    } else {
        println!("[watch] filesystem watcher unavailable; using aggressive polling fallback");
    }

    loop {
        if let Some((_, rx)) = notify.as_mut() {
            tokio::select! {
                _ = fallback_interval.tick() => {
                    check_and_broadcast_if_changed(&state, &mut last).await;
                }
                evt = rx.recv() => {
                    match evt {
                        Some(_) => {
                            debounce_fs_events(rx).await;
                            check_and_broadcast_if_changed(&state, &mut last).await;
                        }
                        None => {
                            println!("[watch] filesystem watcher channel closed; fallback polling only");
                            notify = None;
                        }
                    }
                }
            }
        } else {
            fallback_interval.tick().await;
            check_and_broadcast_if_changed(&state, &mut last).await;
        }
    }
}

async fn check_and_broadcast_if_changed(state: &Arc<AppState>, last: &mut u64) {
    let root = state.root.clone();
    let now = tokio::task::spawn_blocking(move || compute_tree_fingerprint(&root))
        .await
        .unwrap_or(*last);
    if now != *last {
        *last = now;
        if let Ok(mut server) = state.server.lock() {
            server.invalidate(&[]);
            server.gc();
        }
        let next = state.version.fetch_add(1, Ordering::SeqCst) + 1;
        let _ = state.reload_tx.send(next);
        println!("[watch] change detected, reload version {next}");
    }
}

async fn debounce_fs_events(rx: &mut mpsc::UnboundedReceiver<()>) {
    let debounce_window = Duration::from_millis(120);
    let mut deadline = Instant::now() + debounce_window;
    let sleep = tokio::time::sleep_until(deadline);
    tokio::pin!(sleep);

    loop {
        tokio::select! {
            _ = &mut sleep => break,
            maybe = rx.recv() => {
                if maybe.is_none() {
                    break;
                }
                deadline = Instant::now() + debounce_window;
                sleep.as_mut().reset(deadline);
            }
        }
    }
}

fn start_fs_watcher(
    root: &Path,
) -> Result<(RecommendedWatcher, mpsc::UnboundedReceiver<()>), String> {
    let (tx, rx) = mpsc::unbounded_channel::<()>();
    let tx_cb = tx.clone();
    let mut watcher = notify::recommended_watcher(move |res: notify::Result<Event>| match res {
        Ok(event) => {
            if event_is_relevant(&event) {
                let _ = tx_cb.send(());
            }
        }
        Err(_) => {
            let _ = tx_cb.send(());
        }
    })
    .map_err(|e| format!("failed to initialize filesystem watcher: {e}"))?;

    watcher
        .watch(root, RecursiveMode::Recursive)
        .map_err(|e| format!("failed to watch {}: {e}", root.display()))?;

    Ok((watcher, rx))
}

fn event_is_relevant(event: &Event) -> bool {
    for path in &event.paths {
        if path_has_ignored_segment(path) {
            continue;
        }
        if path.is_dir() {
            return true;
        }
        if should_hash_contents(path) {
            return true;
        }
    }
    false
}

fn path_has_ignored_segment(path: &Path) -> bool {
    path.components().any(|c| match c {
        Component::Normal(seg) => {
            let s = seg.to_string_lossy();
            matches!(s.as_ref(), ".git" | "target" | "node_modules")
        }
        _ => false,
    })
}

fn compute_tree_fingerprint(root: &Path) -> u64 {
    fn collect_files(dir: &Path, out: &mut Vec<PathBuf>) {
        let Ok(read_dir) = fs::read_dir(dir) else {
            return;
        };

        for entry in read_dir.flatten() {
            let path = entry.path();
            if path.is_dir() {
                let name = path
                    .file_name()
                    .and_then(|s| s.to_str())
                    .unwrap_or_default();
                if matches!(name, ".git" | "target" | "node_modules") {
                    continue;
                }
                collect_files(&path, out);
                continue;
            }

            if path.is_file() {
                out.push(path);
            }
        }
    }

    let mut files = Vec::new();
    collect_files(root, &mut files);
    files.sort();

    let mut hasher = DefaultHasher::new();
    for path in files {
        let rel = path.strip_prefix(root).unwrap_or(&path);
        rel.to_string_lossy().hash(&mut hasher);
        if let Ok(meta) = fs::metadata(&path) {
            meta.len().hash(&mut hasher);
            if let Ok(modified) = meta.modified() {
                if let Ok(duration) = modified.duration_since(std::time::UNIX_EPOCH) {
                    duration.as_secs().hash(&mut hasher);
                    duration.subsec_nanos().hash(&mut hasher);
                }
            }
        }

        // Metadata-only checks can miss rapid same-size edits on some filesystems.
        // For source-like files, hash file bytes so watcher reacts deterministically.
        if should_hash_contents(&path) {
            if let Ok(bytes) = fs::read(&path) {
                bytes.hash(&mut hasher);
            }
        }
    }

    hasher.finish()
}

fn should_hash_contents(path: &Path) -> bool {
    let Some(ext) = path.extension().and_then(|s| s.to_str()) else {
        return false;
    };

    matches!(
        ext,
        "josieml" | "josie" | "json" | "html" | "htm" | "css" | "js" | "svg" | "txt" | "md"
    )
}

async fn ws_reload(ws: WebSocketUpgrade, AxumState(state): AxumState<Arc<AppState>>) -> Response {
    ws.on_upgrade(move |socket| handle_reload_socket(socket, state))
}

async fn handle_reload_socket(mut socket: WebSocket, state: Arc<AppState>) {
    let mut rx = state.reload_tx.subscribe();
    let initial = state.version.load(Ordering::SeqCst);

    if socket
        .send(Message::Text(initial.to_string().into()))
        .await
        .is_err()
    {
        return;
    }

    loop {
        tokio::select! {
            incoming = socket.recv() => {
                match incoming {
                    Some(Ok(Message::Close(_))) | None | Some(Err(_)) => break,
                    _ => {}
                }
            }
            next = rx.recv() => {
                match next {
                    Ok(version) => {
                        if socket
                            .send(Message::Text(version.to_string().into()))
                            .await
                            .is_err()
                        {
                            break;
                        }
                    }
                    Err(broadcast::error::RecvError::Lagged(_)) => continue,
                    Err(broadcast::error::RecvError::Closed) => break,
                }
            }
        }
    }
}

async fn route_index(headers: HeaderMap, AxumState(state): AxumState<Arc<AppState>>) -> Response {
    serve_route(&state, "", &headers).await
}

async fn route_any(
    AxumPath(path): AxumPath<String>,
    headers: HeaderMap,
    AxumState(state): AxumState<Arc<AppState>>,
) -> Response {
    serve_route(&state, &path, &headers).await
}

async fn serve_route(state: &AppState, raw_path: &str, headers: &HeaderMap) -> Response {
    let rel = match sanitize_rel_path(raw_path) {
        Some(p) => p,
        None => return (StatusCode::BAD_REQUEST, "invalid path").into_response(),
    };

    let user = headers
        .get("x-josie-user")
        .and_then(|v| v.to_str().ok())
        .filter(|v| !v.is_empty())
        .unwrap_or("default");
    let group = headers
        .get("x-josie-group")
        .and_then(|v| v.to_str().ok())
        .filter(|v| !v.is_empty())
        .unwrap_or("default");

    let user_root = state.root.join(user);
    if let Some(file) =
        resolve_static_file(&user_root, &rel).or_else(|| resolve_static_file(&state.root, &rel))
    {
        return serve_static(&file).await;
    }

    if let Some(response) = render_route_response(state, user, group, raw_path) {
        return response;
    }

    (StatusCode::NOT_FOUND, "not found").into_response()
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

fn resolve_static_file(root: &Path, rel: &Path) -> Option<PathBuf> {
    if rel.as_os_str().is_empty() {
        return None;
    }

    let full = root.join(rel);
    if !full.is_file() {
        return None;
    }

    let ext = full
        .extension()
        .and_then(|s| s.to_str())
        .unwrap_or_default();
    if matches!(ext, "josie" | "josieml") {
        return None;
    }

    if ext == "json" {
        return None;
    }

    Some(full)
}

fn render_route_response(
    state: &AppState,
    user: &str,
    group: &str,
    raw_path: &str,
) -> Option<Response> {
    let mut guard = state.server.lock().ok()?;
    let job = match guard.match_route(user, group, raw_path) {
        Ok(v) => v,
        Err(_) => return None,
    };

    let view = match guard.compile_view(&job) {
        Ok(v) => v,
        Err(e) => {
            return Some(
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    format!("failed to compile {}: {e}", job.file_path.display()),
                )
                    .into_response(),
            );
        }
    };
    let output = match guard.render_view(&view, &serde_json::json!({})) {
        Ok(v) => v,
        Err(e) => {
            return Some(
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    format!("failed to render {}: {e}", job.file_path.display()),
                )
                    .into_response(),
            );
        }
    };

    let mut html = output.html;
    if state.watch {
        html = inject_reload_script(html);
    }
    let mut response = Html(html).into_response();
    if let Ok(v) = HeaderValue::from_str(&job.file_path.display().to_string()) {
        response.headers_mut().insert("x-josie-source", v);
    }
    Some(response)
}

fn inject_reload_script(mut html: String) -> String {
    if html.contains("/__josie_ws") {
        return html;
    }

    let script = r#"<script>
(function(){
  var current = null;
  var reconnectTimer = null;

  function connect(){
    var proto = location.protocol === 'https:' ? 'wss://' : 'ws://';
    var ws = new WebSocket(proto + location.host + '/__josie_ws');

    ws.onmessage = function(event){
      var next = Number(event && event.data || 0);
      if (!Number.isFinite(next)) return;
      if (current === null) {
        current = next;
        return;
      }
      if (next !== current) {
        location.reload();
      }
    };

    ws.onclose = function(){
      if (reconnectTimer) clearTimeout(reconnectTimer);
      reconnectTimer = setTimeout(connect, 600);
    };

    ws.onerror = function(){
      try { ws.close(); } catch (_) {}
    };
  }

  connect();
})();
</script>"#;

    if let Some(idx) = html.rfind("</body>") {
        let before = &html[..idx];
        let after = &html[idx..];
        html = format!("{}{}{}", before, script, after);
    } else {
        html.push_str(script);
    }

    html
}

async fn serve_static(path: &Path) -> Response {
    let bytes = match tokio::fs::read(path).await {
        Ok(v) => v,
        Err(e) => {
            return (
                StatusCode::INTERNAL_SERVER_ERROR,
                format!("failed to read {}: {e}", path.display()),
            )
                .into_response();
        }
    };

    let content_type = match path
        .extension()
        .and_then(|s| s.to_str())
        .unwrap_or_default()
    {
        "html" => "text/html; charset=utf-8",
        "css" => "text/css; charset=utf-8",
        "js" => "application/javascript; charset=utf-8",
        "json" => "application/json; charset=utf-8",
        "svg" => "image/svg+xml",
        "png" => "image/png",
        "jpg" | "jpeg" => "image/jpeg",
        "webp" => "image/webp",
        "gif" => "image/gif",
        "ico" => "image/x-icon",
        "woff" => "font/woff",
        "woff2" => "font/woff2",
        _ => "application/octet-stream",
    };

    let mut response = bytes.into_response();
    if let Ok(value) = HeaderValue::from_str(content_type) {
        response.headers_mut().insert(header::CONTENT_TYPE, value);
    }
    response
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn unique_temp_dir(prefix: &str) -> PathBuf {
        let ts = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock should be after unix epoch")
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("{}-{}-{}", prefix, std::process::id(), ts));
        std::fs::create_dir_all(&dir).expect("failed to create temp dir");
        dir
    }

    #[test]
    fn test_fingerprint_detects_josieml_content_change_same_size() {
        let root = unique_temp_dir("josie-watch");
        let file = root.join("login.josieml");

        // Keep byte size identical to catch metadata-only watcher blind spots.
        std::fs::write(&file, "<div>A</div>\n").expect("failed to write initial josieml");
        let before = compute_tree_fingerprint(&root);

        std::fs::write(&file, "<div>B</div>\n").expect("failed to write modified josieml");
        let after = compute_tree_fingerprint(&root);

        assert_ne!(
            before, after,
            "watch fingerprint should change for .josieml content edits"
        );
    }

    #[test]
    fn test_engine_match_route_prefers_josieml_for_extensionless_route() {
        let root = unique_temp_dir("josie-route");
        let user_root = root.join("default");
        std::fs::create_dir_all(&user_root).expect("failed to create user root");
        std::fs::write(user_root.join("login.josie"), "{}").expect("failed to write login.josie");
        std::fs::write(user_root.join("login.josieml"), "<div></div>")
            .expect("failed to write login.josieml");

        let server = CliJosieWebServer::new(SystemConfig::minimal(root.clone()))
            .expect("server must initialize");
        let resolved = server
            .match_route("default", "default", "/login")
            .expect("expected resolver to find login document");

        assert_eq!(
            resolved.file_path.extension().and_then(|s| s.to_str()),
            Some("josieml"),
            "extensionless route should prefer .josieml when both files exist"
        );
    }
}
