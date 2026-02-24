use josie_web::{CachePolicy, CacheRef, CompileInput, JosieWebEngine, RenderInput, RuntimeMode};
use serde_json::{Map, Value, json};
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

fn main() -> Result<(), String> {
    let mut engine = JosieWebEngine::new();

    // Backend compiles template once (startup/publish time).
    let compiled = engine.compile(CompileInput {
        template: r#"<!doctype html>
<html>
<body class="bg-zinc-950 text-zinc-100">
  <main class="p-6">
    <h1 j-text="client.title"></h1>
    <p j-text="client.count"></p>
    <button @click="counter.inc" class="rounded bg-cyan-500 px-3 py-1 text-black">+</button>
  </main>
  <script type="application/josie+json">
  {
    "kind": "program",
    "version": "0.1.0",
    "state": {
      "client": { "title": "Counter", "count": 0 }
    },
    "actions": {
      "counter.inc": { "runStep": "counter.inc.step" }
    },
    "resources": [],
    "steps": [
      {
        "id": "counter.inc.step",
        "op": "do",
        "args": [
          ["set", "client.count", ["+", ["var", "client.count"], 1]]
        ]
      }
    ]
  }
  </script>
</body>
</html>"#
            .to_string(),
        components: HashMap::new(),
        processors: vec![
            "tailwind".to_string(),
            "security".to_string(),
            "minify".to_string(),
        ],
        allow_list: vec![],
        load_scripts: vec![],
        runtime_mode: Some(RuntimeMode::Compiled),
        cache: Some(CacheRef {
            namespace: "example".to_string(),
            key: "counter-page".to_string(),
            version: Some("v1".to_string()),
        }),
        cache_policy: Some(CachePolicy::Auto),
    })?;

    // Backend renders many requests quickly from compiled artifact.
    let render_a = engine.render(RenderInput {
        compiled: compiled.clone(),
        vars: Map::new(),
    })?;
    let mut vars = Map::<String, Value>::new();
    vars.insert(
        "server".to_string(),
        json!({
            "requestId": "req-123"
        }),
    );
    let render_b = engine.render(RenderInput {
        compiled: compiled.clone(),
        vars,
    })?;

    // Export static output to inspect runtime payload.
    let out_dir = std::env::var("JOSIE_WEB_EXPORT_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("/tmp/josie-web-backend-subscribe-notify"));
    fs::create_dir_all(&out_dir)
        .map_err(|e| format!("failed to create export dir {}: {e}", out_dir.display()))?;
    engine.export_compiled(&compiled, &out_dir)?;

    // The compiled runtime now receives __JOSIE_BINDINGS__ (compile-time extracted bindings).
    let compiled_js = fs::read_to_string(out_dir.join("program.compiled.js"))
        .map_err(|e| format!("failed to read program.compiled.js: {e}"))?;
    let has_bindings = compiled_js.contains("__JOSIE_BINDINGS__");

    fs::write(out_dir.join("rendered.a.html"), render_a.html)
        .map_err(|e| format!("failed to write rendered.a.html: {e}"))?;
    fs::write(out_dir.join("rendered.b.html"), render_b.html)
        .map_err(|e| format!("failed to write rendered.b.html: {e}"))?;

    println!("export_dir={}", out_dir.display());
    println!("compiled_has_bindings={has_bindings}");
    println!("note=subscribe/notify is runtime-internal; backend API is still compile()+render()");
    Ok(())
}
