use josie_web::{CompileInput, JosieWebEngine, RenderInput, RuntimeMode};
use serde_json::{Map, Value};
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

fn main() -> Result<(), String> {
    let mut engine = JosieWebEngine::new();
    let mut components = HashMap::new();
    components.insert(
        "card".to_string(),
        "<article class=\"card\"><h2>{{title}}</h2><j-component name=\"badge\" /></article>"
            .to_string(),
    );
    components.insert(
        "badge".to_string(),
        "<span class=\"badge\">SUBCOMPONENT_OK</span>".to_string(),
    );

    let template = r#"<!doctype html>
<html>
<body>
  <main>
    <j-component name="card" />
  </main>
  <script type="application/josie+json">
  {
    "kind": "program",
    "version": "0.1.0",
    "state": {},
    "actions": {},
    "resources": [],
    "steps": []
  }
  </script>
</body>
</html>"#;
    let runtime_mode = match std::env::var("JOSIE_WEB_RUNTIME_MODE")
        .unwrap_or_else(|_| "compiled".to_string())
        .as_str()
    {
        "interpreted" => RuntimeMode::Interpreted,
        _ => RuntimeMode::Compiled,
    };
    let compiled = engine.compile(CompileInput {
        template: template.to_string(),
        components,
        processors: vec![],
        allow_list: vec![],
        load_scripts: vec![],
        runtime_mode: Some(runtime_mode),
        cache: None,
        cache_policy: None,
    })?;

    let mut vars = Map::<String, Value>::new();
    vars.insert("title".to_string(), Value::String("EXPORT_OK".to_string()));
    let rendered = engine.render(RenderInput {
        compiled: compiled.clone(),
        vars,
    })?;

    let out_dir = std::env::var("JOSIE_WEB_EXPORT_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("/tmp/josie-web-export-smoke"));
    fs::create_dir_all(&out_dir)
        .map_err(|e| format!("failed to create export dir {}: {e}", out_dir.display()))?;

    engine.export_compiled(&compiled, &out_dir)?;
    fs::write(out_dir.join("rendered.preview.html"), rendered.html)
        .map_err(|e| format!("failed to write rendered.preview.html: {e}"))?;

    println!("{}", out_dir.display());
    Ok(())
}
