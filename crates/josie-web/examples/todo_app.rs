use josie_web::{CompileInput, JosieWebEngine, RenderInput, RuntimeMode};
use serde_json::Map;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

fn main() -> Result<(), String> {
    let mut engine = JosieWebEngine::new();
    let template = include_str!("program.josieml");
    let runtime_mode = match std::env::var("JOSIE_WEB_RUNTIME_MODE")
        .unwrap_or_else(|_| "compiled".to_string())
        .as_str()
    {
        "interpreted" => RuntimeMode::Interpreted,
        _ => RuntimeMode::Compiled,
    };
    let compiled = engine.compile(CompileInput {
        template: template.to_owned(),
        components: HashMap::new(),
        processors: vec!["tailwind".to_string(), "minify".to_string()],
        allow_list: vec![],
        load_scripts: vec![],
        runtime_mode: Some(runtime_mode),
        cache: None,
        cache_policy: None,
    })?;

    let rendered = engine.render(RenderInput {
        compiled: compiled.clone(),
        vars: Map::new(),
    })?;

    let out_dir = std::env::var("JOSIE_WEB_EXPORT_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("/tmp/josie-web-todo"));
    fs::create_dir_all(&out_dir)
        .map_err(|e| format!("failed to create export dir {}: {e}", out_dir.display()))?;

    engine.export_compiled(&compiled, &out_dir)?;
    fs::write(out_dir.join("rendered.preview.html"), rendered.html)
        .map_err(|e| format!("failed to write rendered.preview.html: {e}"))?;

    println!("{}", out_dir.display());
    Ok(())
}
