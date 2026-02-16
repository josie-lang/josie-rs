#[tokio::main]
async fn main() {
    if let Err(err) = josie_cli::run_from_env().await {
        eprintln!("error: {err}");
        std::process::exit(1);
    }
}
