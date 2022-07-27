mod commands;
mod config;
mod rel_path;
mod serde;
mod shell;
mod topic;

fn main() {
    match run() {
        Ok(_) => std::process::exit(0),
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(1);
        }
    }
}

fn run() -> color_eyre::Result<()> {
    setup_log()?;
    Ok(())
}

fn setup_log() -> color_eyre::Result<()> {
    // Disable eyre's spantrace
    if std::env::var("RUST_SPANTRACE").is_err() {
        std::env::set_var("RUST_SPANTRACE", "0");
    }

    // Set global subscriber
    let subscriber = tracing_subscriber::FmtSubscriber::new();
    tracing::subscriber::set_global_default(subscriber)?;
    Ok(())
}
