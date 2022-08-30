mod commands;
mod context;
mod rel_path;
mod serde;
mod shell;
mod unit;

use clap::Parser;
use color_eyre::eyre::WrapErr;
use std::os::unix::process::CommandExt;

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
    let opts = commands::Opts::try_parse()?;

    // Try to elevate the process to root privileges using polkit if it's running as a user and the
    // `no-root` option isn't set.
    if !context::detect_root() && !opts.no_root {
        let err = std::process::Command::new("pkexec")
            .args(std::env::args_os())
            .exec();

        Err(err).wrap_err("Failed to elevate to root privileges")?
    }

    setup_log()?;

    let context = context::Context::new(opts.config)?;
    commands::run(&context)
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
