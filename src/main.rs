mod commands;
mod context;
mod serde;
mod shell;
mod unit;

use clap::Parser;
use color_eyre::eyre::{bail, WrapErr};
use common::rel_path::RelPath;
use std::os::unix::process::CommandExt;

fn main() {
    match run() {
        Ok(res) => match res {
            Ok(_) => std::process::exit(0),
            Err(_) => std::process::exit(1),
        },
        Err(err) => {
            eprintln!("{:?}", err);
            std::process::exit(1);
        }
    }
}

fn run() -> color_eyre::Result<Result<(), ()>> {
    setup_log()?;

    let opts = commands::Opts::try_parse()?;
    let config = opts
        .config
        .ok_or(())
        .or_else(|_| context::default_config_path())
        .and_then(|path| {
            RelPath::new(path, context::home_dir()).wrap_err("Failed to expand config path")
        })?;

    let dotfiles = opts.dotfiles.map(|path| {
        RelPath::new(path, context::home_dir()).wrap_err("Failed to expand dotfile directory path")
    });

    if context::detect_root() {
        if !opts.root {
            bail!(
                "Running as root is not recommended! Please note that ladybug will use polkit to request\
                 root access if neccessary, but if you are certain that you want to do this, add the `--root` flag."
            )
        }
    } else {
        if !opts.no_root {
            // Try to elevate the process to root privileges using polkit if it's running as a user and the
            // `no-root` option isn't set.
            let mut command = std::process::Command::new("pkexec");
            let mut new_args = Vec::<std::ffi::OsString>::new();
            let mut args = std::env::args();

            new_args.push(args.next().unwrap().into());

            new_args.push("--config".into());
            new_args.push(config.relative().into());

            let dotfiles = dotfiles.unwrap_or_else(|| context::default_dotfile_dir())?;
            new_args.push("--dotfiles".into());
            new_args.push(dotfiles.relative().into());

            new_args.push("--root".into());

            let mut skip = true;
            while let Some(arg) = args.next() {
                if skip {
                    if !arg.starts_with('-') {
                        new_args.push(arg.into());
                        skip = false;
                    }
                } else {
                    new_args.push(arg.into());
                }
            }

            command.args(new_args);
            let err = command.exec();
            return Err(err).wrap_err("Failed to elevate to root privileges");
        }
    }

    let context = context::Context::new(config, dotfiles.map(|inner| inner.ok()).flatten())?;
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

    // Install color_eyre panic and error handlers
    color_eyre::install()?;
    Ok(())
}
