mod commands;
mod context;
mod serde;
mod shell;
mod unit;

use clap::Parser;
use color_eyre::eyre::{eyre, WrapErr};
use common::rel_path::RelPath;
use std::{fs::File, os::unix::process::CommandExt};

enum Error {
    Clap(clap::Error),
    Eyre(color_eyre::Report),
}

impl From<clap::Error> for Error {
    fn from(err: clap::Error) -> Self {
        Self::Clap(err)
    }
}

impl From<color_eyre::Report> for Error {
    fn from(err: color_eyre::Report) -> Self {
        Self::Eyre(err)
    }
}

fn main() {
    match run() {
        Ok(res) => match res {
            Ok(_) => std::process::exit(0),
            Err(_) => std::process::exit(1),
        },
        Err(err) => {
            match err {
                Error::Clap(err) => err.print().expect("Error writing error"),
                Error::Eyre(err) => eprintln!("Error: {err:?}"),
            }
            std::process::exit(1);
        }
    }
}

fn run() -> Result<Result<(), ()>, Error> {
    install_eyre()?;
    install_tracing()?;

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
            return Err(eyre!(
                "Running as root is not recommended! Please note that ladybug will use polkit to request\
                 root access if neccessary. If you are certain that you want to do this, use the `--root` flag."
            )).map_err(Error::Eyre);
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
            return Err(err)
                .wrap_err("Failed to elevate to root privileges")
                .map_err(From::from);
        }
    }

    let context = context::Context::new(config, dotfiles.map(|inner| inner.ok()).flatten())?;
    commands::run(&context).map_err(From::from)
}

fn install_tracing() -> color_eyre::Result<()> {
    let path = context::log_path().wrap_err("Cannot find log path")?;
    std::fs::create_dir_all(path.parent().unwrap()).wrap_err("Failed to create log directory")?;
    let file = File::create(path).wrap_err("Failed to create log file")?;

    // Set global subscriber
    tracing_subscriber::fmt()
        .compact()
        .with_writer(file)
        .try_init()
        .map_err(|err| eyre!(err))
        .wrap_err("Failed to initialize tracing subscriber")
}

fn install_eyre() -> color_eyre::Result<()> {
    // Install color_eyre panic and error handlers
    color_eyre::config::HookBuilder::new()
        .display_env_section(false)
        .display_location_section(false)
        .install()
}
