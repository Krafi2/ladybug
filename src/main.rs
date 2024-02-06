mod commands;
mod context;
mod shell;
mod unit;

use std::{fs::File, path::PathBuf};

use clap::Parser;
use color_eyre::eyre::{eyre, WrapErr};
use tracing::{debug, info};

use crate::context::Context;

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

#[derive(Parser)]
#[clap(author = "Krafi", version = "0.1.0", about = "A cute dotfile manager", long_about = None)]
pub struct Opts {
    #[clap(long, value_parser)]
    config: Option<PathBuf>,
    #[clap(long, value_parser)]
    dotfiles: Option<PathBuf>,
    #[clap(long, action)]
    root: bool,
    #[clap(long, action)]
    no_root: bool,
    #[clap(long, action)]
    no_cache: bool,
    #[clap(subcommand)]
    command: commands::Command,
}

impl std::fmt::Debug for Opts {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Opts")
            .field("no_root", &self.no_root)
            .field("root", &self.root)
            .field("config", &self.config)
            .field("dotfiles", &self.dotfiles)
            .finish_non_exhaustive()
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

    info!("Application started");

    let opts = Opts::try_parse()?;
    debug!(?opts);

    check_user(&opts)?;

    let mut ctx = Context::new(&opts)?;
    debug!(?ctx);

    Ok(opts.command.run(&mut ctx))
}

fn check_user(opts: &Opts) -> Result<(), Error> {
    if context::detect_root() {
        // Warn the user if the program is running as root without the `root` flag being set
        if !opts.root {
            return Err(eyre!(
                "Running as root is not recommended! Please note that ladybug will use sudo to request\
                 root access if neccessary. If you are certain that you want to do this, use the `--root` flag."
            )).map_err(Error::Eyre);
        }
    }
    Ok(())
}

fn install_tracing() -> color_eyre::Result<()> {
    let path = context::log_path().wrap_err("Cannot find log path")?;
    std::fs::create_dir_all(path.parent().unwrap()).wrap_err("Failed to create log directory")?;
    let file = File::options()
        .create(true)
        .append(true)
        .open(path)
        .wrap_err("Failed to create log file")?;

    // Set global subscriber
    tracing_subscriber::fmt()
        .compact()
        .with_ansi(false)
        .with_writer(file)
        .with_max_level(tracing_subscriber::filter::LevelFilter::DEBUG)
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
        .wrap_err("Failed to initialize eyre")
}
