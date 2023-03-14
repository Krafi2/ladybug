mod commands;
mod context;
mod shell;
mod unit;

use std::{fs::File, os::unix::process::CommandExt, path::PathBuf};

use clap::Parser;
use color_eyre::eyre::{eyre, WrapErr};
use tracing::{debug, info};

use common::rel_path::RelPath;

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
struct Opts {
    #[clap(long, action)]
    no_root: bool,
    #[clap(long, action)]
    root: bool,
    #[clap(long, value_parser)]
    config: Option<PathBuf>,
    #[clap(long, value_parser)]
    dotfiles: Option<PathBuf>,
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
    info!("Application started");
    install_eyre()?;
    install_tracing()?;

    let opts = Opts::try_parse()?;
    debug!(?opts);

    let config = opts
        .config
        .clone()
        .ok_or(())
        .or_else(|_| context::default_config_path())
        .and_then(|path| {
            RelPath::new(path, context::home_dir()).wrap_err("Failed to expand config path")
        })?;

    let dotfiles = opts
        .dotfiles
        .clone()
        .map(|path| {
            RelPath::new(path, context::home_dir())
                .wrap_err("Failed to expand dotfile directory path")
        })
        .transpose()?;

    let context = context::Context::new(config.clone(), dotfiles.clone())?;
    debug!(?context);

    // TODO allow the user to specify that some dotfile directories dont require root
    check_user(&opts)?;

    opts.command.run(&context).map_err(From::from)
}

fn check_user(opts: &Opts) -> Result<(), Error> {
    Ok(if context::detect_root() {
        // Warn the user if the program is running as root without the `root` flag being set
        if !opts.root {
            return Err(eyre!(
                "Running as root is not recommended! Please note that ladybug will use polkit to request\
                 root access if neccessary. If you are certain that you want to do this, use the `--root` flag."
            )).map_err(Error::Eyre);
        }
    } else {
        // Try to elevate the process to root privileges using sudo if it's running as a user and the
        // `no-root` option isn't set.
        if !opts.no_root {
            debug!("Running sudo");
            let mut command = std::process::Command::new("sudo");
            let mut args = std::env::args();

            // Ask sudo to preserve some variables that we need
            command.arg("--preserve-env=XDG_CACHE_HOME,XDG_CONFIG_HOME,USER,HOME");
            command.arg(args.next().unwrap()); // Get the path to the executable currently running
            command.arg("--root"); // We certainly want to run as root
            command.args(args); // Carry over the rest of the args

            let err = command.exec();

            // There was an error if the previous statement exited
            return Err(err)
                .wrap_err("Failed to elevate to root privileges")
                .map_err(From::from);
        }
    })
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
