use std::path::{Path, PathBuf};

use color_eyre::eyre::{bail, ContextCompat, Result, WrapErr};
use common::rel_path::{HomeError, RelPath};
use data::Connection;
use eval::IntoReport;
use interpreter::{Config, Interpreter};

use crate::shell::Shell;

pub struct Context {
    dotfile_dir: RelPath,
    home_dir: Result<PathBuf, HomeError>,
    shell: Shell,
    interpreter: Interpreter,
    database: data::Connection,
}

impl std::fmt::Debug for Context {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Context")
            .field("dotfile_dir", &self.dotfile_dir)
            .field("home_dir", &self.home_dir)
            .field("shell", &self.shell)
            .finish_non_exhaustive()
    }
}

impl Context {
    pub fn new(opts: &crate::Opts) -> Result<Self> {
        let config_path = opts
            .config
            .clone()
            .ok_or(())
            .or_else(|_| default_config_path())
            .and_then(|path| {
                RelPath::new(path, home_dir()).wrap_err("Failed to expand config path")
            })?;

        let dotfiles = opts
            .dotfiles
            .clone()
            .map(|path| {
                RelPath::new(path, home_dir()).wrap_err("Failed to expand dotfile directory path")
            })
            .transpose()?;

        let home_dir = home_dir();
        let interpreter = Interpreter::new();

        let config = match std::fs::read_to_string(&config_path) {
            Ok(config_src) => {
                let (config, errors) = interpreter.eval_config(&config_src, home_dir.clone());

                let errn = errors.len();
                for err in errors {
                    let filename = config_path.to_string();
                    let report = err.into_report(&filename);
                    let res = report
                        .eprint::<(&str, ariadne::Source)>((
                            &filename,
                            ariadne::Source::from(&config_src),
                        ))
                        .wrap_err("Failed to print message");
                    tracing::warn!("{res:?}");
                }

                if errn == 1 {
                    bail!("Failed to load config due to previous error");
                } else if errn > 1 {
                    bail!("Failed to load config due to {errn} previous errors")
                }

                config
            }
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => {
                println!("Config not found at '{config_path}', using default");
                Config {
                    dotfiles: None,
                    shell: None,
                }
            }
            Err(err) => {
                dbg!(&config_path);
                return Err(err).wrap_err(format!("Failed to read config at '{config_path}'"));
            }
        };

        let dotfiles = dotfiles
            .or(config.dotfiles)
            .ok_or(())
            .or_else(|_| default_dotfile_dir())?;

        let database = match opts.no_cache {
            true => Connection::open_in_memory().map_err(color_eyre::Report::new),
            false => default_database_path()
                .and_then(|p| Connection::open(&p).map_err(color_eyre::Report::new)),
        }
        .wrap_err("Failed to initialize database")?;

        Ok(Self {
            dotfile_dir: dotfiles,
            home_dir,
            shell: config.shell.map(Shell::from).unwrap_or_else(default_shell),
            interpreter,
            database,
        })
    }

    pub fn dotfile_dir(&self) -> &RelPath {
        &self.dotfile_dir
    }

    pub fn default_shell(&self) -> &Shell {
        &self.shell
    }

    pub fn home_dir(&self) -> Result<&Path, HomeError> {
        self.home_dir
            .as_ref()
            .map(PathBuf::as_path)
            .map_err(Clone::clone)
    }

    pub fn interpreter(&mut self) -> &mut Interpreter {
        &mut self.interpreter
    }

    pub fn database(&mut self) -> &mut Connection {
        &mut self.database
    }

    pub fn database_interpreter(&mut self) -> (&mut Connection, &mut Interpreter) {
        (&mut self.database, &mut self.interpreter)
    }
}

pub fn detect_root() -> bool {
    nix::unistd::Uid::effective().is_root()
}

pub fn home_dir() -> Result<PathBuf, common::rel_path::HomeError> {
    directories_next::UserDirs::new()
        .map(|dirs| dirs.home_dir().to_owned())
        .ok_or(common::rel_path::HomeError::NoHome)
}

pub fn default_config_path() -> Result<PathBuf> {
    project_dirs()
        .map(|dirs| dirs.config_dir().join("config.ldbg"))
        .wrap_err("No config directory found")
}

fn default_database_path() -> Result<PathBuf> {
    project_dirs()
        .map(|dirs| dirs.cache_dir().join("database.sqlite"))
        .wrap_err("No cache directory found")
}

pub fn project_dirs() -> Option<directories_next::ProjectDirs> {
    directories_next::ProjectDirs::from("com", "krafi", "ladybug")
}

pub fn log_path() -> Result<PathBuf> {
    project_dirs()
        .map(|dirs| dirs.cache_dir().join("ladybug.log"))
        .wrap_err("No home directory found")
}

pub fn default_dotfile_dir() -> Result<RelPath> {
    RelPath::new("~/dotfiles".into(), home_dir())
        .wrap_err("Default dotfile directory isn't available")
}

pub fn default_shell() -> Shell {
    Shell::new(
        "/bin/bash".into(),
        ["-c", "%c"].into_iter().map(Into::into).collect(),
    )
}
