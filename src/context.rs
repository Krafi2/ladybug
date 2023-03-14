use color_eyre::eyre::{eyre, ContextCompat, Result, WrapErr};
use common::rel_path::{HomeError, RelPath};
use interpreter::Interpreter;
use std::path::{Path, PathBuf};

use crate::shell::Shell;

pub struct Context {
    dotfile_dir: RelPath,
    home_dir: Result<PathBuf, HomeError>,
    shell: Shell,
    root: bool,
    interpreter: Interpreter,
}

impl std::fmt::Debug for Context {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Context")
            .field("dotfile_dir", &self.dotfile_dir)
            .field("home_dir", &self.home_dir)
            .field("shell", &self.shell)
            .field("root", &self.root)
            .finish_non_exhaustive()
    }
}

impl Context {
    pub fn new(config_path: RelPath, dotfiles: Option<RelPath>) -> Result<Self> {
        let home_dir = home_dir();
        let interpreter = Interpreter::new();

        let config_src = std::fs::read_to_string(&config_path)
            .wrap_err_with(|| format!("Failed to read config from file '{}'", config_path))?;
        let (config, errors) =
            interpreter.eval_config(&config_src, home_dir.as_deref().map_err(Clone::clone));

        let errn = errors.len();
        for err in errors {
            let filename = &config_path.to_string();
            let report = err.into_report(&filename);
            let res = report
                .eprint::<(&str, ariadne::Source)>((&filename, ariadne::Source::from(&config_src)))
                .wrap_err("Failed to print message");
            tracing::warn!("{res:?}");
        }
        if errn > 0 {
            color_eyre::eyre::bail!("Failed to load config due to {errn} previous errors");
        }

        let dotfiles = dotfiles
            .ok_or(())
            .or_else(|_| {
                config
                    .dotfiles
                    .ok_or_else(|| eyre!("No dotfile direcory set in config"))
            })
            .or_else(|_| default_dotfile_dir())?;

        Ok(Self {
            dotfile_dir: dotfiles,
            home_dir,
            shell: config.shell.map(From::from).unwrap_or_else(default_shell),
            root: detect_root(),
            interpreter,
        })
    }

    pub fn is_root(&self) -> bool {
        self.root
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

    pub fn interpreter(&self) -> &Interpreter {
        &self.interpreter
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

pub fn project_dirs() -> Option<directories_next::ProjectDirs> {
    directories_next::ProjectDirs::from("com", "krafi", "ladybug")
}

pub fn log_path() -> Result<PathBuf> {
    project_dirs()
        .map(|dirs| dirs.cache_dir().join("ladybug.log"))
        .wrap_err("No home directory found")
}

pub fn default_dotfile_dir() -> Result<RelPath> {
    RelPath::new("~/ladybug".into(), home_dir())
        .wrap_err("Default dotfile directory isn't available")
}

pub fn default_shell() -> Shell {
    Shell::new(
        "/bin/bash".into(),
        ["-c", "%c"].into_iter().map(Into::into).collect(),
    )
}
