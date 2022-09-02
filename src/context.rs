use crate::{rel_path::RelPath, shell::Shell};
use color_eyre::eyre::{eyre, ContextCompat, Result, WrapErr};
use std::path::{Path, PathBuf};

#[derive(serde::Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Config {
    dotfiles: Option<PathBuf>,
    shell: Option<Shell>,
}

impl Config {
    fn new(conf_file: &Path) -> Result<Self> {
        let config = std::fs::read_to_string(conf_file).wrap_err("Failed to read file")?;
        toml::from_str(&config).wrap_err("Failed to deserialize config")
    }
}

pub struct Context {
    dotfile_dir: RelPath,
    home_dir: Option<PathBuf>,
    shell: Shell,
    root: bool,
}

impl Context {
    pub fn new(config: RelPath, dotfiles: Option<RelPath>) -> Result<Self> {
        // let home_dir = directories_next::UserDirs::new().map(|dirs| dirs.home_dir().to_owned());

        let config = Config::new(&config)
            .wrap_err_with(|| format!("Failed to load config from file '{}'", config.display()))?;

        let dotfiles = dotfiles
            .ok_or(())
            .or_else(|_| {
                config
                    .dotfiles
                    .ok_or_else(|| eyre!("No dotfile direcory set in config"))
                    .and_then(|path| {
                        RelPath::relative_to(path, home_dir())
                            .wrap_err("Failed to expand dotfile directory path set in config file")
                    })
            })
            .or_else(|_| default_dotfile_dir())?;

        Ok(Self {
            dotfile_dir: dotfiles,
            home_dir: home_dir().ok(),
            shell: config.shell.unwrap_or_else(default_shell),
            root: detect_root(),
        })
    }

    pub fn home_dir(&self) -> color_eyre::Result<&Path> {
        self.home_dir
            .as_ref()
            .map(PathBuf::as_path)
            .ok_or_else(|| eyre!("No user home directory found"))
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
}

pub fn detect_root() -> bool {
    nix::unistd::Uid::effective().is_root()
}

pub fn home_dir() -> Result<PathBuf> {
    directories_next::UserDirs::new()
        .map(|dirs| dirs.home_dir().to_owned())
        .ok_or_else(|| eyre!("No user home directory found"))
}

pub fn default_config_path() -> Result<PathBuf> {
    directories_next::ProjectDirs::from("com", "krafi", "ladybug")
        .map(|dirs| dirs.config_dir().join("ladybug.toml"))
        .wrap_err("No config directory found")
}

pub fn default_dotfile_dir() -> Result<RelPath> {
    RelPath::relative_to("~/ladybug".into(), home_dir())
        .wrap_err("Default dotfile directory isn't available")
}

pub fn default_shell() -> Shell {
    Shell::new(
        "/bin/bash".into(),
        ["-c", "%c"].into_iter().map(Into::into).collect(),
    )
}
