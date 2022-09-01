use crate::{rel_path::RelPath, shell::Shell};
use color_eyre::eyre::{eyre, ContextCompat, Result, WrapErr};
use std::path::{Path, PathBuf};

#[derive(serde::Deserialize)]
#[serde(deny_unknown_fields, default)]
pub struct Config {
    dotfile_dir: PathBuf,
    shell: Shell,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            dotfile_dir: "~/ladybug".into(),
            shell: Shell::new(
                "/bin/bash".into(),
                ["-c", "%c"].into_iter().map(Into::into).collect(),
            ),
        }
    }
}

impl Config {
    fn new(conf_file: &Path) -> Result<Self> {
        let config = std::fs::read_to_string(conf_file).wrap_err("Failed to read file")?;
        toml::from_str(&config).wrap_err("Failed to deserialize config")
    }
}

pub struct Context {
    config: Config,
    dotfile_dir: RelPath,
    home_dir: Option<PathBuf>,
    root: bool,
}

impl Context {
    pub fn new(config: Option<PathBuf>) -> Result<Self> {
        let home_dir = directories_next::UserDirs::new().map(|dirs| dirs.home_dir().to_owned());

        let project_dirs = directories_next::ProjectDirs::from("com", "krafi", "ladybug");

        let config = config
            .or_else(|| project_dirs.map(|dirs| dirs.config_dir().join("ladybug.toml")))
            .wrap_err("No config directory found")
            .and_then(|path| {
                Config::new(&path).wrap_err_with(|| {
                    format!("Failed to load config from file '{}'", path.display())
                })
            })
            .unwrap_or_else(|err| {
                tracing::warn!("{}", err);
                Config::default()
            });

        Ok(Self {
            dotfile_dir: RelPath::relative_to(
                config.dotfile_dir.clone(),
                home_dir
                    .as_deref()
                    .ok_or_else(|| eyre!("No user home directory found")),
            )
            .wrap_err_with(|| {
                format!(
                    "Failed to expand dotfile dir path: '{}'",
                    &config.dotfile_dir.display()
                )
            })?,
            config,
            home_dir,
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
        &self.config.shell
    }
}

pub fn detect_root() -> bool {
    nix::unistd::Uid::effective().is_root()
}
