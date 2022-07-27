use crate::shell::Shell;
use color_eyre::eyre::{ContextCompat, Result, WrapErr};
use directories_next::{BaseDirs, ProjectDirs};
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

    base_dirs: BaseDirs,
    project_dirs: ProjectDirs,
}

impl Context {
    pub fn new() -> Result<Self> {
        let project_dirs = ProjectDirs::from("com", "krafi", "ladybug")
            .wrap_err("No user home directory found")?;
        let config = project_dirs.config_dir().join("ladybug.toml");

        Ok(Self {
            config: Config::new(&config).wrap_err_with(|| {
                format!("Failed to load config from file '{}'", config.display())
            })?,
            base_dirs: BaseDirs::new().wrap_err("No user home directory found")?,
            project_dirs,
        })
    }
}
