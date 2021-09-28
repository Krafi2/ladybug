use crate::topic;
use anyhow::{anyhow, Context, Result};
use directories_next::{ProjectDirs, UserDirs};
use serde::Deserialize;
use std::{
    fs,
    path::{Path, PathBuf},
};

#[derive(Deserialize, Debug, Clone)]
pub struct Config {
    /// Topic directory
    pub dotfile_dir: PathBuf,
    /// Default topic config
    pub defaults: topic::TopicConfig,
    /// Relative path to the default topic
    pub default_topic: PathBuf,
    /// What to do when a file already exists
    pub duplicates: Duplicates,
}

impl Config {
    pub fn from_file(path: &Path) -> Result<Self> {
        let toml = fs::read_to_string(path)
            .with_context(|| format!("Couldn't read file '{}'", path.to_string_lossy()))?;
        toml::from_str(&toml)
            .with_context(|| format!("Error while deserializing file {}", path.to_string_lossy()))
    }

    pub fn new() -> Result<Self> {
        let path = paths::config_path()?;
        Self::from_file(&path)
    }
}

#[derive(Deserialize, Debug, Clone)]
pub enum Duplicates {
    Delete,
    Keep,
    Rename,
}

pub mod paths {
    use anyhow::{anyhow, Context, Result};
    use directories_next::{ProjectDirs, UserDirs};
    use std::path::{Path, PathBuf};

    pub fn project_dirs() -> Result<ProjectDirs> {
        ProjectDirs::from("com", "Ladybug", "ladybug")
            .ok_or(anyhow!("This system isn't supported."))
    }

    pub fn config_dir() -> Result<PathBuf> {
        project_dirs()
            .map(|dirs| dirs.config_dir().to_owned())
            .context("Failed to resolve config directory.")
    }

    pub fn user_dirs() -> Result<UserDirs> {
        UserDirs::new().ok_or(anyhow!("This system isn't supported."))
    }

    pub fn home_dir() -> Result<PathBuf> {
        user_dirs()
            .map(|dirs| dirs.home_dir().to_owned())
            .context("Failed to resolve home directory.")
    }

    pub fn config_path() -> Result<PathBuf> {
        Ok(config_dir()?.join("ladybug.toml"))
    }
}
