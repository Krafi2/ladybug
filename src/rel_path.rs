use crate::context::Context;
use std::path::{Path, PathBuf};

/// A path that might be relative to the home directory. Relative paths start with `~`.
#[derive(Debug, Clone)]
pub struct RelPath {
    relative: PathBuf,
    absolute: PathBuf,
}

#[derive(Debug, thiserror::Error)]
#[error("Failed to expand relative path '{path}': {err}")]
pub struct Error {
    path: PathBuf,
    err: color_eyre::Report,
}

impl RelPath {
    /// Construct a new relative path
    pub fn new(path: PathBuf, context: &Context) -> Result<Self, Error> {
        let absolute = match path.strip_prefix("~") {
            Ok(path) => match context.home_dir() {
                Ok(prefix) => Ok(prefix.join(path)),
                Err(err) => Err(Error {
                    path: path.to_owned(),
                    err,
                }),
            },
            Err(_) => Ok(path.clone()),
        }?;

        Ok(Self {
            relative: path,
            absolute,
        })
    }
}

impl std::fmt::Display for RelPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.relative.display().fmt(f)
    }
}

impl AsRef<Path> for RelPath {
    fn as_ref(&self) -> &Path {
        self.absolute.as_path()
    }
}
