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
        Self::relative_to(path, context.home_dir())
    }

    pub fn relative_to(path: PathBuf, home: color_eyre::Result<&Path>) -> Result<Self, Error> {
        let absolute = match path.strip_prefix("~") {
            Ok(path) => match home {
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

    pub fn join<T: AsRef<Path>>(&self, path: T) -> Self {
        Self {
            relative: self.relative.join(&path),
            absolute: self.absolute.join(&path),
        }
    }

    pub fn push<T: AsRef<Path>>(&mut self, path: T) {
        self.relative.push(&path);
        self.absolute.push(&path);
    }

    pub fn pop(&mut self) -> bool {
        if self.relative.pop() {
            self.absolute.pop();
            true
        } else {
            false
        }
    }

    pub fn relative(&self) -> &Path {
        &self.relative
    }

    pub fn absolute(&self) -> &Path {
        &self.absolute
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

impl std::ops::Deref for RelPath {
    type Target = PathBuf;

    fn deref(&self) -> &Self::Target {
        &self.absolute
    }
}
