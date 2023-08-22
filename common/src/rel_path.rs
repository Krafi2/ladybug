use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Copy)]
pub enum HomeError {
    NoHome,
}

impl std::fmt::Display for HomeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Home directory not found")
    }
}

impl std::error::Error for HomeError {}

#[derive(Debug)]
pub struct Error {
    path: PathBuf,
    err: HomeError,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Failed to expand relative path '{}'",
            self.path.display(),
        )
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(&self.err)
    }
}

/// A path that might be relative to the home directory. Relative paths start with `~`.
#[derive(Debug, Clone)]
pub struct RelPath {
    relative: PathBuf,
    absolute: PathBuf,
}

impl RelPath {
    /// Construct a new relative path
    pub fn new<P: AsRef<Path>>(path: PathBuf, home: Result<P, HomeError>) -> Result<Self, Error> {
        if let Ok(rel) = path.strip_prefix("~") {
            return match home {
                Ok(home) => Ok(RelPath {
                    absolute: home.as_ref().join(rel),
                    relative: path,
                }),
                Err(err) => Err(Error { path, err }),
            };
        }
        if let Ok(home) = home {
            if let Ok(rel) = path.strip_prefix(home) {
                return Ok(RelPath {
                    relative: PathBuf::from("~").join(rel),
                    absolute: path,
                });
            }
        }
        Ok(RelPath {
            relative: path.clone(),
            absolute: path.clone(),
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
