use crate::{structures::FromArgs, Ctx, Span};
use ariadne::{Color, Fmt, ReportKind};
use color_eyre::eyre::{eyre, WrapErr};
use common::rel_path::RelPath;
use std::{
    fs::OpenOptions,
    path::{Path, PathBuf},
};

pub enum Error {
    InvalidMethod(String, Span),
    InvalidConflictStrat(String, Span),
    NonRelativePath(PathBuf, Span),
    PathDoesntExist(PathBuf, Span),
}

impl Into<crate::error::Error> for Error {
    fn into(self) -> crate::error::Error {
        super::TransactionError::Files(self).into()
    }
}

report! {
    Error {
        Error::InvalidMethod(found, span) => {
            report(ReportKind::Error, span.start);
            message("Unknown method '{}'", found.as_str().fg(Color::Red));
            label(span, Color::Red, "Expected one of 'link', 'hardlink', or 'copy', but found '{}'", found.fg(Color::Red));
        }
        Error::InvalidConflictStrat(found, span) => {
            report(ReportKind::Error, span.start);
            message("Unknown conflict strategy '{}'", found.as_str().fg(Color::Red));
            label(span, Color::Red, "Expected one of 'abort', 'rename', or 'remove', but found '{}'", found.fg(Color::Red));
        }
        Error::NonRelativePath(path, span) => {
            report(ReportKind::Error, span.start);
            message("Non-relative path '{}'", path.display().fg(Color::Red));
            label(span, Color::Red, "This path must be relative");
        }
        Error::PathDoesntExist(path, span) => {
            report(ReportKind::Error, span.start);
            message("Non-existent path '{}'", path.display().fg(Color::Red));
            label(span, Color::Red, "This path doesn't exist");
        }
    }
}

#[derive(Clone, Copy)]
enum Method {
    SoftLink,
    HardLink,
    Copy,
}

#[derive(Clone, Copy)]
enum Conflict {
    Abort,
    Rename,
    Remove,
}

struct Source(RelPath);

impl Source {
    fn from_path(path: (PathBuf, Span), dir: &RelPath) -> Result<Self, Error> {
        let (path, span) = path;
        if path.is_absolute() || path.starts_with("~") {
            Err(Error::NonRelativePath(path, span))
        } else {
            let relpath = dir.join(&path);
            if relpath.exists() {
                Ok(Self(relpath))
            } else {
                Err(Error::PathDoesntExist(path, span))
            }
        }
    }
}

struct Payload {
    method: Method,
    conflicts: Conflict,
    source: Source,
    target: RelPath,
    files: Vec<PathBuf>,
}

params! {
    struct Params {
        method: Result<(String, Span), ()>,
        conflicts: Result<(String, Span), ()>,
        source: Result<(PathBuf, Span), ()>,
        target: Result<RelPath, ()>,
    }
}

params! { struct ItemParams {} }

pub struct Provider;

impl super::Transactor for Provider {
    fn new_transaction(
        &mut self,
        args: crate::Args,
        packages: crate::Packages,
        context: &mut Ctx,
    ) -> Result<super::Transaction, ()> {
        // Parse the package block params
        let Params {
            method,
            conflicts,
            source,
            target,
        } = Params::from_args(args, context).expect("Parser should be infallible");

        let mut degraded = false;

        let method = method.and_then(|method| match method.0.as_str() {
            "link" => Ok(Method::SoftLink),
            "hardlink" => Ok(Method::HardLink),
            "copy" => Ok(Method::Copy),
            _ => {
                context.emit(Error::InvalidMethod(method.0, method.1));
                degraded = true;
                Err(())
            }
        });

        let conflicts = conflicts.and_then(|conflicts| match conflicts.0.as_str() {
            "abort" => Ok(Conflict::Abort),
            "rename" => Ok(Conflict::Rename),
            "remove" => Ok(Conflict::Remove),
            _ => {
                context.emit(Error::InvalidConflictStrat(conflicts.0, conflicts.1));
                degraded = true;
                Err(())
            }
        });

        let source = source.and_then(|source| {
            Source::from_path(source, context.unit_dir()).map_err(|err| {
                context.emit(err);
                degraded = true;
            })
        });

        let mut files = Vec::new();
        let dir = source
            .as_ref()
            .map(|source| context.unit_dir().join(&source.0));
        for package in packages.packages {
            let path = dir.as_ref().map_err(|_| ()).and_then(|dir| {
                let path = PathBuf::from(package.name.0);
                match Source::from_path((path.clone(), package.name.1), dir) {
                    Ok(_) => Ok(path),
                    Err(err) => {
                        context.emit(err);
                        Err(())
                    }
                }
            });
            let args = ItemParams::from_args(package.args, context);
            if let (Ok(path), Ok(_args)) = (path, args) {
                files.push(path)
            } else {
                degraded = true;
            }
        }

        if degraded {
            Err(())
        } else {
            Ok(super::Transaction {
                provider: super::ProviderKind::Files.into(),
                payload: Box::new(Payload {
                    method: method.unwrap(),
                    conflicts: conflicts.unwrap(),
                    source: source.unwrap(),
                    target: target.unwrap(),
                    files,
                }),
            })
        }
    }
}

impl super::Provider for Provider {
    fn install(&mut self, transaction: super::Transaction) -> color_eyre::Result<()> {
        assert_eq!(transaction.provider, super::ProviderKind::Files.into());
        let Payload {
            method,
            conflicts,
            source,
            target,
            files,
        } = *transaction
            .payload
            .downcast::<Payload>()
            .expect("Wrong type");

        let source = source.0;
        tracing::info_span!("deploy", "Deploying files from {source} to {target}");
        for file in files {
            let source = source.join(&file);
            let dest = target.join(&file);
            match deploy_file(&source, &dest, method, conflicts) {
                Ok(_) => tracing::info!("Deployed {source} to {dest}"),
                Err(err) => tracing::error!("Failed to deploy {source} to {dest}: {err}"),
            }
        }

        todo!()
    }

    fn remove(&mut self, _transaction: super::Transaction) -> color_eyre::Result<()> {
        todo!()
    }
}

impl super::ProviderPrivate for Provider {
    fn new(_root: bool) -> Result<Self, super::ProviderError> {
        Ok(Provider)
    }
}

/// Try to remove the file at `path` according to the `[Duplicates]` strategy. This could be by
/// renaming or deleting it. If this function
/// returns an error, the file couldn't be removed.
fn free_file(path: &Path, conflict: Conflict) -> color_eyre::Result<()> {
    match conflict {
        Conflict::Remove => std::fs::remove_file(path).wrap_err("Failed to remove file"),
        Conflict::Abort => Err(eyre!("Destination is occupied")),
        Conflict::Rename => {
            let mut name = path.file_name().expect("Expected a file").to_owned();
            name.push(".old");
            let mut new = path.to_owned();
            new.set_file_name(name);

            std::fs::rename(path, &new).wrap_err_with(|| {
                format!(
                    "Failed to rename file {} -> {}",
                    path.display(),
                    new.display()
                )
            })
        }
    }
}

pub fn rebase_path(
    path: &Path,
    current: &Path,
    target: &Path,
) -> Result<PathBuf, std::path::StripPrefixError> {
    Ok(target.join(path.strip_prefix(current)?))
}

/// Check if two paths point to the same file, resolving symlinks if neccessary.
fn file_eq(path1: &Path, path2: &Path) -> std::io::Result<bool> {
    let path1 = path1.canonicalize()?;
    let path2 = path2.canonicalize()?;
    Ok(path1 == path2)
}

/// Deploy `source` at `dest`. If `dest` is occupied, try to free
/// it according to the provided `[Duplicates]` strategy.
fn deploy_file(
    source: &RelPath,
    dest: &RelPath,
    method: Method,
    conflict: Conflict,
) -> color_eyre::Result<()> {
    match method {
        Method::SoftLink | Method::HardLink => {
            // Create a symlink only if it isn't already present
            if file_eq(source, dest).unwrap_or(false) {
                // Try to free the file if it exists
                if dest.exists() {
                    free_file(dest, conflict)
                        .wrap_err_with(|| format!("Failed to free file: '{}'", dest.display()))?;
                // The parent directories may not exist
                } else {
                    let dir = dest.parent().expect("Path too short");
                    std::fs::create_dir_all(dir).context("Failed to create parent directories")?;
                }

                match method {
                    Method::SoftLink => imp::symlink_file(source, dest).wrap_err_with(|| {
                        format!(
                            "Failed to create symlink: '{}' -> '{}'",
                            dest.display(),
                            source.display(),
                        )
                    }),
                    Method::HardLink => std::fs::hard_link(source, dest).wrap_err_with(|| {
                        format!(
                            "Failed to create hardlink: '{}' -> '{}'",
                            dest.display(),
                            source.display(),
                        )
                    }),
                    Method::Copy => unreachable!(),
                }?;
            }
            Ok(())
        }
        Method::Copy => {
            let src = OpenOptions::new()
                .read(true)
                .open(source)
                .wrap_err_with(|| format!("Failed to read source file {}", source.display()))?;

            let occupied = match OpenOptions::new().read(true).open(dest) {
                Ok(dest) => {
                    match contents_equal(src, &dest) {
                        // Contents are equal, our job is already done
                        true => return Ok(()),
                        // The contents arent the same so we will need to free the file
                        false => true,
                    }
                }
                Err(err) => match err.kind() {
                    // The path is free so no need to free it
                    std::io::ErrorKind::NotFound => false,
                    // Who knows, there's probably a file so we will try to free it
                    _ => true,
                },
            };

            if occupied {
                free_file(dest, conflict)
                    .wrap_err_with(|| format!("Failed to free file: '{}'", dest.display()))?;
            }

            std::fs::copy(source, dest).wrap_err_with(|| {
                format!(
                    "Failed to copy file: {} -> {}",
                    source.display(),
                    dest.display()
                )
            })?;
            Ok(())
        }
    }
}

// I think this implementation doesn't cover a lot of the edge cases, but we will see how it goes.
/// Compare the contents of two readers to check if they are equal. If either reader returns an
/// error, the function returns false.
fn contents_equal<A, B>(mut reader1: A, mut reader2: B) -> bool
where
    A: std::io::Read,
    B: std::io::Read,
{
    // Is it ok to allocate buffers on the stack? I've seen c code do it.
    let mut buf1 = [0; 1024];
    let mut buf2 = [0; 1024];

    loop {
        match reader1.read(&mut buf1) {
            Ok(len1) => match reader2.read(&mut buf2) {
                Ok(len2) => {
                    // The readers might have reached the end at a different lenght
                    if len1 != len2 {
                        break false;
                    }
                    // We might have reached the end of both readers, which means they are equal
                    if len1 == 0 {
                        break true;
                    }
                    if &buf1[..len1] != &buf2[..len2] {
                        break false;
                    }
                }
                Err(_) => break false,
            },
            Err(_) => break false,
        }
    }
}

#[cfg(any(target_os = "redox", unix))]
mod imp {
    pub use std::os::unix::fs::symlink as symlink_file;
}

#[cfg(windows)]
mod imp {
    pub use std::os::windows::fs::symlink_file;
}

#[cfg(test)]
mod tests {
    use std::{
        fs::File,
        io::{Seek, Write},
    };

    use super::free_file;
    use super::Conflict;
    use tempfile::{tempdir, tempfile, NamedTempFile};

    #[test]
    fn delete_conficting_file() {
        let dir = tempdir().expect("Failed to create tempdir");
        let file = NamedTempFile::new_in(&dir).expect("Failed to create tempfile");
        free_file(file.path(), Conflict::Remove).expect("Failed to free file");

        assert!(
            dir.path()
                .read_dir()
                .expect("Failed to read dir")
                .next()
                .is_none(),
            "Directory should be empty"
        );
    }

    #[test]
    fn rename_conflicting_file() {
        const NAME: &'static str = "test.test";
        const NEW_NAME: &'static str = "test.test.old";

        let dir = tempdir().expect("Failed to create tempdir");
        let path = dir.path().join(NAME);
        std::fs::write(&path, []).expect("Failed to create file");
        free_file(&path, Conflict::Rename).expect("Failed to free file");

        assert_eq!(
            dir.path()
                .read_dir()
                .expect("Failed to read dir")
                .next()
                .expect("Expected a file")
                .expect("Failed to read entry")
                .file_name(),
            NEW_NAME
        );
    }

    #[test]
    fn keep_conficting_file() {
        let dir = tempdir().expect("Failed to create tempdir");
        let file = NamedTempFile::new_in(&dir).expect("Failed to create tempfile");
        let res = free_file(file.path(), Conflict::Abort);
        assert!(res.is_err());

        assert_eq!(
            dir.path()
                .read_dir()
                .expect("Failed to read dir")
                .next()
                .expect("Expected a file")
                .expect("Failed to read entry")
                .path(),
            file.path()
        );
    }

    #[test]
    fn contents_equal() {
        const DATA: &[u8] = "This is data that should be equal.".as_bytes();

        let mut file1 = tempfile().expect("Failed to create file");
        let mut file2 = tempfile().expect("Failed to create file");

        file1.write_all(DATA).expect("Failed to write data");
        file2.write_all(DATA).expect("Failed to write data");

        let res = super::contents_equal(file1, file2);
        assert!(res, "File contents should be detected as equal");
    }

    #[test]
    fn contents_not_equal() {
        const DATA1: &[u8] = "This is data that should be equal.".as_bytes();
        const DATA2: &[u8] = "This data is different.".as_bytes();

        fn make_file(data: &[u8]) -> File {
            let mut file = tempfile().expect("Failed to create file");
            file.write_all(data).expect("Failed to write data");
            file.flush().unwrap();
            file.rewind().unwrap();
            file
        }

        let file1 = make_file(DATA1);
        let file2 = make_file(DATA2);

        let res = super::contents_equal(file1, file2);
        assert!(!res, "File contents should be detected as different");
    }
}
