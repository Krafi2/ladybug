use crate::{structures::FromArgs, Ctx, Span};
use ariadne::{Color, Fmt, ReportKind};
use color_eyre::eyre::{eyre, WrapErr};
use common::rel_path::RelPath;
use parser::Spanned;
use std::{
    fs::{Metadata, OpenOptions},
    path::{Path, PathBuf},
};

use super::{ExecutionCtx, OpResult};

#[derive(Debug)]
pub enum Error {
    InvalidMethod(String, Span),
    InvalidConflictStrat(String, Span),
    NonRelativePath(PathBuf, Span),
    PathDoesntExist(RelPath, Span),
    PathUnreachable(RelPath, std::io::Error, Span),
    SourceNotADirectory(RelPath, Span),
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
            message("Path '{}' doesn't exist", path.fg(Color::Red));
            label(span, Color::Red, "This path doesn't exist");
        }
        Error::PathUnreachable(path, err, span) => {
            report(ReportKind::Error, span.start);
            message("Cannot find path '{}'", path.fg(Color::Red));
            label(span, Color::Red, "IoError: {}", err);
        }
        Error::SourceNotADirectory(path, span) => {
            report(ReportKind::Error, span.start);
            message("Source path '{}' isn't a directory", path.fg(Color::Red));
            label(span, Color::Red, "Not a directory");
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

#[derive(Debug)]
struct Source(RelPath);

impl Source {
    fn new(rel_path: Spanned<RelPath>) -> Result<Self, Error> {
        if rel_path.is_dir() {
            Ok(Self(rel_path.inner))
        } else {
            Err(Error::SourceNotADirectory(rel_path.inner, rel_path.span))
        }
    }

    fn from_path(dir: &RelPath, path: Spanned<PathBuf>) -> Result<Self, Error> {
        join_path(dir, path)
            .and_then(check_exists)
            .and_then(Self::new)
    }

    fn file_exists(&self, path: Spanned<PathBuf>) -> Result<Spanned<PathBuf>, Error> {
        join_path(&self.0, path.clone())
            .and_then(check_exists)
            .map(|_| path)
    }
}

fn join_path(dir: &RelPath, path: Spanned<PathBuf>) -> Result<Spanned<RelPath>, Error> {
    if path.is_absolute() || path.starts_with("~") {
        Err(Error::NonRelativePath(path.inner, path.span))
    } else {
        Ok(path.map(|path| dir.join(path)))
    }
}

fn check_exists(path: Spanned<RelPath>) -> Result<Spanned<RelPath>, Error> {
    match path.try_exists() {
        Ok(true) => Ok(path),
        Ok(false) => Err(Error::PathDoesntExist(path.inner, path.span)),
        Err(err) => Err(Error::PathUnreachable(path.inner, err, path.span)),
    }
}

pub(super) struct Payload {
    method: Method,
    conflicts: Conflict,
    source: Source,
    target: RelPath,
    files: Vec<PathBuf>,
}

pub(super) struct PayloadRes {
    deployed: usize,
}

params! {
    struct Params {
        method: Option<Spanned<String>>,
        conflicts: Option<Spanned<String>>,
        source: Option<Spanned<PathBuf>>,
        target: Option<RelPath>,
    }
}

params! { struct ItemParams {} }

pub struct Provider;

impl super::ConstructProvider for Provider {
    fn new(_root: bool) -> Result<Self, super::ProviderError> {
        Ok(Provider)
    }
}

impl super::Provider for Provider {
    type Transaction = Payload;
    type State = PayloadRes;

    fn new_transaction(
        &mut self,
        args: crate::Args,
        packages: crate::Packages,
        ctx: &mut Ctx,
    ) -> Result<Self::Transaction, ()> {
        // Parse the package block params
        let params = Params::from_args(args, ctx).expect("Parser should be infallible");

        let mut degraded = params.is_degraded();
        let Params {
            method,
            conflicts,
            source,
            target,
        } = params.value;

        let method = method
            .and_then(|method| match method.as_str() {
                "link" => Some(Method::SoftLink),
                "hardlink" => Some(Method::HardLink),
                "copy" => Some(Method::Copy),
                _ => {
                    ctx.emit(Error::InvalidMethod(method.inner, method.span));
                    degraded = true;
                    None
                }
            })
            .unwrap_or(Method::SoftLink);

        let conflicts = conflicts
            .and_then(|conflicts| match conflicts.as_str() {
                "abort" => Some(Conflict::Abort),
                "rename" => Some(Conflict::Rename),
                "remove" => Some(Conflict::Remove),
                _ => {
                    ctx.emit(Error::InvalidConflictStrat(conflicts.inner, conflicts.span));
                    degraded = true;
                    None
                }
            })
            .unwrap_or(Conflict::Rename);

        let dir = ctx.unit_dir().bind(ctx.dotfile_dir().clone());
        let source = match source {
            // Attempt to create the specified source
            Some(source) => Source::from_path(&dir, source)
                .map_err(|err| ctx.emit(err))
                .ok(),
            // Default to the unit directory if no source was provided
            None => Some(Source(dir.clone())),
        };

        let mut files = Vec::new();
        for package in packages.packages {
            // Check if the referenced file exists
            let path = source.as_ref().and_then(|source| {
                source
                    .file_exists(package.name.map(PathBuf::from))
                    .map(|path| path.inner)
                    .map_err(|err| ctx.emit(err))
                    .ok()
            });

            let args = ItemParams::from_args(package.args, ctx);

            // See if anything went wrong
            match (path, args) {
                (Some(path), Some(args)) if !args.is_degraded() => files.push(path),
                _ => degraded = true,
            }
        }

        if degraded {
            Err(())
        } else {
            Ok(Payload {
                method,
                conflicts,
                source: source.unwrap(),
                target: target.unwrap(),
                files,
            })
        }
    }

    fn install(
        &mut self,
        transaction: &Self::Transaction,
        mut ctx: ExecutionCtx,
    ) -> (OpResult, Self::State) {
        let Payload {
            method,
            conflicts,
            source,
            target,
            files,
        } = transaction;

        let source = &source.0;
        tracing::debug_span!("deploy", "Deploying files from {source} to {target}");

        for (i, file) in files.iter().enumerate() {
            let source = source.join(&file);
            let dest = target.join(&file);
            match deploy_file(&source, &dest, *method, *conflicts) {
                Ok(_) => {
                    let msg = &format!("Deployed {source} to {dest}");
                    tracing::debug!("{}", msg);
                    ctx.set_message(msg);
                }
                Err(err) => {
                    tracing::error!("Failed to deploy {source} to {dest}: {err}");
                    return (Err(err), PayloadRes { deployed: i });
                }
            }
        }

        (
            Ok(()),
            PayloadRes {
                deployed: files.len(),
            },
        )
    }

    fn remove(
        &mut self,
        transaction: &Self::Transaction,
        res: Option<Self::State>,
        mut ctx: ExecutionCtx,
    ) -> OpResult {
        let Payload {
            method,
            conflicts: _,
            source,
            target,
            files,
        } = transaction;

        let source = &source.0;
        tracing::debug_span!("deploy", "Deploying files from {source} to {target}");

        let deployed = if let Some(res) = res {
            res.deployed
        } else {
            files.len()
        };

        for file in &files[..deployed] {
            let source = source.join(&file);
            let dest = target.join(&file);
            match remove_file(&source, &dest, *method) {
                Ok(_) => {
                    let msg = format!("Removed {dest}");
                    tracing::debug!("{}", msg);
                    ctx.set_message(&msg);
                }
                // Log errors but don't report to user
                Err(err) => {
                    tracing::error!("Failed to remove {dest}: {err}");
                }
            }
        }
        Ok(())
    }
}

fn backup_path(path: &Path) -> PathBuf {
    let mut name = path.file_name().expect("Expected a file").to_owned();
    name.push(".old");
    let mut backup = path.to_owned();
    backup.set_file_name(name);
    backup
}

/// Try to remove the file at `path` according to the `[Duplicates]` strategy. This could be by
/// renaming or deleting it. If this function
/// returns an error, the file couldn't be removed.
fn free_file(path: &Path, conflict: Conflict) -> color_eyre::Result<()> {
    match conflict {
        Conflict::Remove => std::fs::remove_file(path).wrap_err("Failed to remove file"),
        Conflict::Abort => Err(eyre!("Destination is occupied")),
        Conflict::Rename => {
            let new = backup_path(path);
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

/// Check if two paths point to the same file, resolving symlinks if neccessary.
fn file_eq(path1: &Path, path2: &Path) -> std::io::Result<bool> {
    let path1 = path1.canonicalize()?;
    let path2 = path2.canonicalize()?;
    Ok(path1 == path2)
}

/// Deploy `source` at `dest`. If `dest` is occupied, try to free
/// it according to the provided `[Conflict]` strategy.
fn deploy_file(
    source: &RelPath,
    dest: &RelPath,
    method: Method,
    conflict: Conflict,
) -> color_eyre::Result<()> {
    match method {
        Method::SoftLink | Method::HardLink => {
            // Create a symlink only if it isn't already present
            if !file_eq(source, dest).unwrap_or(false) {
                // Try to free the file if it exists
                let meta = if dest.exists() {
                    let meta = dest.metadata()?;
                    free_file(dest, conflict)
                        .wrap_err_with(|| format!("Failed to free file: '{}'", dest.display()))?;
                    meta
                // The parent directories may not exist
                } else {
                    let dir = dest.parent().expect("Path too short");
                    create_dirs(dir).wrap_err("Failed to create parent directories")?
                };

                match method {
                    Method::SoftLink => imp::symlink_file(source, dest).wrap_err_with(|| {
                        format!(
                            "Failed to create symlink: '{}' -> '{}'",
                            source.display(),
                            dest.display(),
                        )
                    }),
                    Method::HardLink => std::fs::hard_link(source, dest).wrap_err_with(|| {
                        format!(
                            "Failed to create hardlink: '{}' -> '{}'",
                            source.display(),
                            dest.display(),
                        )
                    }),
                    Method::Copy => unreachable!(),
                }?;
                imp::copy_permissions(dest, &meta)
                    .wrap_err_with(|| format!("Failed to set permissions at {dest}"))?;
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

            let meta = create_dirs(dest.parent().unwrap())
                .wrap_err("Failed to create parent directories")?;
            std::fs::copy(source, dest).wrap_err_with(|| {
                format!(
                    "Failed to copy file: {} -> {}",
                    source.display(),
                    dest.display()
                )
            })?;
            imp::copy_permissions(dest, &meta)
                .wrap_err_with(|| format!("Failed to set permissions at {dest}"))?;
            Ok(())
        }
    }
}

// Create dir's parents and set permissions to those of the last existing ancestor
fn create_dirs(dir: &Path) -> std::io::Result<Metadata> {
    match dir.metadata() {
        Ok(meta) => Ok(meta),
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => {
            // At least one of the ancestors should exist
            let meta = create_dirs(dir.parent().expect("Expected parent"))?;
            std::fs::create_dir(dir)?;
            imp::copy_permissions(dir, &meta)?;
            Ok(meta)
        }
        Err(err) => Err(err),
    }
}

fn remove_file(src: &RelPath, target: &RelPath, method: Method) -> color_eyre::Result<()> {
    // Check if source is deployed to target
    let eq = match method {
        Method::SoftLink | Method::HardLink => file_eq(src.absolute(), target.absolute())
            .wrap_err_with(|| format!("Failed to canonalize path: '{target}'"))?,
        Method::Copy => contents_equal(
            OpenOptions::new()
                .read(true)
                .open(src.absolute())
                .wrap_err_with(|| format!("Failed to open source file: '{src}'"))?,
            OpenOptions::new()
                .read(true)
                .open(target.absolute())
                .wrap_err_with(|| format!("Failed to open destination file: '{target}'"))?,
        ),
    };

    // Only remove the file if we are certain that it was deployed by ladybug
    if eq {
        let backup = backup_path(target);
        if backup.exists() {
            std::fs::rename(&backup, target)
                .wrap_err_with(|| format!("Failed to restore file: '{}'", backup.display()))
        } else {
            std::fs::remove_file(target)
                .wrap_err_with(|| format!("Failed to remove file: '{}'", target))
        }
    } else {
        Ok(())
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
    use std::{fs::Metadata, os::unix::prelude::MetadataExt, path::Path};

    pub fn copy_permissions(path: &Path, metadata: &Metadata) -> std::io::Result<()> {
        let owner = nix::unistd::Uid::from_raw(metadata.uid());
        let group = nix::unistd::Gid::from_raw(metadata.gid());
        nix::unistd::chown(path, Some(owner), Some(group))
            .map_err(|err| std::io::Error::from_raw_os_error(err as i32))
    }
}

#[cfg(windows)]
mod imp {
    pub use std::os::windows::fs::symlink_file;
    use std::{fs::Metadata, path::Path};

    pub fn copy_permissions(path: &Path, metadata: &Metadata) -> std::io::Result<()> {
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {

    use std::{
        fs::File,
        io::{Seek, Write},
        path::PathBuf,
    };

    use super::free_file;
    use super::Conflict;
    use common::rel_path::RelPath;
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
    fn remove_file() {
        let dir = tempdir().expect("Failed to create tempdir");
        let dir = RelPath::new::<PathBuf>(
            dir.path().to_path_buf(),
            Err(common::rel_path::HomeError::NoHome),
        )
        .unwrap();
        let src = NamedTempFile::new_in(&dir).expect("Failed to create tempfile");
        let src = dir.join(src.path().file_name().unwrap());
        let dest = dir.join("dest");

        super::imp::symlink_file(&src, &dest).expect("Failed to create symlink");
        super::remove_file(&src, &dest, super::Method::SoftLink).expect("Failed to remove file");

        assert!(!dest.exists())
    }

    #[test]
    fn remove_file_fail() {
        let dir = tempdir().expect("Failed to create tempdir");
        let dir = RelPath::new::<PathBuf>(
            dir.path().to_path_buf(),
            Err(common::rel_path::HomeError::NoHome),
        )
        .unwrap();
        let src = NamedTempFile::new_in(&dir).expect("Failed to create tempfile");
        let src = dir.join(src.path().file_name().unwrap());
        let dest = NamedTempFile::new_in(&dir).expect("Failed to create tempfile");
        let dest = dir.join(dest.path().file_name().unwrap());

        super::remove_file(&src, &dest, super::Method::SoftLink).expect("Failed to remove file");

        assert!(dest.exists())
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
        const DATA1: &[u8] = "This data should be equal.".as_bytes();
        const DATA2: &[u8] = "But is not.".as_bytes();

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
