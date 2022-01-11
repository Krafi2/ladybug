use crate::topic::Duplicates;
use anyhow::{anyhow, Context, Result};
use std::{
    fs::{self, OpenOptions},
    io::{ErrorKind, Read, Write},
    path::{Path, PathBuf},
};

pub fn rebase_path(path: &Path, current: &Path, target: &Path) -> Result<PathBuf> {
    Ok(target.join(path.strip_prefix(current)?))
}

/// Check if two paths point to the same file, resolving symlinks if neccessary.
fn file_eq(path1: &Path, path2: &Path) -> Result<bool> {
    let path1 = path1.canonicalize()?;
    let path2 = path2.canonicalize()?;
    Ok(path1 == path2)
}

/// Try to remove the file at `path` according to the `[Duplicates]` strategy. This could be by
/// renaming or deleting it. If this function
/// returns an error, the file couldn't be removed.
fn free_file(path: &Path, duplicates: Duplicates) -> Result<()> {
    match duplicates {
        Duplicates::Delete => fs::remove_file(path).context("Failed to remove file"),
        Duplicates::Keep => Err(anyhow!("File already present")),
        Duplicates::Rename => {
            let mut name = path.file_name().expect("Expected a file").to_owned();
            name.push(".old");
            let new = path.join(name);

            fs::rename(path, &new).with_context(|| {
                format!(
                    "Failed to rename file: '{}' -> '{}'",
                    path.display(),
                    new.display()
                )
            })
        }
    }
}

/// Creates a new symlink at `link`, pointing to `original`. If there is a file at `link`, try to free
/// it according to the provided `[Duplicates]` strategy.
pub fn place_symlink(original: &Path, link: &Path, duplicates: Duplicates) -> Result<()> {
    // Create a symlink only if it isn't already present
    if !file_eq(original, link).unwrap_or(false) {
        // Try to free the file if it exists
        if link.exists() {
            free_file(link, duplicates)
                .with_context(|| format!("Failed to free file: '{}'", link.display()))?;
        // The parent directories may not exist
        } else {
            let dir = link.parent().expect("Path too short");
            std::fs::create_dir_all(dir).context("Failed to create parent directories")?;
        }

        imp::symlink_file(original, link).with_context(|| {
            format!(
                "Failed to create symlink: '{}' -> '{}'",
                link.display(),
                original.display(),
            )
        })?;
    }
    Ok(())
}

pub fn place_file(path: &Path, contents: &[u8], duplicates: Duplicates) -> Result<()> {
    let clean_file = match OpenOptions::new().read(true).open(path) {
        Ok(file) => {
            match contents_equal(&file, <&[u8]>::clone(&contents)) {
                // Contents are equal, our job is already done
                true => return Ok(()),
                // The contents arent the same so we will need to free the file
                false => true,
            }
        }
        Err(err) => match err.kind() {
            // The path is free so no need to free it
            ErrorKind::NotFound => false,
            // Who knows, there's probably a file so we will try to free it
            _ => true,
        },
    };

    if clean_file {
        free_file(path, duplicates)
            .with_context(|| format!("Failed to free file: '{}'", path.display()))?;
    }

    let mut file = OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(path)
        .with_context(|| format!("Failed to open file: '{}'", path.display()))?;

    file.write_all(contents)
        .with_context(|| format!("Failed to write to file: '{}'", path.display()))
}

// I think this implementation doesn't cover a lot of the edge cases, but we will see how it goes.
/// Compare the contents of two readers to check if they are equal. If either reader returns an
/// error, the function returns false.
fn contents_equal<A, B>(mut reader1: A, mut reader2: B) -> bool
where
    A: Read,
    B: Read,
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
