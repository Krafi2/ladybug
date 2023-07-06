use std::process::Output;

use color_eyre::{eyre::eyre, Section, SectionExt};

#[derive(Debug)]
pub enum Error {
    Spawn(std::io::Error),
    IO(std::io::Error),
    Failed { stderr: Vec<u8>, code: Option<i32> },
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::Spawn(err) => Some(err),
            Error::IO(_) => None,
            Error::Failed { .. } => None,
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Spawn(_) => f.write_str("Cannot start process"),
            Error::IO(err) => err.fmt(f),
            Error::Failed { code, .. } => match code {
                Some(code) => write!(f, "Process failed with exit code {code}"),
                None => f.write_str("Process was terminated by a signal"),
            },
        }
    }
}

impl Error {
    pub fn into_report(self) -> color_eyre::Report {
        let section = if let Error::Failed { stderr, .. } = &self {
            if stderr.is_empty() {
                None
            } else {
                Some(
                    String::from_utf8_lossy(stderr)
                        .as_ref()
                        .trim()
                        .to_owned()
                        .header("Stderr:"),
                )
            }
        } else {
            None
        };
        let err = eyre!(self);
        if let Some(section) = section {
            err.section(section)
        } else {
            err
        }
    }
}

#[derive(Debug, Clone)]
pub struct Command {
    pub cmd: String,
    pub args: Vec<String>,
}

impl Command {
    pub fn new(cmd: String, args: Vec<String>) -> Self {
        Self { cmd, args }
    }

    pub fn builder(&self) -> std::process::Command {
        let mut builder = std::process::Command::new(&self.cmd);
        builder.args(&self.args);
        builder
    }
}

pub fn run_command(cmd: &mut std::process::Command) -> Result<Output, Error> {
    let child = cmd.spawn().map_err(Error::Spawn)?;
    let output = child.wait_with_output().map_err(Error::IO)?;
    check_output(output)
}

pub fn check_output(output: Output) -> Result<Output, Error> {
    if output.status.success() {
        Ok(output)
    } else {
        Err(Error::Failed {
            stderr: output.stderr,
            code: output.status.code(),
        })
    }
}
