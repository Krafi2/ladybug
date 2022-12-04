use std::process::Output;

use color_eyre::{eyre::eyre, Section, SectionExt};

#[derive(Debug)]
pub enum Error {
    Spawn(std::io::Error),
    IO(std::io::Error),
    Failed(Output),
}

impl Error {
    pub fn into_report(self) -> color_eyre::Report {
        match self {
            Error::Spawn(err) => eyre!(err).wrap_err("Cannot start process"),
            Error::IO(err) => eyre!(err),
            Error::Failed(output) => {
                let mut err = match output.status.code() {
                    Some(code) => eyre!("Process failed with exit code {code}"),
                    None => eyre!("Process was terminated by a signal"),
                };
                if !output.stderr.is_empty() {
                    err = err.section(
                        String::from_utf8_lossy(&output.stderr)
                            .as_ref()
                            .trim()
                            .to_owned()
                            .header("Stderr:"),
                    )
                };
                err
            }
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

pub fn run_command(mut cmd: std::process::Command) -> Result<Output, Error> {
    let child = cmd.spawn().map_err(Error::Spawn)?;
    let output = child.wait_with_output().map_err(Error::IO)?;

    if output.status.success() {
        Ok(output)
    } else {
        Err(Error::Failed(output))
    }
}
