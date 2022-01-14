use crate::serde::optional_string_or_struct;
use anyhow::{anyhow, Context};
use serde::{Deserialize, Serialize};
use std::{
    ffi::{OsStr, OsString},
    fmt::Display,
    path::{Path, PathBuf},
    process::{Command, ExitStatus, Stdio},
    str::FromStr,
};

const CMD_PATTERN: &'static str = "{{cmd}}";

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(deny_unknown_fields)]
pub struct HookConfig {
    /// A command to run before deploying
    #[serde(deserialize_with = "optional_string_or_struct")]
    pub pre: Option<Hook>,
    /// A command to run after deploying
    #[serde(deserialize_with = "optional_string_or_struct")]
    pub post: Option<Hook>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
#[serde(rename_all = "lowercase")]
#[serde(deny_unknown_fields)]
pub enum Hook {
    File { file: PathBuf },
    Command { cmd: String },
}

impl Hook {
    pub fn run(
        &self,
        source: &Path,
        target: &Path,
        shell: &[String],
    ) -> anyhow::Result<ExitStatus> {
        let stdin = Stdio::inherit();
        let stdout = Stdio::inherit();
        let stderr = Stdio::inherit();

        match self {
            Hook::File { file } => {
                let file = source.join(file);
                run_cmd(target, file.as_os_str(), &vec![], stdin, stdout, stderr)
                    .with_context(|| format!("Failed to run hook at file '{}'", file.display()))
            }
            Hook::Command { cmd } => {
                let command = shell
                    .iter()
                    .map(|s| OsString::from(s.replace(CMD_PATTERN, cmd)))
                    .collect::<Vec<_>>();

                let (command, args) = command
                    .split_first()
                    .expect("Expected at least one shell command");

                run_cmd(target, command, args, stdin, stdout, stderr)
                    .with_context(|| format!("Failed to run hook: '{}'", cmd))
            }
        }
    }
}

fn run_cmd(
    dir: &Path,
    cmd: &OsStr,
    args: &[OsString],
    stdin: Stdio,
    stdout: Stdio,
    stderr: Stdio,
) -> anyhow::Result<ExitStatus> {
    Command::new(cmd)
        .args(args)
        .current_dir(dir)
        .stdin(stdin)
        .stdout(stdout)
        .stderr(stderr)
        .status()
        .map_err(anyhow::Error::new)
        .and_then(|status| match status.success() {
            true => Ok(status),
            false => Err(anyhow!("Process exited with status: {}", status)),
        })
}

impl FromStr for Hook {
    type Err = Void;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self::Command(s.to_owned()))
    }
}

pub enum Void {}

impl Display for Void {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unreachable!()
    }
}
