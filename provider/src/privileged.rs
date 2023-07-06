use color_eyre::eyre::WrapErr;
use std::{fs::File, path::PathBuf, process::Command};

use crate::ExecutionCtx;

struct ProcId(u32);

/// Messages sent to the privileged process
enum Message {
    /// Spawn a subprocess
    Spawn {
        cmd: String,
        args: Option<Vec<String>>,
        env: Option<Vec<(String, String)>>,
        dir: Option<PathBuf>,
    },
    Kill {
        id: ProcId,
    },
    Wait {
        id: ProcId,
    },
}

/// Replies sent by the privileged process
enum Reply {
    OkSpawn {
        stdin: Handle,
        stdout: Handle,
        stderr: Handle,
        id: ProcId,
    },
    ExitStatus {
        code: Option<u32>,
    },
    Err(std::io::Error),
}

/// Entrypoint for the privileged process
fn main() {
    todo!()
}

pub enum SuperError {}

/// Handle for the proccess handling privileged requests
pub(crate) struct Server {}

impl Server {
    pub fn new() -> Self {
        todo!()
    }

    fn launch() -> Result<Self, SuperError> {
        let mut command = std::process::Command::new("sudo");
        let mut args = std::env::args();

        // Ask sudo to preserve some variables that we need
        command.arg("--preserve-env=XDG_CACHE_HOME,XDG_CONFIG_HOME,XDG_DATA_DIRS,XDG_CONFIG_DIRS,PATH,USER,HOME");
        command.arg(args.next().unwrap()); // Get the path to the executable currently running
        command.env("LADYBUG_PRIVILEGED", "true");

        let proc = command.spawn();

        // There was an error if the previous statement exited
        proc.wrap_err("Failed to elevate to root privileges");
        todo!()
    }

    pub fn super_ctx(&mut self, ctx: ExecutionCtx) -> Result<SuperCtx, SuperError> {
        todo!()
    }
}

struct Handle(imp::Inner);

pub(crate) struct SuperCtx<'a> {
    ctx: ExecutionCtx<'a>,
    server: &'a mut Server,
}

impl<'a> SuperCtx<'a> {
    /// Spawn a privileged process
    pub fn spawn(&mut self, cmd: Command) -> std::io::Result<SuperProc> {
        todo!()
    }

    /// Wait for the process to exit
    pub fn wait(&mut self, proc: SuperProc) -> std::io::Result<ExitStatus> {
        // Close stdin please
        todo!()
    }

    pub fn wait_with_output(&mut self, proc: SuperProc) -> std::io::Result<Output> {
        // Close stdin please
        todo!()
    }

    /// Kill the process
    pub fn kill(&mut self, proc: SuperProc) -> std::io::Result<()> {
        todo!()
    }

    /// Set the progress message
    pub fn set_message(&mut self, msg: String) {
        self.ctx.set_message(msg)
    }
}

#[non_exhaustive]
pub(super) struct SuperProc {
    pub id: ProcId,
    pub stdin: Option<File>,
    pub stdout: Option<File>,
    pub stderr: Option<File>,
}

pub(super) struct Output {
    pub status: ExitStatus,
    pub stdout: Vec<u8>,
    pub stderr: Vec<u8>,
}

impl Output {
    /// Convert the output to an error if if the process failed
    pub fn into_result(self) -> Result<Self, common::command::Error> {
        if self.status.success() {
            Ok(self)
        } else {
            Err(common::command::Error::Failed {
                stderr: self.stderr,
                code: self.status.code(),
            })
        }
    }
}

struct ExitStatus {
    code: Option<i32>,
}

impl ExitStatus {
    pub fn code(&self) -> Option<i32> {
        self.code
    }

    pub fn success(&self) -> bool {
        self.code.map(|c| c == 0).unwrap_or(false)
    }
}

#[cfg(any(target_os = "redox", unix))]
mod imp {
    pub type Inner = std::os::fd::RawFd;

    impl From<super::Handle> for std::fs::File {
        fn from(handle: super::Handle) -> Self {
            // This should be a valid open handle
            unsafe { std::os::fd::FromRawFd::from_raw_fd(handle.0) }
        }
    }
}

#[cfg(windows)]
mod imp {
    pub type Inner = std::os::windows::io::RawHandle;

    impl From<super::Handle> for std::fs::File {
        fn from(handle: super::Handle) -> Self {
            // This should be a valid open handle
            unsafe { std::os::windows::io::FromRawHandle(handle.0) }
        }
    }
}

/// Take over execution if the current proccess is the privileged one
pub fn detect_privileged() {
    // The privileged process wil set `LADYBUG_PRIVILEGED`
    if let Ok(_) = std::env::var("LADYBUG_PRIVILEGED") {
        main();
        std::process::exit(0);
    }
}
