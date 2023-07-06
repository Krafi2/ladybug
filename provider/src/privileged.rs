use core::panic;
use std::{
    ffi::OsString,
    fs::File,
    io::{Read, Write},
    os::{
        fd::{AsRawFd, FromRawFd, OwnedFd, RawFd},
        unix::net::{UnixListener, UnixStream},
    },
    path::PathBuf,
    process::{Child, Command, Stdio},
    thread,
    time::Duration,
};

use sendfd::RecvWithFd;
use serde::{Deserialize, Serialize};

use self::other_side::ProcId;

type Result<T> = std::result::Result<T, Error>;

#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum Error {
    #[error("Failed to initialize privileged server")]
    Init(#[source] std::io::Error),
    #[error("Failed to connect to privileged server")]
    Connect(
        #[from]
        #[source]
        std::io::Error,
    ),
    #[error(transparent)]
    Other(std::io::Error),
}

impl Clone for Error {
    fn clone(&self) -> Self {
        match self {
            // Clone by discarding payload
            Error::Init(err) => Self::Init(err.kind().into()),
            Error::Connect(err) => Self::Connect(err.kind().into()),
            Error::Other(err) => Self::Connect(err.kind().into()),
        }
    }
}

/// Messages sent to the privileged process
#[derive(Debug, Serialize, Deserialize)]
enum Message {
    /// Spawn a subprocess
    Spawn {
        cmd: OsString,
        args: Vec<OsString>,
        env: Vec<(OsString, OsString)>,
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
#[derive(Debug, Serialize, Deserialize)]
enum Reply {
    OkSpawn {
        stdin: bool,
        stdout: bool,
        stderr: bool,
        id: ProcId,
    },
    ExitStatus(ExitStatus),
    Killed,
    Err(IoError),
}

impl Reply {
    fn into_result(self) -> Result<Self> {
        match self {
            Reply::Err(err) => Err(Error::Connect(err.0)),
            ok => Ok(ok),
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(from = "ErrorKindDef", into = "ErrorKindDef")]
struct IoError(std::io::Error);

// Implement clone by discarding payload
impl Clone for IoError {
    fn clone(&self) -> Self {
        let err = self.0.kind().into();
        Self(err)
    }
}

impl From<ErrorKindDef> for IoError {
    fn from(value: ErrorKindDef) -> Self {
        let kind = match value {
            ErrorKindDef::NotFound => std::io::ErrorKind::NotFound,
            ErrorKindDef::PermissionDenied => std::io::ErrorKind::PermissionDenied,
            ErrorKindDef::ConnectionRefused => std::io::ErrorKind::ConnectionRefused,
            ErrorKindDef::ConnectionReset => std::io::ErrorKind::ConnectionReset,
            ErrorKindDef::ConnectionAborted => std::io::ErrorKind::ConnectionAborted,
            ErrorKindDef::NotConnected => std::io::ErrorKind::NotConnected,
            ErrorKindDef::AddrInUse => std::io::ErrorKind::AddrInUse,
            ErrorKindDef::AddrNotAvailable => std::io::ErrorKind::AddrNotAvailable,
            ErrorKindDef::BrokenPipe => std::io::ErrorKind::BrokenPipe,
            ErrorKindDef::AlreadyExists => std::io::ErrorKind::AlreadyExists,
            ErrorKindDef::WouldBlock => std::io::ErrorKind::WouldBlock,
            ErrorKindDef::InvalidInput => std::io::ErrorKind::InvalidInput,
            ErrorKindDef::InvalidData => std::io::ErrorKind::InvalidData,
            ErrorKindDef::TimedOut => std::io::ErrorKind::TimedOut,
            ErrorKindDef::WriteZero => std::io::ErrorKind::WriteZero,
            ErrorKindDef::Interrupted => std::io::ErrorKind::Interrupted,
            ErrorKindDef::Unsupported => std::io::ErrorKind::Unsupported,
            ErrorKindDef::UnexpectedEof => std::io::ErrorKind::UnexpectedEof,
            ErrorKindDef::OutOfMemory => std::io::ErrorKind::OutOfMemory,
            _ => std::io::ErrorKind::Other,
        };
        Self(kind.into())
    }
}

impl Into<ErrorKindDef> for IoError {
    fn into(self) -> ErrorKindDef {
        match self.0.kind() {
            std::io::ErrorKind::NotFound => ErrorKindDef::NotFound,
            std::io::ErrorKind::PermissionDenied => ErrorKindDef::PermissionDenied,
            std::io::ErrorKind::ConnectionRefused => ErrorKindDef::ConnectionRefused,
            std::io::ErrorKind::ConnectionReset => ErrorKindDef::ConnectionReset,
            std::io::ErrorKind::ConnectionAborted => ErrorKindDef::ConnectionAborted,
            std::io::ErrorKind::NotConnected => ErrorKindDef::NotConnected,
            std::io::ErrorKind::AddrInUse => ErrorKindDef::AddrInUse,
            std::io::ErrorKind::AddrNotAvailable => ErrorKindDef::AddrNotAvailable,
            std::io::ErrorKind::BrokenPipe => ErrorKindDef::BrokenPipe,
            std::io::ErrorKind::AlreadyExists => ErrorKindDef::AlreadyExists,
            std::io::ErrorKind::WouldBlock => ErrorKindDef::WouldBlock,
            std::io::ErrorKind::InvalidInput => ErrorKindDef::InvalidInput,
            std::io::ErrorKind::InvalidData => ErrorKindDef::InvalidData,
            std::io::ErrorKind::TimedOut => ErrorKindDef::TimedOut,
            std::io::ErrorKind::WriteZero => ErrorKindDef::WriteZero,
            std::io::ErrorKind::Interrupted => ErrorKindDef::Interrupted,
            std::io::ErrorKind::Unsupported => ErrorKindDef::Unsupported,
            std::io::ErrorKind::UnexpectedEof => ErrorKindDef::UnexpectedEof,
            std::io::ErrorKind::OutOfMemory => ErrorKindDef::OutOfMemory,
            _ => ErrorKindDef::Other,
        }
    }
}

// ErrorKind definition to enable serialization
#[derive(Debug, Serialize, Deserialize)]
#[non_exhaustive]
pub enum ErrorKindDef {
    NotFound,
    PermissionDenied,
    ConnectionRefused,
    ConnectionReset,
    ConnectionAborted,
    NotConnected,
    AddrInUse,
    AddrNotAvailable,
    BrokenPipe,
    AlreadyExists,
    WouldBlock,
    InvalidInput,
    InvalidData,
    TimedOut,
    WriteZero,
    Interrupted,
    Unsupported,
    UnexpectedEof,
    OutOfMemory,
    Other,
}

/// Take over execution if the current proccess is the privileged one
pub fn detect_privileged() {
    // The privileged process wil set `LADYBUG_PRIVILEGED`
    if let Ok(_) = std::env::var("LADYBUG_PRIVILEGED") {
        other_side::main();
        std::process::exit(0);
    }
}

mod other_side {
    use std::{
        collections::HashMap,
        ops::RangeFrom,
        os::{fd::AsRawFd, unix::net::UnixStream},
        process::{Child, Command},
    };

    use sendfd::SendWithFd;
    use serde::{Deserialize, Serialize};

    use super::{set_sock_timeout, IoError, Message, Reply};

    /// Entrypoint for the privileged process
    pub fn main() {
        let mut server = Server::new();
        loop {
            let _ = match server.recv_message() {
                Ok(msg) => server.handle_msg(msg),
                Err(err) => match *err {
                    bincode::ErrorKind::Io(err) => Err(err),
                    // This would be a bug so we don't bother returning the error in a nice way
                    other => panic!("Malformed message: {other}"),
                },
            }
            .map_err(|err| server.emit_err(err));
        }
    }

    #[derive(Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
    pub struct ProcId(u32);

    struct Server {
        id_gen: RangeFrom<u32>,
        procs: HashMap<ProcId, Child>,
        stream: UnixStream,
    }

    impl Server {
        fn new() -> Self {
            Self {
                id_gen: 0..,
                procs: HashMap::default(),
                stream: UnixStream::connect(super::SOCKET)
                    .and_then(set_sock_timeout)
                    .expect("Failed to connect to socket"),
            }
        }

        fn handle_msg(&mut self, msg: Message) -> std::io::Result<()> {
            match msg {
                Message::Spawn {
                    cmd,
                    args,
                    env,
                    dir,
                } => {
                    let mut cmd = Command::new(cmd);
                    cmd.args(args).envs(env);
                    if let Some(dir) = dir {
                        cmd.current_dir(dir);
                    }
                    let mut proc = cmd.spawn()?;
                    let id = ProcId(self.id_gen.next().unwrap());

                    let mut fds = Vec::new();
                    let stdin = proc.stdin.take().map(|stdio| {
                        fds.push(stdio.as_raw_fd());
                        stdio
                    });
                    let stdout = proc.stdout.take().map(|stdio| {
                        fds.push(stdio.as_raw_fd());
                        stdio
                    });
                    let stderr = proc.stderr.take().map(|stdio| {
                        fds.push(stdio.as_raw_fd());
                        stdio
                    });

                    // Send over ths fds
                    self.stream.send_with_fd(&[], &fds)?;
                    // Send data
                    self.send(&Reply::OkSpawn {
                        stdin: stdin.is_some(),
                        stdout: stdout.is_some(),
                        stderr: stderr.is_some(),
                        id: ProcId(id.0),
                    });
                    self.procs.insert(id, proc);
                    Ok(())
                    // Our side of the file descriptors will now be closed
                }
                Message::Kill { id } => {
                    let mut proc = self.procs.remove(&id).expect("This process doesn't exist");
                    proc.kill()?;
                    self.send(&Reply::Killed);
                    Ok(())
                }
                Message::Wait { id } => {
                    let mut proc = self.procs.remove(&id).expect("This process doesn't exist");
                    let status = proc.wait()?;
                    self.send(&Reply::ExitStatus(status.into()));
                    Ok(())
                }
            }
        }

        fn emit_err(&self, err: std::io::Error) {
            self.send(&Reply::Err(IoError(err)))
        }

        fn send(&self, value: &Reply) {
            let err = match bincode::serialize_into(&self.stream, value).map_err(|e| *e) {
                Err(bincode::ErrorKind::Io(err)) => {
                    match bincode::serialize_into(&self.stream, &Reply::Err(IoError(err))) {
                        Ok(_) => return,
                        Err(err) => *err,
                    }
                }
                Err(err) => err,
                Ok(_) => return,
            };
            panic!("Failed to send message: {err}");
        }

        fn recv_message(&self) -> bincode::Result<Message> {
            bincode::deserialize_from::<_, Message>(&self.stream)
        }
    }
}

/// Handle for the proccess handling privileged requests
pub struct Server {
    inner: Option<Result<ServerInner>>,
}

impl Server {
    pub fn new() -> Self {
        Self { inner: None }
    }

    pub fn super_ctx<'a>(&'a mut self) -> SuperCtx<'a> {
        SuperCtx { server: self }
    }

    fn inner(&mut self) -> Result<&mut ServerInner> {
        self.inner
            .get_or_insert_with(|| ServerInner::new().map_err(Error::Init))
            .as_mut()
            .map_err(|err| err.clone())
    }
}

struct ServerInner {
    proc: Child,
    stream: Transceiver,
}

struct Transceiver {
    stream: UnixStream,
    files: Vec<RawFd>,
}

impl Transceiver {
    fn new(stream: UnixStream) -> Self {
        Self {
            stream,
            files: Vec::new(),
        }
    }

    fn next_fd(&mut self) -> Option<RawFd> {
        self.files.drain(..1).next()
    }
}

impl Read for Transceiver {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let mut fds = [RawFd::default(); 4];
        let (bytes, nfd) = self.stream.recv_with_fd(buf, &mut fds)?;
        self.files.extend_from_slice(&fds[..nfd]);
        Ok(bytes)
    }
}

impl Write for Transceiver {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.stream.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.stream.flush()
    }
}

const SOCKET: &str = "/tmp/ladybug_server";

impl ServerInner {
    fn new() -> std::io::Result<Self> {
        let mut command = std::process::Command::new("sudo");
        let mut args = std::env::args();

        let listener = UnixListener::bind(SOCKET).and_then(set_sock_timeout)?;

        let mut proc = command
        // Ask sudo to preserve some variables that we need
        .arg("--preserve-env=XDG_CACHE_HOME,XDG_CONFIG_HOME,XDG_DATA_DIRS,XDG_CONFIG_DIRS,PATH,USER,HOME")
        .arg(args.next().unwrap()) // Get the path to the executable currently running
        .env("LADYBUG_PRIVILEGED", "true")
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::piped()).spawn()?;

        // The send timeout should apply here as well
        let (stream, _addr) = listener.accept().map_err(|err| {
            // Check if the error was caused by the process crashing
            check_health(&mut proc).err().unwrap_or(err)
        })?;

        Ok(Self {
            proc,
            stream: Transceiver::new(stream),
        })
    }

    fn send(&mut self, msg: Message) -> Result<()> {
        bincode::serialize_into(&mut self.stream, &msg).map_err(|err| match *err {
            // Check that the process is running if we get an Io error
            bincode::ErrorKind::Io(err) => match check_health(&mut self.proc) {
                Ok(_) => Error::Connect(err),
                Err(health) => Error::Connect(health),
            },
            // There shouldn't be any parsing errors, I trust you, bincode
            other => panic!("{other}"),
        })
    }

    fn receive(&mut self) -> Result<Reply> {
        bincode::deserialize_from(&mut self.stream).map_err(|err| match *err {
            // Check that the process is running if we get an Io error
            bincode::ErrorKind::Io(err) => match check_health(&mut self.proc) {
                Ok(_) => Error::Connect(err),
                Err(health) => Error::Connect(health),
            },
            // There shouldn't be any parsing errors, I trust you, bincode
            other => panic!("{other}"),
        })
    }
}

fn check_health(proc: &mut Child) -> std::io::Result<()> {
    if let Ok(_status) = proc.try_wait() {
        let mut stderr = String::new();
        proc.stderr.as_mut().unwrap().read_to_string(&mut stderr)?;
        Err(std::io::Error::new(
            std::io::ErrorKind::NotConnected,
            color_eyre::eyre::eyre!(stderr).wrap_err("Privileged server exited unexpectedly"),
        ))
    } else {
        Ok(())
    }
}

fn set_sock_timeout<T>(t: T) -> std::io::Result<T>
where
    T: From<OwnedFd>,
    OwnedFd: From<T>,
{
    let stream = UnixStream::from(OwnedFd::from(t));
    let timeout = Duration::from_secs(1);
    stream.set_read_timeout(Some(timeout))?;
    stream.set_write_timeout(Some(timeout))?;
    let t = T::from(<OwnedFd as From<UnixStream>>::from(stream));
    Ok(t)
}

pub struct SuperCtx<'a> {
    server: &'a mut Server,
}

impl<'a> SuperCtx<'a> {
    /// Spawn a privileged process
    pub fn spawn(&mut self, cmd: &mut Command) -> Result<SuperProc> {
        let server = self.server.inner()?;
        server.send(Message::Spawn {
            cmd: cmd.get_program().into(),
            args: cmd.get_args().map(ToOwned::to_owned).collect(),
            env: cmd
                .get_envs()
                .map(|(key, val)| {
                    (
                        key.to_owned(),
                        val.map(ToOwned::to_owned)
                            .unwrap_or_else(|| OsString::default()),
                    )
                })
                .collect(),
            dir: cmd.get_current_dir().map(ToOwned::to_owned),
        })?;
        match server.receive().and_then(Reply::into_result)? {
            Reply::OkSpawn {
                stdin,
                stdout,
                stderr,
                id,
            } => unsafe {
                let stdin =
                    stdin.then(|| RemoteStdin(File::from_raw_fd(server.stream.next_fd().unwrap())));
                let stdout = stdout
                    .then(|| RemoteStdout(File::from_raw_fd(server.stream.next_fd().unwrap())));
                let stderr = stderr
                    .then(|| RemoteStderr(File::from_raw_fd(server.stream.next_fd().unwrap())));

                Ok(SuperProc {
                    id,
                    stdin,
                    stdout,
                    stderr,
                })
            },
            _ => panic!("Unexpected message"),
        }
    }

    /// Wait for the process to exit
    pub fn wait<T: Into<Process>>(&mut self, proc: T) -> Result<ExitStatus> {
        let proc: Process = proc.into();
        match proc {
            Process::User(mut proc) => proc.wait().map(Into::into).map_err(Error::Other),
            Process::Privileged(proc) => {
                let inner = self.server.inner()?;

                // Close stdin to prevent deadlocks
                if let Some(stdin) = proc.stdin {
                    std::mem::drop(stdin);
                }
                inner.send(Message::Wait { id: proc.id })?;

                match inner.receive().and_then(Reply::into_result)? {
                    Reply::ExitStatus(status) => Ok(status),
                    _ => panic!("Unexpected message"),
                }
            }
        }
    }

    pub fn wait_with_output<T: Into<Process>>(&mut self, proc: T) -> Result<Output> {
        let proc: Process = proc.into();
        match proc {
            Process::User(proc) => proc
                .wait_with_output()
                .map(Into::into)
                .map_err(Error::Other),
            Process::Privileged(proc) => {
                let server = self.server.inner()?;

                // Close stdin to prevent deadlocks
                if let Some(stdin) = proc.stdin {
                    std::mem::drop(stdin);
                }
                server.send(Message::Wait { id: proc.id })?;

                // Collect stderr on a separate thread to avoid blocking the process
                let stderr = proc.stderr.map(|mut stderr| {
                    thread::spawn(move || {
                        let mut buf = Vec::new();
                        stderr.read_to_end(&mut buf).map(|_| buf)
                    })
                });

                let stdout = proc
                    .stdout
                    .map(|mut stdout| {
                        let mut buf = Vec::new();
                        stdout.read_to_end(&mut buf).map(|_| buf)
                    })
                    .transpose();

                let stderr = stderr
                    .map(|handle| match handle.join() {
                        Ok(ok) => ok,
                        Err(err) => std::panic::resume_unwind(err),
                    })
                    .transpose();

                match server.receive().and_then(Reply::into_result)? {
                    Reply::ExitStatus(status) => Ok(Output {
                        status,
                        stdout: stdout?.unwrap_or_default(),
                        stderr: stderr?.unwrap_or_default(),
                    }),
                    _ => panic!("Unexpected message"),
                }
            }
        }
    }

    /// Kill the process
    pub fn kill<T: Into<Process>>(&mut self, proc: T) -> Result<()> {
        let proc: Process = proc.into();
        match proc {
            Process::User(mut proc) => proc.kill().map_err(Error::Other),
            Process::Privileged(proc) => {
                let inner = self.server.inner()?;
                inner.send(Message::Kill { id: proc.id })?;
                match inner.receive().and_then(Reply::into_result)? {
                    Reply::Killed => Ok(()),
                    _ => panic!("Unexpected message"),
                }
            }
        }
    }
}

pub enum Stdin {
    Child(std::process::ChildStdin),
    Remote(RemoteStdin),
}

impl Write for Stdin {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            Stdin::Child(writer) => writer.write(buf),
            Stdin::Remote(writer) => writer.write(buf),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            Stdin::Child(writer) => writer.flush(),
            Stdin::Remote(writer) => writer.flush(),
        }
    }

    fn write_vectored(&mut self, bufs: &[std::io::IoSlice<'_>]) -> std::io::Result<usize> {
        match self {
            Stdin::Child(writer) => writer.write_vectored(bufs),
            Stdin::Remote(writer) => writer.write_vectored(bufs),
        }
    }

    fn write_all(&mut self, buf: &[u8]) -> std::io::Result<()> {
        match self {
            Stdin::Child(writer) => writer.write_all(buf),
            Stdin::Remote(writer) => writer.write_all(buf),
        }
    }
}

impl From<std::process::ChildStdin> for Stdin {
    fn from(value: std::process::ChildStdin) -> Self {
        Self::Child(value)
    }
}

impl From<RemoteStdin> for Stdin {
    fn from(value: RemoteStdin) -> Self {
        Self::Remote(value)
    }
}

pub enum Stdout {
    Child(std::process::ChildStdout),
    Remote(RemoteStdout),
}

impl From<std::process::ChildStdout> for Stdout {
    fn from(value: std::process::ChildStdout) -> Self {
        Self::Child(value)
    }
}

impl From<RemoteStdout> for Stdout {
    fn from(value: RemoteStdout) -> Self {
        Self::Remote(value)
    }
}

impl AsRawFd for Stdout {
    fn as_raw_fd(&self) -> std::os::fd::RawFd {
        match self {
            Self::Child(stdout) => stdout.as_raw_fd(),
            Self::Remote(stdout) => stdout.as_raw_fd(),
        }
    }
}

impl Read for Stdout {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        match self {
            Stdout::Child(reader) => reader.read(buf),
            Stdout::Remote(reader) => reader.read(buf),
        }
    }

    fn read_vectored(&mut self, bufs: &mut [std::io::IoSliceMut<'_>]) -> std::io::Result<usize> {
        match self {
            Stdout::Child(reader) => reader.read_vectored(bufs),
            Stdout::Remote(reader) => reader.read_vectored(bufs),
        }
    }

    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> std::io::Result<usize> {
        match self {
            Stdout::Child(reader) => reader.read_to_end(buf),
            Stdout::Remote(reader) => reader.read_to_end(buf),
        }
    }

    fn read_to_string(&mut self, buf: &mut String) -> std::io::Result<usize> {
        match self {
            Stdout::Child(reader) => reader.read_to_string(buf),
            Stdout::Remote(reader) => reader.read_to_string(buf),
        }
    }

    fn read_exact(&mut self, buf: &mut [u8]) -> std::io::Result<()> {
        match self {
            Stdout::Child(reader) => reader.read_exact(buf),
            Stdout::Remote(reader) => reader.read_exact(buf),
        }
    }
}

pub enum Stderr {
    Child(std::process::ChildStderr),
    Remote(RemoteStderr),
}

impl Read for Stderr {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        match self {
            Stderr::Child(reader) => reader.read(buf),
            Stderr::Remote(reader) => reader.read(buf),
        }
    }

    fn read_vectored(&mut self, bufs: &mut [std::io::IoSliceMut<'_>]) -> std::io::Result<usize> {
        match self {
            Stderr::Child(reader) => reader.read_vectored(bufs),
            Stderr::Remote(reader) => reader.read_vectored(bufs),
        }
    }

    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> std::io::Result<usize> {
        match self {
            Stderr::Child(reader) => reader.read_to_end(buf),
            Stderr::Remote(reader) => reader.read_to_end(buf),
        }
    }

    fn read_to_string(&mut self, buf: &mut String) -> std::io::Result<usize> {
        match self {
            Stderr::Child(reader) => reader.read_to_string(buf),
            Stderr::Remote(reader) => reader.read_to_string(buf),
        }
    }

    fn read_exact(&mut self, buf: &mut [u8]) -> std::io::Result<()> {
        match self {
            Stderr::Child(reader) => reader.read_exact(buf),
            Stderr::Remote(reader) => reader.read_exact(buf),
        }
    }
}

impl From<std::process::ChildStderr> for Stderr {
    fn from(value: std::process::ChildStderr) -> Self {
        Self::Child(value)
    }
}

impl From<RemoteStderr> for Stderr {
    fn from(value: RemoteStderr) -> Self {
        Self::Remote(value)
    }
}

pub enum Process {
    User(std::process::Child),
    Privileged(SuperProc),
}

impl Process {
    pub fn take_stdin(&mut self) -> Option<Stdin> {
        match self {
            Process::User(child) => child.stdin.take().map(Stdin::Child),
            Process::Privileged(proc) => proc.stdin.take().map(Stdin::Remote),
        }
    }

    pub fn put_stdin(&mut self, stdin: Stdin) {
        match (self, stdin) {
            (Process::Privileged(proc), Stdin::Remote(stdin)) => {
                proc.stdin = Some(stdin);
            }
            (Process::User(proc), Stdin::Child(stdin)) => {
                proc.stdin = Some(stdin);
            }
            _ => panic!("Invalid stdin handle"),
        }
    }

    pub fn take_stdout(&mut self) -> Option<Stdout> {
        match self {
            Process::User(child) => child.stdout.take().map(Stdout::Child),
            Process::Privileged(proc) => proc.stdout.take().map(Stdout::Remote),
        }
    }

    pub fn put_stdout(&mut self, stdout: Stdout) {
        match (self, stdout) {
            (Process::Privileged(proc), Stdout::Remote(stdout)) => {
                proc.stdout = Some(stdout);
            }
            (Process::User(proc), Stdout::Child(stdout)) => {
                proc.stdout = Some(stdout);
            }
            _ => panic!("Invalid stdout handle"),
        }
    }

    pub fn take_stderr(&mut self) -> Option<Stderr> {
        match self {
            Process::User(child) => child.stderr.take().map(Stderr::Child),
            Process::Privileged(proc) => proc.stderr.take().map(Stderr::Remote),
        }
    }

    pub fn put_stderr(&mut self, stderr: Stderr) {
        match (self, stderr) {
            (Process::Privileged(proc), Stderr::Remote(stderr)) => {
                proc.stderr = Some(stderr);
            }
            (Process::User(proc), Stderr::Child(stderr)) => {
                proc.stderr = Some(stderr);
            }
            _ => panic!("Invalid stderr handle"),
        }
    }
}

impl From<std::process::Child> for Process {
    fn from(value: std::process::Child) -> Self {
        Self::User(value)
    }
}

impl From<SuperProc> for Process {
    fn from(value: SuperProc) -> Self {
        Self::Privileged(value)
    }
}

pub struct RemoteStdin(File);

impl Write for RemoteStdin {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.0.flush()
    }

    fn write_vectored(&mut self, bufs: &[std::io::IoSlice<'_>]) -> std::io::Result<usize> {
        self.0.write_vectored(bufs)
    }

    fn write_all(&mut self, buf: &[u8]) -> std::io::Result<()> {
        self.0.write_all(buf)
    }
}

pub struct RemoteStdout(File);

impl AsRawFd for RemoteStdout {
    fn as_raw_fd(&self) -> std::os::fd::RawFd {
        self.0.as_raw_fd()
    }
}

impl Read for RemoteStdout {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.0.read(buf)
    }

    fn read_vectored(&mut self, bufs: &mut [std::io::IoSliceMut<'_>]) -> std::io::Result<usize> {
        self.0.read_vectored(bufs)
    }

    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> std::io::Result<usize> {
        self.0.read_to_end(buf)
    }

    fn read_to_string(&mut self, buf: &mut String) -> std::io::Result<usize> {
        self.0.read_to_string(buf)
    }
}

pub struct RemoteStderr(File);

impl Read for RemoteStderr {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.0.read(buf)
    }

    fn read_vectored(&mut self, bufs: &mut [std::io::IoSliceMut<'_>]) -> std::io::Result<usize> {
        self.0.read_vectored(bufs)
    }

    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> std::io::Result<usize> {
        self.0.read_to_end(buf)
    }

    fn read_to_string(&mut self, buf: &mut String) -> std::io::Result<usize> {
        self.0.read_to_string(buf)
    }
}

#[non_exhaustive]
pub struct SuperProc {
    pub id: ProcId,
    pub stdin: Option<RemoteStdin>,
    pub stdout: Option<RemoteStdout>,
    pub stderr: Option<RemoteStderr>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Output {
    pub status: ExitStatus,
    pub stdout: Vec<u8>,
    pub stderr: Vec<u8>,
}

impl Output {
    /// Convert the output to an error if if the process failed
    pub fn into_result(self) -> std::result::Result<Self, common::command::Error> {
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

impl From<std::process::Output> for Output {
    fn from(out: std::process::Output) -> Self {
        Output {
            status: ExitStatus {
                code: out.status.code(),
            },
            stdout: out.stdout,
            stderr: out.stderr,
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ExitStatus {
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

impl From<std::process::ExitStatus> for ExitStatus {
    fn from(value: std::process::ExitStatus) -> Self {
        Self { code: value.code() }
    }
}

// #[cfg(any(target_os = "redox", unix))]
// mod imp {
//     use std::{
//         fs::File,
//         os::fd::{AsRawFd, FromRawFd},
//         process::{ChildStderr, ChildStdin, ChildStdout},
//     };

//     use serde::{Deserialize, Serialize};

//     #[derive(Debug, Serialize, Deserialize)]
//     pub struct Inner(File);

//     impl From<Inner> for std::fs::File {
//         fn from(handle: Inner) -> Self {
//             // This should be a valid open handle
//             unsafe { std::os::fd::FromRawFd::from_raw_fd(handle.0) }
//         }
//     }

//     impl From<&ChildStdin> for Inner {
//         fn from(value: &ChildStdin) -> Self {
//             unsafe { Self(File::from_raw_fd(value.as_raw_fd())) }
//         }
//     }

//     impl From<&ChildStdout> for Inner {
//         fn from(value: &ChildStdout) -> Self {
//             Self(value.as_raw_fd())
//         }
//     }

//     impl From<&ChildStderr> for Inner {
//         fn from(value: &ChildStderr) -> Self {
//             Self(value.as_raw_fd())
//         }
//     }
// }
