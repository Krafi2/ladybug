use core::panic;
use std::{
    collections::HashMap,
    ffi::{OsStr, OsString},
    fs::File,
    io::{Read, Write},
    os::{
        fd::{AsRawFd, FromRawFd, RawFd},
        unix::net::{UnixListener, UnixStream},
    },
    path::{Path, PathBuf},
    process::{Child, Command as StdCommand, Stdio as StdStdio},
    sync::Arc,
    thread,
};

use bincode::Options;
use color_eyre::eyre::{eyre, Context};
use sendfd::RecvWithFd;
use serde::{Deserialize, Serialize};

use self::other_side::ProcId;

type Result<T> = std::result::Result<T, Error>;

#[derive(Clone)]
pub struct Error(Arc<ErrorKind>);

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.0.source()
    }
}

impl From<ErrorKind> for Error {
    fn from(value: ErrorKind) -> Self {
        Self(Arc::new(value))
    }
}

impl From<std::io::Error> for Error {
    fn from(value: std::io::Error) -> Self {
        Self(Arc::new(ErrorKind::Connect(value.into())))
    }
}

#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum ErrorKind {
    #[error("Failed to initialize privileged server")]
    Init(#[source] color_eyre::Report),
    #[error("Failed to connect to privileged server")]
    Connect(#[source] color_eyre::Report),
    #[error("Privileged server crashed")]
    Crash(#[source] color_eyre::Report),
    #[error(transparent)]
    Other(color_eyre::Report),
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
        stdin: StdioVal,
        stdout: StdioVal,
        stderr: StdioVal,
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
            Reply::Err(err) => Err(ErrorKind::Connect(err.0.into()).into()),
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

impl From<IoError> for ErrorKindDef {
    fn from(val: IoError) -> Self {
        match val.0.kind() {
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

/// Take over execution if the current process is the privileged one
pub fn detect_privileged() {
    // The privileged process wil set `LADYBUG_PRIVILEGED`
    if std::env::var_os(ENV_VAR).is_some() {
        other_side::main();
        std::process::exit(0);
    }
}

mod other_side {
    use std::{
        collections::HashMap,
        ops::RangeFrom,
        os::{
            fd::{AsRawFd, RawFd},
            unix::net::UnixStream,
        },
        process::{Child, Command},
    };

    use bincode::Options;
    use color_eyre::eyre::{eyre, WrapErr};
    use sendfd::SendWithFd;
    use serde::{Deserialize, Serialize};

    use super::{IoError, Message, Reply};

    /// Entrypoint for the privileged process
    pub fn main() {
        match run() {
            Ok(_) => (),
            Err(err) => {
                eprintln!("Server error:{err:?}");
                std::process::exit(1);
            }
        }
    }

    pub fn run() -> color_eyre::Result<()> {
        println!("Server");
        let mut server = Server::new();
        loop {
            match server.recv_message() {
                Ok(msg) => server.handle_msg(msg),
                Err(err) => match *err {
                    bincode::ErrorKind::Io(err) => server.emit_err(err),
                    other => Err(eyre!(other)).wrap_err("Malformed message"),
                },
            }?
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
                    // .and_then(set_sock_timeout)
                    .expect("Failed to connect to socket"),
            }
        }

        fn handle_msg(&mut self, msg: Message) -> color_eyre::Result<()> {
            match msg {
                Message::Spawn {
                    cmd,
                    args,
                    env,
                    dir,
                    stdin,
                    stdout,
                    stderr,
                } => {
                    let mut cmd = Command::new(cmd);

                    cmd.args(args).envs(env);

                    if let Some(dir) = dir {
                        cmd.current_dir(dir);
                    }

                    cmd.stdin(stdin);
                    cmd.stdout(stdout);
                    cmd.stderr(stderr);

                    let mut proc = cmd.spawn().wrap_err("Failed to spawn requested process")?;
                    let id = ProcId(self.id_gen.next().unwrap());

                    let stdin = proc.stdin.is_some();
                    let stdout = proc.stdout.is_some();
                    let stderr = proc.stderr.is_some();

                    let fds = [
                        proc.stdin.as_ref().map(AsRawFd::as_raw_fd),
                        proc.stdout.as_ref().map(AsRawFd::as_raw_fd),
                        proc.stderr.as_ref().map(AsRawFd::as_raw_fd),
                    ]
                    .into_iter()
                    .flat_map(std::convert::identity)
                    .collect::<Vec<_>>();

                    // Send data
                    let reply = Reply::OkSpawn {
                        stdin,
                        stdout,
                        stderr,
                        id: ProcId(id.0),
                    };
                    self.send(&reply, &fds)?;

                    // Close our side of the file descriptors
                    proc.stdin.take();
                    proc.stdout.take();
                    proc.stderr.take();

                    self.procs.insert(id, proc);
                    Ok(())
                }
                Message::Kill { id } => {
                    let mut proc = self.remove_proc(id)?;
                    proc.kill().wrap_err("Failed to kill requested process")?;
                    self.send(&Reply::Killed, &[])?;
                    Ok(())
                }
                Message::Wait { id } => {
                    let mut proc = self.remove_proc(id)?;
                    let status = proc
                        .wait()
                        .wrap_err("Failed to wait on requested process")?;
                    self.send(&Reply::ExitStatus(status.into()), &[])?;
                    Ok(())
                }
            }
        }

        fn remove_proc(&mut self, id: ProcId) -> color_eyre::Result<Child> {
            self.procs
                .remove(&id)
                .ok_or(eyre!("Tried to remove non-existent process"))
        }

        fn emit_err(&mut self, err: std::io::Error) -> color_eyre::Result<()> {
            self.send(&Reply::Err(IoError(err)), &[])
        }

        fn send(&mut self, value: &Reply, fds: &[RawFd]) -> color_eyre::Result<()> {
            bincode::DefaultOptions::new()
                .serialize(value)
                .and_then(|buf| {
                    let mut bytes = 0;
                    loop {
                        let bytecount = self.stream.send_with_fd(&buf[bytes..], &fds)?;
                        bytes += bytecount;

                        if bytes == buf.len() {
                            break Ok(());
                        }
                    }
                })
                .or_else(|err| match *err {
                    bincode::ErrorKind::Io(err) => {
                        bincode::serialize_into(&self.stream, &Reply::Err(IoError(err)))
                    }
                    _ => Err(err),
                })
                .wrap_err("Failed to send message to client")
        }

        fn recv_message(&mut self) -> bincode::Result<Message> {
            bincode::DefaultOptions::new().deserialize_from::<_, Message>(&mut self.stream)
        }
    }
}

/// Handle for the process handling privileged requests
pub struct Client {
    inner: Option<Result<ClientInner>>,
}

impl Client {
    pub fn new() -> Self {
        Self { inner: None }
    }

    pub fn super_ctx(&mut self) -> &mut SuperCtx {
        self
    }

    fn take_inner(&mut self) -> Result<ClientInner> {
        match self
            .inner
            .get_or_insert_with(|| ClientInner::new().map_err(|err| ErrorKind::Init(err).into()))
        {
            Ok(_) => std::mem::replace(
                &mut self.inner,
                Some(Err(
                    ErrorKind::Crash(eyre!("`ClientInner` was moved out")).into()
                )),
            )
            .unwrap(),
            Err(err) => Err(err.clone()),
        }
    }

    fn replace_inner(&mut self, inner: ClientInner) {
        self.inner = Some(Ok(inner));
    }

    fn send(&mut self, msg: &Message) -> Result<()> {
        let inner = self.take_inner()?;
        match inner.send(msg) {
            Ok((inner, res)) => {
                self.replace_inner(inner);
                res
            }
            Err(err) => {
                self.inner = Some(Err(err.clone()));
                Err(err)
            }
        }
    }

    fn receive(&mut self) -> Result<Reply> {
        let inner = self.take_inner()?;
        match inner.receive() {
            Ok((inner, res)) => {
                self.replace_inner(inner);
                res
            }
            Err(err) => {
                self.inner = Some(Err(err.clone()));
                Err(err)
            }
        }
    }

    fn next_fd(&mut self) -> Result<RawFd> {
        let mut inner = self.take_inner()?;
        let res = inner
            .stream
            .next_fd()
            .map_err(|err| ErrorKind::Other(eyre!(err).wrap_err("Failed to receive fd")).into());
        self.replace_inner(inner);
        res
    }

    /// Spawn a privileged process
    pub fn spawn(&mut self, cmd: &mut Command) -> Result<SuperProc> {
        self.send(&Message::Spawn {
            cmd: cmd.program.clone(),
            args: cmd.args.clone(),
            env: cmd
                .env
                .iter()
                .map(|(key, val)| (key.to_owned(), val.to_owned()))
                .collect(),
            dir: cmd.dir.clone(),
            stdin: cmd.stdin.0,
            stdout: cmd.stdout.0,
            stderr: cmd.stderr.0,
        })?;

        match self.receive().and_then(Reply::into_result)? {
            Reply::OkSpawn {
                stdin,
                stdout,
                stderr,
                id,
            } => unsafe {
                let stdin = stdin
                    .then(|| self.next_fd().map(|fd| RemoteStdin(File::from_raw_fd(fd))))
                    .transpose()?;
                let stdout = stdout
                    .then(|| self.next_fd().map(|fd| RemoteStdout(File::from_raw_fd(fd))))
                    .transpose()?;
                let stderr = stderr
                    .then(|| self.next_fd().map(|fd| RemoteStderr(File::from_raw_fd(fd))))
                    .transpose()?;

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
            Process::User(mut proc) => proc
                .wait()
                .map(Into::into)
                .map_err(|err| ErrorKind::Other(eyre!(err)).into()),
            Process::Privileged(proc) => {
                // Close stdin to prevent deadlocks
                if let Some(stdin) = proc.stdin {
                    std::mem::drop(stdin);
                }
                self.send(&Message::Wait { id: proc.id })?;

                match self.receive().and_then(Reply::into_result)? {
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
                .map_err(|err| ErrorKind::Other(eyre!(err)).into()),
            Process::Privileged(proc) => {
                // Close stdin to prevent deadlocks
                if let Some(stdin) = proc.stdin {
                    std::mem::drop(stdin);
                }
                self.send(&Message::Wait { id: proc.id })?;

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

                match self.receive().and_then(Reply::into_result)? {
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
            Process::User(mut proc) => proc
                .kill()
                .map_err(|err| ErrorKind::Other(eyre!(err)).into()),
            Process::Privileged(proc) => {
                self.send(&Message::Kill { id: proc.id })?;
                match self.receive().and_then(Reply::into_result)? {
                    Reply::Killed => Ok(()),
                    _ => panic!("Unexpected message"),
                }
            }
        }
    }
}

impl Drop for Client {
    fn drop(&mut self) {
        if let Some(Ok(inner)) = self.inner.take() {
            kill_child(inner.proc);
        }
    }
}

fn kill_child(mut child: Child) {
    match child.kill() {
        Err(err) if err.kind() == std::io::ErrorKind::InvalidInput => (),
        Err(err) => tracing::error!("Cannot kill privileged server process: {err}"),
        Ok(_) => (),
    }
}

struct ClientInner {
    proc: Child,
    stream: Transceiver,
}

impl ClientInner {
    fn new() -> color_eyre::Result<Self> {
        let mut command = std::process::Command::new("sudo");

        // Make sure that we create a new socket
        match std::fs::remove_file(SOCKET) {
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => (),
            Err(err) => {
                return Err(err).wrap_err(format!("Failed to remove old socket at '{SOCKET}'"))
            }
            Ok(_) => (),
        }

        // TODO: Maybe pass a file descriptor to the server so that the connection can't be hijacked
        let listener = UnixListener::bind(SOCKET).wrap_err("Failed to bind socket")?;

        let proc = command
        // Ask sudo to preserve some variables that we need
        .arg(format!("--preserve-env=XDG_CACHE_HOME,XDG_CONFIG_HOME,XDG_DATA_DIRS,XDG_CONFIG_DIRS,PATH,USER,HOME,RUST_BACKTRACE,{ENV_VAR}"))
        // .arg("--reset-timestamp")
        .arg(std::env::current_exe().wrap_err("Failed to get ladybug executable")?) // Get the path to the executable currently running
        .env(ENV_VAR, "true")
        .stdin(StdStdio::null())
        .stdout(StdStdio::piped())
        .stderr(StdStdio::piped()).spawn().wrap_err("Failed to spawn command")?;

        let (stream, addr) = match listener.accept() {
            Ok(ok) => ok,
            Err(err) => return match check_health(proc)
                .wrap_err("Privileged process exited unexpectedly")
            {
                Err(e) => Err(e).wrap_err(err),
                Ok(proc) => {
                    kill_child(proc);
                    Err(eyre!(err))
                }
            }
            .wrap_err("Failed to connect to peer"),
        };

        tracing::info!("Connected to peer at {addr:?}");

        Ok(Self {
            proc,
            stream: Transceiver::new(stream),
        })
    }

    fn send(mut self, msg: &Message) -> Result<(Self, Result<()>)> {
        match bincode::DefaultOptions::new()
            .serialize_into(&mut self.stream, msg)
            .wrap_err("Error sending messages")
        {
            Ok(_) => Ok((self, Ok(()))),
            // Check that the process is running if we get an Io error
            Err(err) => match check_health(self.proc) {
                // The process is still running
                Ok(proc) => {
                    self.proc = proc;
                    Ok((self, Err(ErrorKind::Connect(eyre!(err)).into())))
                }
                // The process crashed
                Err(err) => Err(ErrorKind::Crash(err).into()),
            },
        }
    }

    fn receive(mut self) -> Result<(Self, Result<Reply>)> {
        match bincode::DefaultOptions::new()
            .deserialize_from(&mut self.stream)
            .wrap_err("Error receiving messages")
        {
            Ok(reply) => Ok((self, Ok(reply))),
            // Check that the process is running if we get an Io error
            Err(err) => match check_health(self.proc) {
                // The process is still running
                Ok(proc) => {
                    self.proc = proc;
                    Ok((self, Err(ErrorKind::Connect(eyre!(err)).into())))
                }
                // The process crashed
                Err(e) => Err(ErrorKind::Crash(e.wrap_err(err)).into()),
            },
        }
    }
}

#[derive(Debug)]
struct Transceiver {
    stream: UnixStream,
    buf: Vec<u8>,
    fds: Vec<RawFd>,
    bytes: usize,
    nfd: usize,
}

impl Transceiver {
    fn new(stream: UnixStream) -> Self {
        Self {
            stream,
            buf: vec![0; 1024],
            fds: vec![0; 16],
            bytes: 0,
            nfd: 0,
        }
    }

    fn next_fd(&mut self) -> std::io::Result<RawFd> {
        loop {
            if self.nfd == 0 {
                let (bytes, nfd) = self
                    .stream
                    .recv_with_fd(&mut self.buf[self.bytes..], &mut self.fds)?;
                self.bytes += bytes;
                self.nfd += nfd;
                continue;
            }

            let fd = self.fds[0];
            self.fds.copy_within(1..self.nfd, 0);
            self.nfd -= 1;
            return Ok(fd);
        }
    }
}

impl Read for Transceiver {
    fn read(&mut self, mut buf: &mut [u8]) -> std::io::Result<usize> {
        if self.bytes > 0 {
            let bytes = buf.write(&self.buf[..self.bytes])?;
            self.buf.copy_within(bytes..self.bytes, 0);
            self.bytes -= bytes;
            return Ok(bytes);
        }

        let (bytes, nfd) = self.stream.recv_with_fd(buf, &mut self.fds[self.nfd..])?;
        self.nfd += nfd;
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

const SOCKET: &str = "/tmp/ladybug_server.sock";
const ENV_VAR: &str = "LADYBUG_PRIVILEGED";

fn check_health(mut proc: Child) -> color_eyre::Result<Child> {
    if let Ok(Some(status)) = proc.try_wait() {
        let output = proc
            .wait_with_output()
            .wrap_err("Failed to collect output")?;
        println!("Server exited");
        let err = common::command::check_output(output)
            .map_err(color_eyre::Report::from)
            .err()
            .unwrap_or_else(|| eyre!("Process exited with status {status}",));
        Err(err)
    } else {
        Ok(proc)
    }
}

pub type SuperCtx = Client;

pub struct Command {
    program: OsString,
    args: Vec<OsString>,
    dir: Option<PathBuf>,
    env: HashMap<OsString, OsString>,
    stdin: Stdio,
    stdout: Stdio,
    stderr: Stdio,
}

impl Command {
    pub fn new<S: AsRef<OsStr>>(program: S) -> Self {
        Self {
            program: program.as_ref().to_owned(),
            args: Vec::new(),
            dir: None,
            env: HashMap::new(),
            stdin: Stdio::inherit(),
            stdout: Stdio::inherit(),
            stderr: Stdio::inherit(),
        }
    }

    pub fn arg<S: AsRef<OsStr>>(&mut self, arg: S) -> &mut Command {
        self.args.push(arg.as_ref().to_owned());
        self
    }

    pub fn args<I, S>(&mut self, args: I) -> &mut Command
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        for arg in args {
            self.arg(arg.as_ref());
        }
        self
    }

    pub fn env<K, V>(&mut self, key: K, val: V) -> &mut Command
    where
        K: AsRef<OsStr>,
        V: AsRef<OsStr>,
    {
        self.env
            .insert(key.as_ref().to_owned(), val.as_ref().to_owned());
        self
    }

    pub fn envs<I, K, V>(&mut self, vars: I) -> &mut Command
    where
        I: IntoIterator<Item = (K, V)>,
        K: AsRef<OsStr>,
        V: AsRef<OsStr>,
    {
        for (ref key, ref val) in vars {
            self.env(key.as_ref(), val.as_ref());
        }
        self
    }

    pub fn env_remove<K: AsRef<OsStr>>(&mut self, key: K) -> &mut Command {
        self.env.remove(key.as_ref());
        self
    }

    pub fn env_clear(&mut self) -> &mut Command {
        self.env.clear();
        self
    }

    pub fn current_dir<P: AsRef<Path>>(&mut self, dir: P) -> &mut Command {
        self.dir = Some(dir.as_ref().to_owned());
        self
    }

    pub fn stdin<T: Into<Stdio>>(&mut self, cfg: T) -> &mut Command {
        self.stdin = cfg.into();
        self
    }

    pub fn stdout<T: Into<Stdio>>(&mut self, cfg: T) -> &mut Command {
        self.stdout = cfg.into();
        self
    }

    pub fn stderr<T: Into<Stdio>>(&mut self, cfg: T) -> &mut Command {
        self.stderr = cfg.into();
        self
    }

    pub fn into_std_cmd(&self) -> StdCommand {
        let Command {
            program,
            args,
            dir,
            env,
            stdin,
            stdout,
            stderr,
        } = self;

        let mut cmd = StdCommand::new(program);
        cmd.args(args).envs(env);

        if let Some(dir) = dir {
            cmd.current_dir(dir);
        }

        cmd.stdin::<StdStdio>(stdin.0.into());
        cmd.stdout::<StdStdio>(stdout.0.into());
        cmd.stderr::<StdStdio>(stderr.0.into());
        cmd
    }
}

pub struct Stdio(StdioVal);

impl Stdio {
    pub fn inherit() -> Self {
        Self(StdioVal::Inherit)
    }
    pub fn piped() -> Self {
        Self(StdioVal::Piped)
    }
    pub fn null() -> Self {
        Self(StdioVal::Null)
    }
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, Clone, Copy)]
enum StdioVal {
    Inherit,
    Piped,
    Null,
}

impl Into<std::process::Stdio> for StdioVal {
    fn into(self) -> std::process::Stdio {
        match self {
            StdioVal::Inherit => std::process::Stdio::inherit(),
            StdioVal::Piped => std::process::Stdio::piped(),
            StdioVal::Null => std::process::Stdio::null(),
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
    /// Spawn either a privileged or user command. The `StdioSet` is used to set
    /// the stdio of a privileged process but not a user one!
    pub fn spawn(cmd: &mut Command, user: bool, ctx: &mut SuperCtx) -> Result<Process> {
        if user {
            cmd.into_std_cmd()
                .spawn()
                .map(Into::into)
                .map_err(|err| ErrorKind::Other(eyre!(err)).into())
        } else {
            ctx.spawn(cmd).map(Into::into)
        }
    }

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

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
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
    /// Convert the output to an error if the process failed
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

#[cfg(test)]
mod tests {
    use std::io::Read;

    use super::{detect_privileged, Client, Command, Stdio};

    #[test]
    fn test_fd() -> color_eyre::Result<()> {
        detect_privileged();

        let mut client = Client::new();
        let mut cmd = Command::new("echo");
        cmd.stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .stdin(Stdio::piped());
        cmd.arg("hey it's me");
        let mut child = client.spawn(&mut cmd)?;
        let mut buf = String::new();
        let mut stdout = child.stdout.take().unwrap();
        stdout.read_to_string(&mut buf)?;
        assert_eq!(&buf, "hey it's me");
        Ok(())
    }
}
