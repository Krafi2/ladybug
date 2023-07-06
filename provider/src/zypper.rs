use std::{
    fs::File,
    io::{BufReader, Read, Write},
    process::{Child, ChildStdin, ChildStdout, Stdio},
    time::Duration,
};

use ariadne::{Color, Fmt, ReportKind};
use color_eyre::eyre::{eyre, WrapErr};
use eval::{params, report, FromArgs, RecoverFromArgs};
use parser::Span;
use quick_xml::events::Event;
use quick_xml::reader::Reader as XmlReader;
use timeout_readwrite::TimeoutReader;
use tracing::debug;

use super::ProviderError;
use crate::{privileged::SuperCtx, EvalCtx};

#[derive(Debug)]
pub enum Error {
    PackageNotRecognized(String, Span),
    Eyre(color_eyre::Report, Span),
}

report! {
    Error {
        Error::PackageNotRecognized(package, span) => {
            report(ReportKind::Error, span.start);
            message("Package '{}' doesn't exist", package.fg(Color::Red));
            label(span, Color::Red, "Couldn't find this package");
        }
        Error::Eyre(err, span) => {
            report(ReportKind::Error, span.start);
            message("Encountered an error while processing package");
            label(span, Color::Red, "{err:#}");
        }
    }
}

#[derive(PartialEq)]
enum Operation {
    Install,
    Remove,
}

struct Zypper<I, O> {
    stdin: I,
    reader: XmlReader<BufReader<O>>,
    buf: Vec<u8>,
}

impl<I: Write, O: Read> Zypper<I, O> {
    fn new(stdin: I, stdout: O) -> color_eyre::Result<Self> {
        let mut reader = XmlReader::from_reader(BufReader::new(stdout));
        reader.trim_text(true);

        let mut new = Self {
            stdin,
            reader,
            buf: Vec::new(),
        };

        // Pop the initialization events
        loop {
            match new.read_event() {
                Ok(Event::Start(tag)) => {
                    if tag.local_name().as_ref() == b"stream" {
                        break;
                    }
                }
                Err(quick_xml::Error::Io(err)) => return Err(eyre!(err)).wrap_err("IO error"),
                Err(e) => return Err(e).wrap_err("Failed to parse xml"),
                _ => (),
            }
        }

        Ok(new)
    }

    fn read_event<'a>(&'a mut self) -> quick_xml::Result<Event<'a>> {
        self.reader.read_event_into(&mut self.buf)
    }

    /// Check if the package named 'package' exists
    fn exists(&mut self, package: &str) -> color_eyre::Result<bool> {
        writeln!(&mut self.stdin, "search --match-exact {}", &package)
            .wrap_err("Failed to write zypper stdin")?;
        let mut found = None;
        let mut level = 0;
        let mut handle_error = false;
        let mut error = None;

        loop {
            let event = self.read_event();
            match event {
                Ok(Event::Start(tag)) => {
                    if tag.local_name().as_ref() == b"message" {
                        let ty = tag.attributes().next().unwrap()?;
                        assert_eq!(ty.key.local_name().as_ref(), b"type");
                        // Handle error message
                        if ty.value.as_ref() == b"error" {
                            handle_error = true;
                        }
                    }
                    level += 1;
                }
                Ok(Event::Empty(tag)) => {
                    // This means that we got a search result
                    if tag.local_name().as_ref() == b"solvable" {
                        found = Some(true);
                    }
                }
                Ok(Event::End(_)) => level -= 1,
                Ok(Event::Text(message)) => {
                    let message = message.unescape().unwrap();
                    if handle_error {
                        error = Some(message.into_owned());
                    } else if message == "No matching items found." {
                        found = Some(false);
                    }
                }
                Ok(Event::Eof) => return Err(eyre!("Unexpected EOF")),
                Err(quick_xml::Error::Io(err)) => return Err(eyre!(err)).wrap_err("IO error"),
                Err(e) => return Err(e).wrap_err("Failed to parse xml"),
                _ => (),
            }
            if level == 0 {
                if let Some(error) = error {
                    return Err(eyre!(error));
                }
                if let Some(found) = found {
                    return Ok(found);
                }
            }
        }
    }

    fn zypper_op(
        &mut self,
        transaction: &Transaction,
        op: Operation,
        mut ctx: SuperCtx,
    ) -> Result<(), color_eyre::Report> {
        let from = if let Some(from) = &transaction.from {
            from.iter()
                .map(String::as_str)
                .flat_map(|s| [" --from ", s])
                .collect::<String>()
        } else {
            "".to_owned()
        };
        let packages = transaction
            .packages
            .iter()
            .fold(String::new(), |mut state, package| {
                state.push_str(" ");
                // Remove packages by prepending !
                if let Operation::Remove = op {
                    state.push('!');
                }
                state.push_str(&package.name);
                state
            });
        let cmd = format!("install{} {}", from, packages);
        debug!("Running '{cmd}'");
        writeln!(&mut self.stdin, "{cmd}").wrap_err("Failed to write to stdout")?;

        let mut handle_error = false;
        let mut level = 0;
        let mut error = None;
        let mut finished = false;

        loop {
            let event = self.read_event();
            match event {
                Ok(Event::Start(tag)) => {
                    match tag.local_name().as_ref() {
                        b"message" => {
                            let ty = tag.attributes().next().unwrap()?;
                            assert_eq!(ty.key.local_name().as_ref(), b"type");
                            if ty.value.as_ref() == b"error" {
                                handle_error = true;
                            }
                        }
                        // There were problems and the operation was canceled
                        b"prompt" => {
                            let id = tag.attributes().next().unwrap()?;
                            assert_eq!(id.key.local_name().as_ref(), b"id");
                            if id.value.as_ref() == b"1" {
                                finished = true;
                            }
                        }
                        _ => (),
                    }
                    level += 1;
                }
                Ok(Event::Empty(tag)) => {
                    let mut attributes = tag.attributes();
                    match tag.local_name().as_ref() {
                        b"progress" => {
                            let id = attributes.next().unwrap()?;
                            let name = attributes.next().unwrap()?;
                            assert_eq!(id.key.local_name().as_ref(), b"id");
                            assert_eq!(name.key.local_name().as_ref(), b"name");

                            if id.value.as_ref() == b"install-resolvable"
                                || id.value.as_ref() == b"remove-resolvable"
                            {
                                if let Some(done) = attributes.next().transpose()? {
                                    if done.key.local_name().as_ref() == b"done" {
                                        // Print what has been installed
                                        let name = String::from_utf8_lossy(&name.value);
                                        ctx.set_message(name.into_owned());
                                        debug!("{}", &name);

                                        // The name is in the format "(n/n) blabla"
                                        // The transaction is complete if the numbers in the parantheses match
                                        let (s, _) = name
                                            .strip_prefix('(')
                                            .unwrap()
                                            .split_once(')')
                                            .unwrap();
                                        let (left, right) = s.split_once('/').unwrap();
                                        if left == right {
                                            finished = true;
                                        }
                                    }
                                }
                            }
                        }
                        _ => (),
                    }
                }
                Ok(Event::End(_)) => level -= 1,
                Ok(Event::Text(message)) => {
                    let message = message.unescape().unwrap();

                    // Zypper ends progress messages with '...'
                    if message.ends_with("...") {
                        ctx.set_message(message.into_owned());
                    }

                    // Zypper feels like it's finished
                    if message == "Nothing to do." {
                        finished = true;
                    }

                    if handle_error {
                        // Ignore errors related to unknown packages when removing
                        if op == Operation::Remove
                            && message.starts_with("No provider of ")
                            && message.ends_with(" found.")
                        {
                            ()
                        } else {
                            error = Some(message.into_owned());
                        }
                        handle_error = false;
                    }
                }
                Ok(Event::Eof) => return Err(eyre!("Unexpected EOF")),
                Err(quick_xml::Error::Io(err)) => return Err(eyre!(err)).wrap_err("IO error"),
                Err(e) => return Err(e).wrap_err("Failed to parse xml"),
                _ => (),
            }
            if level == 0 {
                if let Some(error) = error {
                    return Err(eyre!(error));
                }
                if finished {
                    return Ok(());
                }
            }
        }
    }
}

type SuperZypper = Zypper<File, TimeoutReader<File>>;
type UserZypper = Zypper<ChildStdin, TimeoutReader<ChildStdout>>;

/// The zypper provider spawns a persistent zypper process using `zypper shell` and uses stdio to
/// communicate commands.
pub enum Provider {
    /// A normal proccess running without privileges
    Process { zypper: UserZypper, proc: Child },
    /// A privileged process spawned by the super context
    Super { zypper: SuperZypper },
}

const CMD: &str = "zypper";
const ARGS: [&str; 3] = ["--xmlout", "--non-interactive", "shell"];
const TIMEOUT: Duration = Duration::from_secs(10);

impl super::ConstructProvider for Provider {
    fn new() -> Result<Self, ProviderError> {
        // Try to spawn the zypper subprocess
        let mut zypper = Self::zypper_cmd()
            .spawn()
            .wrap_err("Failed to spawn zypper process")
            .map_err(|err| ProviderError::Unavailable(std::rc::Rc::new(err)))?;

        let reader = TimeoutReader::new(zypper.stdout.take().unwrap(), TIMEOUT);
        let stdin = zypper.stdin.take().unwrap();

        Ok(Self::Process {
            zypper: Zypper::new(stdin, reader)
                .map_err(|err| ProviderError::Unavailable(std::rc::Rc::new(err)))?,
            proc: zypper,
        })
    }
}

struct Package {
    name: String,
}

pub(super) struct Transaction {
    from: Option<Vec<String>>,
    packages: Vec<Package>,
}

params! { struct Params { from: Option<Vec<String>> } }
params! { struct PackageParams {} }

impl super::Provider for Provider {
    type Transaction = Transaction;
    type State = ();

    fn new_transaction(
        &mut self,
        args: super::Args,
        packages: super::Packages,
        ctx: &mut EvalCtx,
    ) -> Result<Self::Transaction, ()> {
        // Parse the package block params
        let params = Params::recover_default(args, ctx);

        let mut packs = Vec::new();
        let mut degraded = params.is_degraded();
        // Verify packages
        for package in packages.packages {
            if let Some(args) = PackageParams::from_args(package.args, ctx) {
                if args.is_degraded() {
                    degraded = true;
                    continue;
                }

                let exists = match self {
                    Provider::Process { zypper, proc } => zypper.exists(&package.name.inner),
                    Provider::Super { zypper } => zypper.exists(&package.name.inner),
                };

                match exists {
                    // The package may exist
                    Ok(exists) => {
                        // All is well
                        if exists {
                            let package = Package {
                                name: package.name.inner,
                            };
                            packs.push(package);
                        // The package doesn't exist
                        } else {
                            ctx.emit(Error::PackageNotRecognized(
                                package.name.inner,
                                package.name.span,
                            ));
                            degraded = true;
                        }
                    }
                    // Some other error happened while confirming the package's existence
                    Err(other) => {
                        ctx.emit(Error::Eyre(other, package.name.span));
                        degraded = true;
                    }
                }
            }
        }

        if !degraded && !params.is_degraded() {
            Ok(Transaction {
                from: params.value.from,
                packages: packs,
            })
        } else {
            Err(())
        }
    }

    fn install(
        &mut self,
        transaction: &Self::Transaction,
        ctx: SuperCtx,
    ) -> (super::OpResult, Self::State) {
        let res = self
            .get_super(&mut ctx)
            .and_then(|zypper| zypper.zypper_op(transaction, Operation::Install, ctx));
        (res, ())
    }

    fn remove(
        &mut self,
        transaction: &Self::Transaction,
        _state: Option<Self::State>,
        ctx: SuperCtx,
    ) -> super::OpResult {
        self.get_super(&mut ctx)?
            .zypper_op(transaction, Operation::Remove, ctx)
    }
}

impl Provider {
    fn zypper_cmd() -> std::process::Command {
        let mut cmd = std::process::Command::new("zypper");
        cmd.args(["--xmlout", "--non-interactive", "shell"])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());
        cmd
    }

    /// Create a new pivileged super instance
    fn new_super(ctx: &mut SuperCtx) -> color_eyre::Result<SuperZypper> {
        let proc = ctx.spawn(Self::zypper_cmd())?;
        SuperZypper::new(proc.stdin, TimeoutReader::new(proc.stdout, TIMEOUT))
    }

    /// Get a privileged super instance
    fn get_super(&mut self, ctx: &mut SuperCtx) -> color_eyre::Result<&mut SuperZypper> {
        match self {
            // If we have a normal process, kill it and launch a privileged instance
            Provider::Process { zypper, proc } => {
                // Zypper claims to exit gracefully when killed
                proc.kill().wrap_err("Old process already exited")?;
                // Update the process
                *self = Self::Super {
                    zypper: Self::new_super(ctx)?,
                };
            }
            _ => (),
        }
        // We should be running a super now
        match self {
            Provider::Super { zypper } => Ok(zypper),
            Provider::Process { zypper, proc } => unreachable!(),
        }
    }
}
