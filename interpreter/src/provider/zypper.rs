use std::{
    io::{BufReader, Write},
    process::{ChildStdin, ChildStdout, Stdio},
    time::Duration,
};

use ariadne::{Color, Fmt, ReportKind};
use color_eyre::eyre::{eyre, WrapErr};
use quick_xml::events::Event;
use quick_xml::reader::Reader as XmlReader;
use timeout_readwrite::TimeoutReader;

use super::{ExecutionCtx, ProviderError};
use crate::{
    structures::{FromArgs, RecoverFromArgs},
    Ctx, Span,
};

#[derive(Debug)]
pub enum Error {
    PackageNotRecognized(String, Span),
    Eyre(color_eyre::Report, Span),
}

impl Into<crate::error::Error> for Error {
    fn into(self) -> crate::error::Error {
        super::TransactionError::Zypper(self).into()
    }
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
            message("Encountered an error while proccessing package");
            label(span, Color::Red, "{err}");
        }
    }
}

#[derive(PartialEq)]
enum Operation {
    Install,
    Remove,
}

/// The zypper provider spawns a persistent zypper process using `zypper shell` and uses stdio to
/// communicate commands.
pub struct Provider {
    /// The zypper process
    zypper: std::process::Child,
    stdin: ChildStdin,
    reader: XmlReader<BufReader<TimeoutReader<ChildStdout>>>,
    buf: Vec<u8>,
}

impl super::ConstructProvider for Provider {
    fn new(root: bool) -> Result<Self, ProviderError> {
        if !root {
            return Err(ProviderError::NeedRoot);
        }

        // Try to spawn the zypper subprocess
        let mut zypper = std::process::Command::new("zypper")
            .arg("--xmlout") // Enable XML output
            .arg("--non-interactive") // Don't ask for confirmation
            .arg("shell")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .wrap_err("Failed to spawn zypper process")
            .map_err(|err| ProviderError::Unavailable(std::rc::Rc::new(err)))?;

        let reader = TimeoutReader::new(zypper.stdout.take().unwrap(), Duration::from_secs(10));
        let reader = BufReader::new(reader);
        let mut reader = XmlReader::from_reader(reader);
        reader.trim_text(true);
        let stdin = zypper.stdin.take().unwrap();
        let buf = Vec::new();

        let mut new = Self {
            zypper,
            stdin,
            reader,
            buf,
        };

        // Pop the initialization events
        loop {
            match new.read_event() {
                Ok(Event::Start(tag)) => {
                    if tag.local_name().as_ref() == b"stream" {
                        break;
                    }
                }
                // Let's hope that any errors here aren't fatal
                _ => (),
            }
        }

        // See if the subprocess exited unexpectedly
        match new.check_health() {
            Ok(_) => Ok(new),
            Err(err) => Err(ProviderError::Unavailable(std::rc::Rc::new(err))),
        }
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
        ctx: &mut Ctx,
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

                match self.exists(&package.name.inner) {
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
        ctx: ExecutionCtx,
    ) -> (super::OpResult, Self::State) {
        let res = self
            .zypper_op(transaction, Operation::Install, ctx)
            .and_then(|_| {
                self.check_health()
                    .wrap_err("Zypper process exited unexpectedly")
            });
        (res, ())
    }

    fn remove(
        &mut self,
        transaction: &Self::Transaction,
        _state: Option<Self::State>,
        ctx: ExecutionCtx,
    ) -> super::OpResult {
        self.zypper_op(transaction, Operation::Remove, ctx)
            .and_then(|_| {
                self.check_health()
                    .wrap_err("Zypper process exited unexpectedly")
            })
    }
}

impl Provider {
    /// Get the subprocess' stdin handle
    fn stdin(&mut self) -> &mut ChildStdin {
        &mut self.stdin
    }

    fn read_event<'a>(&'a mut self) -> quick_xml::Result<Event<'a>> {
        self.reader.read_event_into(&mut self.buf)
    }

    /// Check if the package named 'package' exists
    fn exists(&mut self, package: &str) -> color_eyre::Result<bool> {
        writeln!(self.stdin(), "search --match-exact {}", &package)
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

    /// See if the subprocess exited unexpectedly
    fn check_health(&mut self) -> color_eyre::Result<()> {
        match self.zypper.try_wait() {
            Ok(Some(status)) => Err(eyre!("Zypper process exited unexpectedly with: {status}")),
            Err(e) => Err(eyre!(e)),
            Ok(None) => Ok(()),
        }
    }

    fn zypper_op(
        &mut self,
        transaction: &Transaction,
        op: Operation,
        mut ctx: ExecutionCtx,
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
                state.push_str(&package.name);
                state
            });
        let cmd = match op {
            Operation::Install => "install",
            Operation::Remove => "remove",
        };
        writeln!(self.stdin(), "{}{} {}", cmd, from, packages)
            .wrap_err("Failed to write to stdout")?;

        let mut handle_error = false;
        let mut level = 0;
        let mut error = None;
        let mut finished = false;

        loop {
            let event = self.read_event();
            match event {
                Ok(Event::Start(tag)) => {
                    if tag.local_name().as_ref() == b"message" {
                        let ty = tag.attributes().next().unwrap()?;
                        assert_eq!(ty.key.local_name().as_ref(), b"type");
                        if ty.value.as_ref() == b"error" {
                            handle_error = true;
                        }
                    }
                    level += 1;
                }
                Ok(Event::Empty(tag)) => {
                    let mut attributes = tag.attributes();
                    if tag.local_name().as_ref() == b"progress" {
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
                                    ctx.set_message(&name);

                                    // The name is in the format "(n/n) blabla"
                                    // The transaction is complete if the numbers in the parantheses match
                                    let (s, _) =
                                        name.strip_prefix('(').unwrap().split_once(')').unwrap();
                                    let (left, right) = s.split_once('/').unwrap();
                                    if left == right {
                                        finished = true;
                                    }
                                }
                            }
                        }
                    }
                }
                Ok(Event::End(_)) => level -= 1,
                Ok(Event::Text(message)) => {
                    let message = message.unescape().unwrap();

                    // Zypper ends progress messages with '...'
                    if message.ends_with("...") {
                        ctx.set_message(&message);
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
