use std::{
    io::{BufReader, Write},
    process::{ChildStdin, ChildStdout, Stdio},
};

use ariadne::{Color, Fmt, ReportKind};
use color_eyre::eyre::{eyre, WrapErr};
use quick_xml::events::Event;
use quick_xml::reader::Reader as XmlReader;

use super::ProviderError;
use crate::{
    structures::{FromArgs, RecoverFromArgs},
    Ctx, Span,
};

#[derive(Debug, thiserror::Error)]
enum ProcessError {
    #[error("Failed to spawn zypper process: {0}")]
    Spawn(std::io::Error),
    #[error("Zypper process exited unexpectedly with: {0}")]
    Exited(std::process::ExitStatus),
    #[error("{0}")]
    Other(std::io::Error),
}

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
    reader: XmlReader<BufReader<ChildStdout>>,
    buf: Vec<u8>,
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
                    let message = message.trim();
                    if handle_error {
                        error = Some(message.to_owned());
                    } else if message == "No matching items found." {
                        found = Some(false);
                    }
                }
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
    fn check_health(&mut self) -> Result<(), ProcessError> {
        match self.zypper.try_wait() {
            Ok(Some(status)) => Err(ProcessError::Exited(status)),
            Err(e) => Err(ProcessError::Other(e)),
            Ok(None) => Ok(()),
        }
    }

    fn send_command(
        &mut self,
        transaction: &Payload,
        op: Operation,
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
                                    tracing::debug!("zypper: {}", name);

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
                    let message = message.trim();
                    if handle_error {
                        if op == Operation::Remove
                            && message.starts_with("No provider of ")
                            && message.ends_with(" found.")
                        {
                            // Ignore errors related to unknown packages when removing
                            ()
                        } else {
                            error = Some(message.to_owned());
                        }
                        handle_error = false;
                    }
                }
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

struct ZyppPackage {
    name: String,
}

pub(super) struct Payload {
    from: Option<Vec<String>>,
    packages: Vec<ZyppPackage>,
}

params! { struct ZyppParams { from: Option<Vec<String>> } }
params! { struct PackageParams {} }

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
            .map_err(|e| ProviderError::Unavailable(std::rc::Rc::new(ProcessError::Spawn(e))))?;

        let reader = BufReader::new(zypper.stdout.take().unwrap());
        let reader = XmlReader::from_reader(reader);
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

impl super::Provider for Provider {
    type Transaction = Payload;
    type State = ();

    fn new_transaction(
        &mut self,
        args: super::Args,
        packages: super::Packages,
        context: &mut Ctx,
    ) -> Result<Self::Transaction, ()> {
        // Parse the package block params
        let params = ZyppParams::recover_default(args, context);

        let mut packs = Vec::new();
        let mut degraded = params.is_degraded();
        // Verify packages
        for package in packages.packages {
            if let Some(args) = PackageParams::from_args(package.args, context) {
                if args.is_degraded() {
                    degraded = true;
                    continue;
                }

                match self.exists(&package.name.inner) {
                    // The package may exist
                    Ok(exists) => {
                        // All is well
                        if exists {
                            let package = ZyppPackage {
                                name: package.name.inner,
                            };
                            packs.push(package);
                        // The package doesn't exist
                        } else {
                            context.emit(Error::PackageNotRecognized(
                                package.name.inner,
                                package.name.span,
                            ));
                            degraded = true;
                        }
                    }
                    // Some other error happened while confirming the package's existence
                    Err(other) => {
                        context.emit(Error::Eyre(other, package.name.span));
                        degraded = true;
                    }
                }
            }
        }

        if !degraded && !params.is_degraded() {
            Ok(Payload {
                from: params.value.from,
                packages: packs,
            })
        } else {
            Err(())
        }
    }

    fn install(&mut self, transaction: &Self::Transaction) -> (super::OpResult, Self::State) {
        let res = self
            .send_command(transaction, Operation::Install)
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
    ) -> super::OpResult {
        self.send_command(transaction, Operation::Remove)
            .and_then(|_| {
                self.check_health()
                    .wrap_err("Zypper process exited unexpectedly")
            })
    }
}
