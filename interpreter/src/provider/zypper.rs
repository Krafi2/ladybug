use super::{ProviderError, Transaction};
use crate::{
    structures::{FromArgs, RecoverFromArgs},
    Ctx, Span,
};
use ariadne::{Color, Fmt, ReportKind};
use color_eyre::eyre::WrapErr;
use std::{
    io::{Read, Write},
    process::Stdio,
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
            message("Encountered an error");
            label(span, Color::Red, "{err}");
        }
    }
}

/// The zypper provider spawns a persistent zypper process using `zypper shell` and uses stdio to
/// communicate commands.
pub struct Provider {
    /// The zypper process
    zypper: std::process::Child,
}

impl Provider {
    /// Get the subprocess' stdin handle
    fn stdin(&mut self) -> &mut std::process::ChildStdin {
        self.zypper
            .stdin
            .as_mut()
            .expect("Expected a valid stdin handle")
    }

    /// Get the subprocess' stdout handle
    fn stdout(&mut self) -> &mut std::process::ChildStdout {
        self.zypper
            .stdout
            .as_mut()
            .expect("Expected a valid stout handle")
    }

    /// Get the subprocess' stderr handle
    fn stderr(&mut self) -> &mut std::process::ChildStderr {
        self.zypper
            .stderr
            .as_mut()
            .expect("Expected a valid stderr handle")
    }

    /// Check if the package named 'package' exists
    fn exists(&mut self, package: &str) -> color_eyre::Result<bool> {
        writeln!(self.stdin(), "info {}", &package).wrap_err("Failed to write stdin")?;
        let mut buf = Vec::new();
        self.stdout()
            .read_to_end(&mut buf)
            .wrap_err("Failed to read stdout")?;

        // This is what zypper says when it can't find the package
        let unfound = contains_bytes(
            &buf,
            format!("package '{}' not found.", &package).as_bytes(),
        );
        Ok(!unfound)
    }

    /// See if the subprocess exited unexpectedly
    fn try_wait(&mut self) -> Result<(), ProcessError> {
        match self.zypper.try_wait() {
            Ok(Some(status)) => Err(ProcessError::Exited(status)),
            Err(e) => Err(ProcessError::Other(e)),
            Ok(None) => Ok(()),
        }
    }

    fn zypper_cmd(&mut self, cmd: &str, transaction: &Transaction) -> color_eyre::Result<()> {
        let transaction = transaction
            .payload
            .downcast_ref::<Payload>()
            .expect("Wrong type");

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

        writeln!(self.stdin(), "{}{} {}", cmd, from, packages)
            .wrap_err("Failed to write to stdout")?;

        let mut buf = Vec::new();
        self.stderr()
            .read_to_end(&mut buf)
            .wrap_err("Failed to read stderr")?;

        // Check if the proccess printed anything to `stderr`. I'm not sure if this will work in
        // all cases but let's give it a try.
        if !buf.is_empty() {
            return Err(color_eyre::eyre::eyre!(
                String::from_utf8_lossy(&buf).into_owned()
            ));
        }

        self.try_wait()
            .map_err(|err| color_eyre::eyre::eyre!(err))?;

        Ok(())
    }
}

struct ZyppPackage {
    name: String,
}

struct Payload {
    from: Option<Vec<String>>,
    packages: Vec<ZyppPackage>,
}

params! { struct ZyppParams { from: Option<Vec<String>> } }
params! { struct PackageParams {} }

impl super::Transactor for Provider {
    fn new_transaction(
        &mut self,
        args: super::Args,
        packages: super::Packages,
        context: &mut Ctx,
    ) -> Result<Transaction, ()> {
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
            Ok(Transaction {
                provider: super::ProviderKind::Zypper.into(),
                payload: Box::new(Payload {
                    from: params.value.from,
                    packages: packs,
                }),
            })
        } else {
            Err(())
        }
    }
}

impl super::Provider for Provider {
    fn install(&mut self, transaction: &Transaction) -> color_eyre::Result<()> {
        self.zypper_cmd("install", transaction)
    }

    fn remove(&mut self, transaction: &Transaction) -> color_eyre::Result<()> {
        self.zypper_cmd("remove", transaction)
    }
}

impl super::ProviderPrivate for Provider {
    fn new(root: bool) -> Result<Self, ProviderError> {
        if !root {
            return Err(ProviderError::NeedRoot);
        }

        // Try to spawn the zypper subprocess
        let zypper = std::process::Command::new("zypper")
            .arg("shell")
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|e| ProviderError::Unavailable(std::rc::Rc::new(ProcessError::Spawn(e))))?;

        let mut new = Self { zypper };
        // See if the subprocess exited unexpectedly
        match new.try_wait() {
            Ok(_) => Ok(new),
            Err(err) => Err(ProviderError::Unavailable(std::rc::Rc::new(err))),
        }
    }
}

/// Search for a string of bytes in a haystack
fn contains_bytes(haystack: &[u8], pattern: &[u8]) -> bool {
    let mut n = 0;
    for byte in haystack.iter() {
        let pat = match pattern.get(n) {
            Some(pat) => pat,
            None => return true,
        };

        if byte == pat {
            n += 1;
        } else {
            n = 0;
        }
    }
    false
}
