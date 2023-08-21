use std::{
    io::{BufRead, BufReader},
    process::{Command, Stdio},
    rc::Rc,
};

use ariadne::{Color, Fmt, ReportKind};
use color_eyre::{
    eyre::{bail, eyre, WrapErr},
    Result, Section,
};
use common::command;
use eval::{params, report, FromArgs, RecoverFromArgs};
use parser::Span;
use tracing::debug;

use crate::{
    privileged::{self, SuperCtx},
    ExecCtx,
};

use super::ProviderError;

#[derive(Debug)]
pub enum Error {
    PackageNotRecognized(String, Span),
    AmbiguousPackage(String, Span),
    Eyre(color_eyre::Report, Span),
}

report! {
    Error {
        Error::PackageNotRecognized(package, span) => {
            report(ReportKind::Error, span.start);
            message("Package '{}' doesn't exist", package.fg(Color::Red));
            label(span, Color::Red, "Couldn't find this package");
        }
        Error::AmbiguousPackage(package, span) => {
            report(ReportKind::Error, span.start);
            message("Package name '{}' is ambiguous", package.fg(Color::Red));
            label(span, Color::Red, "Multiple results found");
        }
        Error::Eyre(err, span) => {
            report(ReportKind::Error, span.start);
            message("Encountered an error while processing package");
            label(span, Color::Red, "{err:#}");
        }
    }
}

/// The flatpak provider is stateless
pub struct Provider;

impl super::ConstructProvider for Provider {
    fn new() -> Result<Self, ProviderError> {
        // try to run 'flatpak --version' to see whether flatpak is installed
        let out = Command::new("flatpak")
            .arg("--version")
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .wrap_err("Failed to spawn flatpak process")
            .and_then(|mut cmd| cmd.wait().wrap_err("Failed to run flatpak"));

        match out {
            Ok(status) => {
                if status.success() {
                    Ok(Self)
                } else {
                    Err(ProviderError::Unavailable(Rc::new(eyre!(
                        "Flatpak exited with exit code {status}"
                    ))))
                }
            }
            Err(err) => Err(ProviderError::Unavailable(Rc::new(err))),
        }
    }
}

params! {
    struct Params {
        user: Option<bool>,
    }
}
params! {
    struct PackageParams {}
}

struct Package {
    name: String,
}

pub struct Transaction {
    /// Work on user installation
    user: bool,
    packages: Vec<Package>,
}

impl super::Provider for Provider {
    type Transaction = Transaction;
    type State = ();

    fn new_transaction(
        &mut self,
        args: crate::Args,
        packages: crate::Packages,
        ctx: &mut super::EvalCtx,
        _exec: &ExecCtx,
    ) -> Result<Self::Transaction, ()> {
        // Parse the package block params
        let params = Params::recover_default(args, ctx);
        let user = params.user.unwrap_or_default();

        let mut packs = Vec::new();
        let mut degraded = params.is_degraded();
        // Verify packages
        for package in packages.packages {
            if let Some(args) = PackageParams::from_args(package.args, ctx) {
                if args.is_degraded() {
                    degraded = true;
                    continue;
                }

                match exists(&package.name.inner, user) {
                    Ok(PackageStatus::Ok) => {
                        let package = Package {
                            name: package.name.inner,
                        };
                        packs.push(package);
                    }
                    Ok(PackageStatus::Ambiguous) => {
                        ctx.emit(Error::AmbiguousPackage(
                            package.name.inner,
                            package.name.span,
                        ));
                        degraded = true;
                    }
                    Ok(PackageStatus::Missing) => {
                        ctx.emit(Error::PackageNotRecognized(
                            package.name.inner,
                            package.name.span,
                        ));
                        degraded = true;
                    }
                    Err(err) => {
                        ctx.emit(Error::Eyre(err, package.name.span));
                        degraded = true;
                    }
                }
            }
        }

        if !degraded && !params.is_degraded() {
            Ok(Transaction {
                user,
                packages: packs,
            })
        } else {
            Err(())
        }
    }

    fn install(
        &mut self,
        transaction: &Self::Transaction,
        ctx: &mut SuperCtx,
        exec: &ExecCtx,
    ) -> (super::OpResult, Self::State) {
        let res = flatpak_op(Operation::Install, transaction, ctx, exec);
        (res, ())
    }

    fn remove(
        &mut self,
        transaction: &Self::Transaction,
        _state: Option<Self::State>,
        ctx: &mut SuperCtx,
        exec: &ExecCtx,
    ) -> super::OpResult {
        flatpak_op(Operation::Remove, transaction, ctx, exec)
    }
}

enum Operation {
    Install,
    Remove,
}

fn flatpak_op(
    operation: Operation,
    transaction: &Transaction,
    ctx: &mut SuperCtx,
    exec: &ExecCtx,
) -> super::OpResult {
    let op = match operation {
        Operation::Install => "install",
        Operation::Remove => "remove",
    };

    let mut cmd = Command::new("flatpak");
    cmd.arg(op);
    if transaction.user {
        cmd.arg("--user");
    }
    cmd.arg("--noninteractive")
        .args(transaction.packages.iter().map(|p| &p.name))
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    let mut proc =
        spawn_command(&mut cmd, transaction.user, ctx).wrap_err("Failed to run flatpak")?;

    let mut stdout = BufReader::new(proc.take_stdout().unwrap());
    let mut buf = String::new();

    let res = loop {
        buf.clear();
        match stdout.read_line(&mut buf) {
            //EOF
            Ok(0) => break Ok(()),
            Ok(_) => {
                let mut msg = buf.split(':');
                match msg.next() {
                    Some("Info") => tracing::debug!("{}", msg.next().unwrap()),
                    Some(s) if s.starts_with("Installing") || s.starts_with("Uninstalling") => {
                        debug!("{}", &s);
                        exec.set_message(s.into());
                    }
                    _ => (),
                }
            }
            Err(err) => break Err(err),
        }
    };

    proc.put_stdout(stdout.into_inner());
    let output = ctx
        .wait_with_output(proc)
        .wrap_err("Failed to run flatpak")
        .and_then(|output| output.into_result().map_err(|err| err.into_report()));

    match (output, res) {
        (Err(e1), Err(e2)) => Err(e1).error(e2),
        (Err(err), _) => Err(err),
        (_, Err(err)) => Err(eyre!(err)),
        _ => Ok(()),
    }
}

enum PackageStatus {
    Ok,
    Ambiguous,
    Missing,
}

/// Check if a package exists
fn exists(name: &str, user: bool) -> color_eyre::Result<PackageStatus> {
    let mut cmd = Command::new("flatpak");
    cmd.arg("search")
        .arg(name)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    if user {
        cmd.arg("--user");
    };

    match command::run_command(&mut cmd) {
        Ok(output) => {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let mut lines = stdout.lines();
            match (lines.next(), lines.next()) {
                // No output
                (None, None) => bail!("Flatpak returned no output"),
                // No matches
                (Some("No matches found"), None) => Ok(PackageStatus::Missing),
                // Just one package
                (Some(_), None) => Ok(PackageStatus::Ok),
                // Multiple packages; ambiguous
                (Some(_), Some(_)) => Ok(PackageStatus::Ambiguous),
                _ => unreachable!(),
            }
        }
        Err(err) => Err(err.into_report()).wrap_err("Failed to run flatpak"),
    }
}

fn spawn_command(
    cmd: &mut std::process::Command,
    user: bool,
    ctx: &mut SuperCtx,
) -> Result<privileged::Process, privileged::Error> {
    if user {
        cmd.spawn()
            .map(Into::into)
            .map_err(privileged::Error::Other)
    } else {
        ctx.spawn(cmd).map(Into::into)
    }
}
