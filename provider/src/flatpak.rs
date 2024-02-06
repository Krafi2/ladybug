use std::{
    io::{BufRead, BufReader},
    process::{Command, Stdio},
    rc::Rc,
};

use ariadne::{Color, Fmt, ReportKind};
use color_eyre::{
    eyre::{bail, eyre, WrapErr},
    Result,
};
use common::command;
use data::Package;
use eval::{params, report, Args, FromArgs, RecoverFromArgs};
use parser::Span;
use serde::{Deserialize, Serialize};
use tracing::debug;

use crate::{OpCtx, OpError, ParseCtx, ParseError, ProviderId, RawPackages};

use super::{Provider, ProviderError};

pub(crate) fn new_provider() -> Result<impl crate::Provider, ProviderError> {
    Flatpak::new()
}

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
pub struct Flatpak;

impl Flatpak {
    fn new() -> Result<Self, ProviderError> {
        // try to run 'flatpak --version' to see whether flatpak is installed
        let out = std::process::Command::new("flatpak")
            .arg("--version")
            .stdin(std::process::Stdio::null())
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
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

impl Provider for Flatpak {
    // TODO set messages
    fn parse_packages(
        &mut self,
        args: Args,
        packages: RawPackages,
        ctx: &mut ParseCtx,
    ) -> Result<Vec<data::Package>, ()> {
        // Parse the package block params
        let params = Params::recover_default(args, ctx.eval());
        let user = params.user.unwrap_or_default();

        let mut finished = Vec::new();
        let mut degraded = params.is_degraded();

        // Verify packages
        for package in packages.packages {
            if let Some(args) = PackageParams::from_args(package.args, ctx.eval()) {
                if args.is_degraded() {
                    degraded = true;
                    continue;
                }

                match exists(&package.name.inner, user) {
                    Ok(PackageStatus::Ok) => match bincode::serialize(&Metadata { user }) {
                        Ok(metadata) => {
                            let package = Package::new(
                                package.name.inner,
                                metadata,
                                ctx.runtime().topic(),
                                ProviderId::Flatpak,
                            );
                            finished.push(package);
                            continue;
                        }
                        Err(err) => ctx.emit(ParseError::Bincode(err, package.span)),
                    },
                    Ok(PackageStatus::Ambiguous) => {
                        ctx.emit(Error::AmbiguousPackage(
                            package.name.inner,
                            package.name.span,
                        ));
                    }
                    Ok(PackageStatus::Missing) => {
                        ctx.emit(Error::PackageNotRecognized(
                            package.name.inner,
                            package.name.span,
                        ));
                    }
                    Err(err) => {
                        ctx.emit(Error::Eyre(err, package.name.span));
                    }
                }
            }
            degraded = true;
        }

        if !degraded && !params.is_degraded() {
            Ok(finished)
        } else {
            Err(())
        }
    }

    fn install(&mut self, packages: Vec<data::Package>, ctx: &mut OpCtx) {
        flatpak_op(Operation::Install, packages, ctx)
    }

    fn remove(&mut self, packages: Vec<data::Package>, ctx: &mut OpCtx) {
        flatpak_op(Operation::Remove, packages, ctx)
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

#[derive(Serialize, Deserialize)]
struct Metadata {
    user: bool,
}

#[derive(Clone, Copy)]
enum Operation {
    Install,
    Remove,
}

fn flatpak_op(operation: Operation, packages: Vec<Package>, ctx: &mut OpCtx) {
    let mut user = Vec::new();
    let mut sup = Vec::new();
    let mut errors = Vec::new();
    for package in packages.into_iter() {
        match bincode::deserialize::<Metadata>(package.metadata()) {
            Err(err) => errors.push(OpError::Metadata(err)),
            Ok(metadata) if metadata.user => user.push(package),
            Ok(_) => sup.push(package),
        }
    }
    // Deploy user packages
    do_op(operation, true, user, ctx);
    // Deploy system packages
    do_op(operation, false, sup, ctx);
}

fn do_op(operation: Operation, user: bool, packages: Vec<Package>, ctx: &mut OpCtx) {
    let op = match operation {
        Operation::Install => "install",
        Operation::Remove => "remove",
    };

    let mut cmd = Command::new("flatpak");
    cmd.arg(op);
    if user {
        cmd.arg("--user");
    }
    cmd.arg("--noninteractive")
        .args(packages.iter().map(|p| p.name()))
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    let proc = match user {
        true => cmd.spawn(),
        false => ctx.sup.spawn(&cmd),
    }
    .wrap_err("Failed to run flatpak");

    let mut proc = match proc {
        Ok(proc) => proc,
        Err(err) => {
            ctx.emit(err);
            return;
        }
    };

    let mut stdout = BufReader::new(proc.stdout.take().unwrap());
    let mut buf = String::new();
    let mut degraded = false;

    // TODO: find a way to verify that each package got installed correctly.
    // Currently cannot find failures to test against.
    loop {
        buf.clear();
        match stdout.read_line(&mut buf).wrap_err("Failed to read stdout") {
            //EOF
            Ok(0) => break,
            Ok(_) => {
                // We want to filter out things like "Info: aaaaa"
                let mut msg = buf.split(':');
                match msg.next() {
                    Some("Info") => tracing::debug!("{}", msg.next().unwrap()),
                    Some(s)
                        if s.starts_with("Installing")
                            || s.starts_with("Uninstalling")
                            || s.starts_with("Updating")
                            || s.starts_with("Skipping") =>
                    {
                        debug!("{}", &s);
                        ctx.runtime().set_message(s.into());
                    }
                    _ => (),
                }
            }
            Err(err) => {
                degraded = true;
                ctx.emit(OpError::Other(err));
                break;
            }
        }
    }

    proc.stdout = Some(stdout.into_inner());

    let _ = proc
        .wait_with_output()
        .wrap_err("Failed to run flatpak")
        .and_then(|output| common::command::check_output(output).map_err(|err| err.into_report()))
        .map_err(|err| {
            degraded = true;
            ctx.emit(OpError::Other(err))
        });

    for package in packages {
        if degraded {
            ctx.failed(package)
        } else {
            ctx.installed(package)
        }
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
