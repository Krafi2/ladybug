use std::{collections::HashMap, rc::Rc};

use ariadne::{Color, Fmt, ReportKind};
use color_eyre::{
    eyre::{ContextCompat, WrapErr},
    Result,
};
use data::Package;
use eval::{params, report, Args, FromArgs, RecoverFromArgs};
use parser::Span;
use pkit_zbus::{package_kit::PackageKitProxyBlocking, transaction::TransactionProxyBlocking};
use zbus::{blocking::Connection, names::MemberName};

use crate::{OpCtx, ParseCtx, ProviderId, RawPackages};

use super::{Provider, ProviderError};

pub(crate) fn new_provider() -> Result<impl crate::Provider, ProviderError> {
    PackageKit::new()
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
pub struct PackageKit {
    conn: Connection,
}

// https://lazka.github.io/pgi-docs/PackageKitGlib-1.0/enums.html#PackageKitGlib.FilterEnum
#[allow(dead_code)]
#[repr(u64)]
enum FilterKind {
    None = 1 << 1,
    Installed = 1 << 2,
    NotInstalled = 1 << 3,
    Newest = 1 << 16,
    Arch = 1 << 18,
}
// https://lazka.github.io/pgi-docs/PackageKitGlib-1.0/enums.html#PackageKitGlib.InfoEnum
#[allow(dead_code)]
#[derive(Debug)]
#[repr(u32)]
enum PackageStatus {
    Unknown = 0,
    Installed = 1,
    Available = 2,
    Unavailable = 25,
    Finished = 18,
    Other(u32),
}

impl From<u32> for PackageStatus {
    fn from(value: u32) -> Self {
        match value {
            0 => Self::Unknown,
            1 => Self::Installed,
            2 => Self::Available,
            18 => Self::Finished,
            25 => Self::Unavailable,
            other => Self::Other(other),
        }
    }
}

#[allow(dead_code)]
#[repr(u64)]
enum TransactionFlag {
    None = 1 << 0,
    OnlyTrusted = 1 << 1,
    AllowReinstall = 1 << 4,
    AllowDowngrade = 1 << 6,
}

#[derive(Clone, Copy)]
enum Operation {
    Install,
    Remove,
}

#[derive(Debug)]
struct PkgData {
    name: String,
    version: String,
    arch: String,
    status: String,
}

impl PkgData {
    fn from_id(package_id: &str) -> Option<Self> {
        let [name, version, arch, status] = package_id
            .split(';')
            .map(ToOwned::to_owned)
            .collect::<Vec<_>>()
            .try_into()
            .ok()?;

        Some(Self {
            name,
            version,
            arch,
            status,
        })
    }

    fn into_id(&self) -> String {
        format!(
            "{};{};{};{}",
            self.name, self.version, self.arch, self.status
        )
    }

    fn installed(&self) -> bool {
        self.status.starts_with("installed")
    }
}

impl PackageKit {
    fn new() -> Result<Self, ProviderError> {
        let conn = Connection::system()
            .wrap_err("Failed to initialize dbus connection")
            .map_err(|err| ProviderError::Unavailable(Rc::new(err)))?;

        Ok(Self { conn })
    }

    // Register a new transaction with packagekit
    fn new_transaction(&mut self) -> color_eyre::Result<TransactionProxyBlocking> {
        let proxy = PackageKitProxyBlocking::new(&self.conn)
            .wrap_err("Failed to connect to the packagekit service")?;
        let path = proxy
            .create_transaction()
            .wrap_err("Failed to create transaction")?;
        let trans = TransactionProxyBlocking::builder(&self.conn)
            .destination("org.freedesktop.PackageKit")?
            .path(path)?
            .build()?;
        Ok(trans)
    }

    // Try to resolve the provided packages. Only known packages are returned.
    fn resolve_packages(
        trans: &TransactionProxyBlocking,
        packages: Vec<String>,
    ) -> color_eyre::Result<Vec<(PkgData, PackageStatus)>> {
        trans.resolve(
            FilterKind::Newest as u64,
            &packages.iter().map(String::as_str).collect::<Vec<_>>(),
        )?;

        let mut packages = Vec::new();

        for signal in trans.inner().receive_all_signals()? {
            match signal.header().member().map(MemberName::as_str) {
                Some("ErrorCode") => {
                    let (_code, details) = signal.body().deserialize::<(u32, String)>()?;
                    color_eyre::eyre::bail!(details);
                }
                Some("Package") => {
                    let (info, package_id, _summary) =
                        signal.body().deserialize::<(u32, String, String)>()?;
                    packages.push((
                        PkgData::from_id(&package_id).wrap_err("Malformed package id")?,
                        PackageStatus::from(info),
                    ));
                }
                Some("progress") => (),
                Some("Finished") => {
                    break;
                }
                other => tracing::warn!("PackageKit: unknow signal '{other:?}'"),
            }
        }

        Ok(packages)
    }

    fn do_op(&mut self, op: Operation, packages: Vec<data::Package>, ctx: &mut OpCtx) {
        // Poor mans try block, sorry
        let op_res: color_eyre::Result<(Vec<String>, Vec<data::Package>)> = (|| {
            // Filter out packages that are already installed/removed
            let packages = packages
                .into_iter()
                .filter_map(
                    |pkg| match (PkgData::from_id(pkg.name()).unwrap().installed(), op) {
                        (false, Operation::Install) | (true, Operation::Remove) => Some(pkg),
                        _ => {
                            ctx.installed(pkg);
                            None
                        }
                    },
                )
                .collect::<Vec<_>>();

            // Short circuit if we don't need to do anything
            if packages.is_empty() {
                return Ok((vec![], vec![]));
            }

            let pack_names = packages.iter().map(|p| p.name()).collect::<Vec<_>>();

            let trans = self.new_transaction()?;
            trans.set_hints(&["interactive=true"])?;

            match op {
                Operation::Install => trans
                    .install_packages(TransactionFlag::None as u64, &pack_names)
                    .wrap_err("Failed to install packages")?,
                Operation::Remove => trans
                    .remove_packages(TransactionFlag::None as u64, &pack_names, false, false)
                    .wrap_err("Failed to remove packages")?,
            }

            let verb = match op {
                Operation::Install => "Installing",
                Operation::Remove => "Removing",
            };

            let mut finished = Vec::new();

            for signal in trans.inner().receive_all_signals()? {
                match signal.header().member().map(MemberName::as_str) {
                    Some("ErrorCode") => {
                        let (_code, details) = signal.body().deserialize::<(u32, String)>()?;
                        color_eyre::eyre::bail!(details);
                    }
                    Some("Package") => {
                        let (_status, package_id, _summary) =
                            signal.body().deserialize::<(u32, String, String)>()?;
                        finished.push(package_id);
                    }
                    Some("ItemProgress") => {
                        let (package_id, _status, percentage) =
                            signal.body().deserialize::<(String, u32, u32)>()?;
                        let total = trans
                            .percentage()
                            .wrap_err("Cannot get transaction details")?;
                        ctx.runtime()
                            .set_message(format!("{total}% |{verb} {package_id}: {percentage}%"))
                    }
                    Some("Finished") => {
                        break;
                    }
                    other => tracing::warn!("PackageKit: unknow signal '{other:?}'"),
                }
            }

            Ok((finished, packages))
        })();

        // Mark successfully applied packages or emit an error
        match op_res {
            Ok((finished, packages)) => {
                // Were gonna be looking up packages based on their names
                let mut pkg_idx = packages
                    .into_iter()
                    .map(|p| (p.name().to_owned(), p))
                    .collect::<HashMap<String, Package>>();

                for name in finished {
                    if let Some(pkg) = pkg_idx.remove(&name) {
                        ctx.installed(pkg);
                    }
                }

                // Assume that all other packages have failed
                for (_, pkg) in pkg_idx.drain() {
                    ctx.failed(pkg);
                }
            }
            Err(err) => ctx.emit(err),
        }
    }
}

params! {
    struct Params {}
}
params! {
    struct PackageParams {}
}

impl Provider for PackageKit {
    // TODO set messages
    fn parse_packages(
        &mut self,
        args: Args,
        packages: RawPackages,
        ctx: &mut ParseCtx,
    ) -> Result<Vec<data::Package>, ()> {
        let arg_span = args.span;
        // Parse the package block params
        let params = Params::recover_default(args, ctx.eval());

        let mut degraded = params.is_degraded();
        let mut pkg_data = packages
            .packages
            .iter()
            .map(|p| (p.name.inner.clone(), (p.span, 0, None)))
            .collect::<HashMap<String, (Span, usize, Option<PkgData>)>>();
        let pack_len = packages.packages.len();

        // Verify the PackageParams of each package
        let to_resolve = packages
            .packages
            .into_iter()
            .filter_map(|p| {
                PackageParams::from_args(p.args, ctx.eval()).and_then(|_| Some(p.name.inner))
            })
            .collect::<Vec<String>>();

        if to_resolve.len() != pack_len {
            degraded = true;
        }

        let trans = match self
            .new_transaction()
            .wrap_err("Failed to connect to packagekit")
        {
            Ok(trans) => trans,
            Err(err) => {
                ctx.emit(Error::Eyre(err, arg_span));
                return Err(());
            }
        };

        let resolved = match Self::resolve_packages(&trans, to_resolve)
            .wrap_err("Failed to resolve packages")
        {
            Ok(resolved) => resolved,
            Err(err) => {
                ctx.emit(Error::Eyre(err, packages.span));
                return Err(());
            }
        };

        for (pkg, _status) in resolved {
            match pkg_data.get_mut(&pkg.name) {
                Some((_span, count, id)) => {
                    *count += 1;
                    id.replace(pkg);
                }
                None => {
                    tracing::warn!("Packagekit returned an unregistered package: Ignoring")
                }
            }
        }

        let mut finished = Vec::new();
        for (name, (span, count, pkg)) in pkg_data {
            match count {
                0 => {
                    ctx.emit(Error::PackageNotRecognized(name, span));
                    degraded = true;
                }
                1 => {
                    let pkg = pkg.unwrap();
                    finished.push(Package::new(
                        pkg.into_id(),
                        vec![],
                        ctx.runtime.topic(),
                        ProviderId::PackageKit,
                    ))
                }
                _ => {
                    ctx.emit(Error::AmbiguousPackage(name, span));
                    degraded = true;
                }
            }
        }

        if !degraded {
            Ok(finished)
        } else {
            Err(())
        }
    }

    fn install(&mut self, packages: Vec<data::Package>, ctx: &mut OpCtx) {
        self.do_op(Operation::Install, packages, ctx)
    }

    fn remove(&mut self, packages: Vec<data::Package>, ctx: &mut OpCtx) {
        self.do_op(Operation::Remove, packages, ctx)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_pkit() -> color_eyre::Result<()> {
        let mut provider = PackageKit::new().unwrap();
        let trans = provider.new_transaction()?;
        let packages = vec!["fish", "git", "zsh", "qwertzyaruns"]
            .into_iter()
            .map(ToOwned::to_owned)
            .collect();
        let packages = PackageKit::resolve_packages(&trans, packages)?;
        dbg!(packages);
        Ok(())
    }
}
