mod files;
mod flatpak;
mod privileged;
mod zypper;

use color_eyre::eyre::eyre;
use common::rel_path::RelPath;
use data::{Package, TopicId};
use privileged::SuperCtx;

use ariadne::{Color, Fmt, ReportKind};
use eval::{report, Args, ConvertError, Value};
use parser::{Span, Spanned};

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    rc::Rc,
};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
enum ProviderId {
    Files,
    Flatpak,
    Zypper,
}

impl ProviderId {
    fn from_name(name: &str) -> Option<Self> {
        match name {
            "flatpak" => Some(ProviderId::Flatpak),
            "zypper" => Some(ProviderId::Zypper),
            _ => None,
        }
    }
}

impl From<ProviderId> for data::ProviderId {
    fn from(value: ProviderId) -> Self {
        let i = match value {
            ProviderId::Files => 0,
            ProviderId::Flatpak => 1,
            ProviderId::Zypper => 2,
        };
        data::ProviderId::new(i)
    }
}

impl TryFrom<data::ProviderId> for ProviderId {
    type Error = data::ProviderId;

    fn try_from(value: data::ProviderId) -> Result<Self, Self::Error> {
        match value.inner() {
            0 => Ok(ProviderId::Files),
            1 => Ok(ProviderId::Flatpak),
            2 => Ok(ProviderId::Zypper),
            _ => Err(value),
        }
    }
}

impl std::fmt::Display for ProviderId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            ProviderId::Files => "files",
            ProviderId::Flatpak => "flatpak",
            ProviderId::Zypper => "zypper",
        };
        f.write_str(s)
    }
}

#[derive(Clone, Debug, thiserror::Error)]
pub enum ProviderError {
    // Provider is unavailable
    #[error("{0:#}")]
    Unavailable(std::rc::Rc<color_eyre::Report>),
}

/// Raw unparsed package
pub struct RawPackage {
    pub name: Spanned<String>,
    pub args: Args,
    pub span: Span,
}

/// Raf unparsed packages
pub struct RawPackages {
    pub packages: Vec<RawPackage>,
    pub span: Span,
}

/// Error during package definition parsing
#[derive(Debug)]
enum ParseError {
    Provider(ProviderId, ProviderError, Span),
    Bincode(bincode::Error, Span),
}

report! {
    ParseError {
        ParseError::Provider(id, err, span) => {
            report(ReportKind::Error, span.start);
            message("Provider '{}' is not available", id.fg(Color::Red));
            label(span, Color::Red, "{err}");
        }
        ParseError::Bincode(err, span) => {
            report(ReportKind::Error, span.start);
            message("Unexpected bincode error");
            label(span, Color::Red, "{err}");
        }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("Unknown provider {found}")]
struct InvalidProvider {
    found: String,
}

struct Factory {
    cache: HashMap<ProviderId, Result<Box<dyn Provider>, ProviderError>>,
}

impl Factory {
    pub fn new() -> Self {
        Self {
            cache: HashMap::default(),
        }
    }

    fn get_provider(
        &mut self,
        id: ProviderId,
    ) -> Result<&mut (dyn Provider + 'static), ProviderError> {
        let provider = self.cache.entry(id).or_insert_with(|| match id {
            ProviderId::Files => files::new_provider().map(|p| Box::new(p) as _),
            ProviderId::Flatpak => flatpak::new_provider().map(|p| Box::new(p) as _),
            ProviderId::Zypper => zypper::new_provider().map(|p| Box::new(p) as _),
        });

        provider.as_deref_mut().map_err(|err| err.clone())
    }

    fn try_get_provider(
        &mut self,
        id: ProviderId,
    ) -> Option<Result<&mut (dyn Provider + 'static), ProviderError>> {
        self.cache
            .get_mut(&id)
            .map(|res| res.as_deref_mut().map_err(|err| err.clone()))
    }

    fn parse_packages(
        &mut self,
        id: Spanned<ProviderId>,
        args: Args,
        packages: RawPackages,
        ctx: &mut ParseCtx,
    ) -> Result<Vec<Package>, Option<ParseError>> {
        match self.get_provider(id.inner) {
            Ok(transactor) => transactor
                .parse_packages(args, packages, ctx)
                .map_err(|_| None),
            Err(err) => Err(Some(ParseError::Provider(id.inner, err, id.span))),
        }
    }
}

/// Runtime data for providers. Manages individual providers and the privileged
/// interface.
pub struct Runtime {
    factory: Factory,
    super_ctx: SuperCtx,
}

impl Runtime {
    pub fn new() -> Self {
        Self {
            factory: Factory::new(),
            super_ctx: SuperCtx::new(),
        }
    }

    pub fn parse_packages(
        &mut self,
        args: crate::Args,
        packages: RawPackages,
        ctx: &mut eval::Ctx,
        exec: &RuntimeCtx,
    ) -> Result<Vec<Package>, ()> {
        let args_span = args.span;
        let accurate_span = args.accurate_span;
        let mut provider = None;

        // Find the provider and remove it from the argument list
        let args = args
            .args
            .into_iter()
            .filter_map(|arg| {
                if arg.name.0 == "provider" {
                    provider = Some(arg.value);
                    None
                } else {
                    Some(arg)
                }
            })
            .collect();
        let args = Args {
            args,
            span: args_span,
            accurate_span,
        };

        let provider = match provider {
            Some(provider) => provider,
            None => {
                ctx.emit(eval::ParamError::NotFound {
                    span: args_span,
                    name: "provider",
                    has_args: accurate_span,
                });
                return Err(());
            }
        };

        let provider_name = match &provider.inner {
            Value::String(s) => s,
            Value::Error(err) => {
                ctx.emit(ConvertError::EvalErr {
                    span: provider.span,
                    err: err.clone(),
                });
                return Err(());
            }
            other => {
                ctx.emit(ConvertError::TypeErr {
                    span: provider.span,
                    expected: eval::Type::String,
                    found: other.get_type(),
                });
                return Err(());
            }
        };

        let provider_id = match ProviderId::from_name(provider_name) {
            Some(id) => id,
            None => {
                ctx.emit(ConvertError::ValueErr {
                    span: provider.span,
                    err: Box::new(InvalidProvider {
                        found: provider_name.to_owned(),
                    }),
                });
                return Err(());
            }
        };

        let mut ctx = ParseCtx::new(exec, ctx);
        match self.factory.parse_packages(
            Spanned::new(provider_id, provider.span),
            args,
            packages,
            &mut ctx,
        ) {
            Ok(ok) => Ok(ok),
            Err(err) => {
                if let Some(err) = err {
                    ctx.emit(err);
                }
                Err(())
            }
        }
    }

    pub fn new_files(
        &mut self,
        ident_span: Span,
        args: crate::Args,
        packages: RawPackages,
        ctx: &mut eval::Ctx,
        exec: &RuntimeCtx,
    ) -> Result<Vec<Package>, ()> {
        let mut ctx = ParseCtx::new(exec, ctx);
        self.factory
            .parse_packages(
                Spanned::new(ProviderId::Files.into(), ident_span),
                args,
                packages,
                &mut ctx,
            )
            .map_err(|err| {
                if let Some(err) = err {
                    ctx.emit(err);
                }
            })
    }

    pub fn install(&mut self, packages: Vec<Package>, ctx: &RuntimeCtx) -> OpResult {
        let mut ctx = OpCtx::new(ctx, &mut self.super_ctx);
        let sorted = sort_packages(packages, &mut ctx);

        for (provider, packages) in sorted {
            let provider = &mut self
                .factory
                .try_get_provider(provider)
                .expect("Unitialized provider")
                .expect("Provider not available");

            provider.install(packages, &mut ctx);
        }
        ctx.into()
    }

    /// Remove these packages. All packages must belong to the same provider.
    pub fn remove(&mut self, packages: Vec<data::Package>, ctx: &RuntimeCtx) -> OpResult {
        let mut ctx = OpCtx::new(ctx, &mut self.super_ctx);
        let sorted = sort_packages(packages, &mut ctx);

        for (provider, packages) in sorted {
            let provider = &mut self
                .factory
                .try_get_provider(provider)
                .expect("Unitialized provider")
                .expect("Provider not available");

            provider.remove(packages, &mut ctx);
        }
        ctx.into()
    }
}

fn sort_packages(packages: Vec<Package>, ctx: &mut OpCtx) -> HashMap<ProviderId, Vec<Package>> {
    let mut providers = HashMap::new();
    for package in packages {
        let id = match ProviderId::try_from(package.provider()) {
            Ok(id) => id,
            Err(id) => {
                let err = eyre!("provider id '{}' doesn't match any known value", id.inner())
                    .wrap_err(format!("While evaluating package '{}'", package.name()));
                tracing::error!("{err:#}");
                ctx.emit(err);
                ctx.failed(package);
                continue;
            }
        };
        providers.entry(id).or_insert_with(Vec::new).push(package);
    }
    providers
}

pub struct OpResult {
    pub completed: Vec<Package>,
    pub failed: Vec<Package>,
    pub errors: Result<(), Vec<OpError>>,
}

impl OpResult {
    pub fn is_err(&self) -> bool {
        self.errors.is_err()
    }

    pub fn is_ok(&self) -> bool {
        self.errors.is_ok()
    }

    pub fn errn(&self) -> usize {
        match &self.errors {
            Ok(_) => 0,
            Err(errors) => errors.len(),
        }
    }

    pub fn push_err(&mut self, err: OpError) {
        match &mut self.errors {
            errs @ Ok(_) => *errs = Err(vec![err]),
            Err(errs) => errs.push(err),
        }
    }

    pub fn drain_errors(&mut self) -> impl Iterator<Item = OpError> {
        let errors = match &mut self.errors {
            Ok(_) => vec![],
            Err(errors) => std::mem::replace(errors, vec![]),
        };
        errors.into_iter()
    }
}

trait Provider {
    /// Parse package definitions
    fn parse_packages(
        &mut self,
        args: Args,
        packages: RawPackages,
        ctx: &mut ParseCtx,
    ) -> Result<Vec<data::Package>, ()>;

    /// Install packages
    fn install(&mut self, packages: Vec<data::Package>, ctx: &mut OpCtx);

    /// Revert intallation of packages
    fn remove(&mut self, packages: Vec<data::Package>, ctx: &mut OpCtx);
}

struct ParseCtx<'a> {
    runtime: &'a RuntimeCtx<'a>,
    eval: &'a mut eval::Ctx,
}

impl<'a> ParseCtx<'a> {
    fn new(runtime: &'a RuntimeCtx, eval: &'a mut eval::Ctx) -> Self {
        Self { runtime, eval }
    }

    fn runtime(&self) -> &RuntimeCtx {
        &self.runtime
    }

    fn eval(&mut self) -> &mut eval::Ctx {
        &mut self.eval
    }

    /// Emit a dynamic error
    fn emit<E: Into<Box<dyn eval::IntoReportBoxed + 'static>>>(&mut self, err: E) {
        let boxed = err.into();
        let err = eval::Error::from(boxed);
        self.eval.emit(err);
    }
}

/// Context for transaction execution
pub struct RuntimeCtx<'a> {
    set_msg: Rc<dyn Fn(String) + 'a>,
    dir: RelPath,
    topic: Option<TopicId>,

    home_dir: Result<PathBuf, common::rel_path::HomeError>,
    dotfile_dir: RelPath,
    abort_early: bool,
}

impl<'a> RuntimeCtx<'a> {
    pub fn new(
        set_msg: Rc<dyn Fn(String) + 'a>,
        dir: RelPath,
        topic: Option<TopicId>,
        home_dir: Result<PathBuf, common::rel_path::HomeError>,
        dotfile_dir: RelPath,
        abort_early: bool,
    ) -> Self {
        Self {
            set_msg,
            dir,
            topic,
            home_dir,
            dotfile_dir,
            abort_early,
        }
    }

    pub fn dir(&self) -> &RelPath {
        &self.dir
    }

    pub fn dotfile_dir(&self) -> &RelPath {
        &self.dotfile_dir
    }

    /// Set a progress message
    pub fn set_message(&self, msg: String) {
        (self.set_msg)(msg)
    }

    pub fn topic(&self) -> Option<TopicId> {
        self.topic.as_ref().copied()
    }

    pub fn home_dir(&self) -> Result<&Path, common::rel_path::HomeError> {
        self.home_dir.as_deref().map_err(Clone::clone)
    }

    pub fn abort_early(&self) -> bool {
        self.abort_early
    }
}

struct OpCtx<'a> {
    runtime: &'a RuntimeCtx<'a>,
    sup: &'a mut SuperCtx,
    installed: Vec<Package>,
    failed: Vec<Package>,
    errors: Vec<OpError>,
}

impl<'a> OpCtx<'a> {
    pub fn new(runtime: &'a RuntimeCtx, sup: &'a mut SuperCtx) -> Self {
        Self {
            runtime,
            sup,
            installed: Vec::new(),
            failed: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn runtime(&self) -> &RuntimeCtx {
        &self.runtime
    }

    pub fn sup(&mut self) -> &mut SuperCtx {
        &mut self.sup
    }

    /// Mark a package as installed
    pub fn installed(&mut self, pack: Package) {
        self.installed.push(pack)
    }

    /// Mark a package as failed
    pub fn failed(&mut self, pack: Package) {
        self.failed.push(pack)
    }

    pub fn emit<T: Into<OpError>>(&mut self, err: T) {
        self.errors.push(err.into())
    }
}

impl<'a> From<OpCtx<'a>> for OpResult {
    fn from(value: OpCtx) -> Self {
        Self {
            completed: value.installed,
            failed: value.failed,
            errors: if value.errors.is_empty() {
                Ok(())
            } else {
                Err(value.errors)
            },
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum OpError {
    /// Corrupted metadata
    #[error("corrupted metadata")]
    Metadata(#[from] bincode::Error),
    /// Other error
    #[error(transparent)]
    Other(#[from] color_eyre::Report),
}

impl From<common::rel_path::Error> for OpError {
    fn from(value: common::rel_path::Error) -> Self {
        Self::Other(color_eyre::eyre::eyre!(value))
    }
}
