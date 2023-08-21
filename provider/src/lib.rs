mod files;
mod flatpak;
mod privileged;
mod zypper;

use common::rel_path::RelPath;
use privileged::{Server, SuperCtx};

use ariadne::{Color, Fmt, ReportKind};
use eval::{report, Args, ConvertError, Value};
use parser::{Span, Spanned};

use std::{any::Any, collections::HashMap};

pub use privileged::detect_privileged;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
enum ProviderKind {
    Files,
    Flatpak,
    Zypper,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct ProviderId(ProviderKind);

impl From<ProviderKind> for ProviderId {
    fn from(kind: ProviderKind) -> Self {
        Self(kind)
    }
}

impl ProviderId {
    fn from_name(name: &str) -> Option<Self> {
        match name {
            "flatpak" => Some(ProviderKind::Flatpak),
            "zypper" => Some(ProviderKind::Zypper),
            _ => None,
        }
        .map(ProviderId)
    }
}

impl std::fmt::Display for ProviderId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self.0 {
            ProviderKind::Files => "files",
            ProviderKind::Flatpak => "flatpak",
            ProviderKind::Zypper => "zypper",
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

pub struct Package {
    pub name: Spanned<String>,
    pub args: Args,
    pub span: Span,
}

pub struct Packages {
    pub packages: Vec<Package>,
    pub span: Span,
}

#[derive(Debug)]
pub enum TransactionError {
    Parse(eval::ParamError),
    Provider(Span, ProviderId, ProviderError),
    Zypper(zypper::Error),
    Files(files::Error),
    Flatpak(flatpak::Error),
}

report! {
    TransactionError {
        TransactionError::Parse(err) => {
            delegate(err);
        }
        TransactionError::Provider(span, id, err) => {
            report(ReportKind::Error, span.start);
            message("Provider '{}' is not available", id.fg(Color::Red));
            label(span, Color::Red, "{err}");
        }
        TransactionError::Zypper(err) => {
            delegate(err);
        }
        TransactionError::Files(err) => {
            delegate(err);
        }
        TransactionError::Flatpak(err) => {
            delegate(err);
        }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("Unknown provider {found}")]
struct InvalidProvider {
    found: String,
}

type DynProvider = dyn Provider<Transaction = Box<dyn Any>, State = Box<dyn Any>>;

struct Factory {
    cache: HashMap<ProviderId, Result<Box<DynProvider>, ProviderError>>,
}

impl Factory {
    pub fn new() -> Self {
        Self {
            cache: HashMap::default(),
        }
    }

    fn get_raw_provider(&mut self, id: ProviderId) -> Result<&mut DynProvider, ProviderError> {
        fn new_provider<T: ConstructProvider + 'static>() -> Result<Box<DynProvider>, ProviderError>
        {
            T::new().map(|p| Box::new(ProviderAdapter(p)) as Box<DynProvider>)
        }

        let provider = self.cache.entry(id).or_insert_with(|| match id.0 {
            ProviderKind::Files => new_provider::<files::Provider>(),
            ProviderKind::Flatpak => new_provider::<flatpak::Provider>(),
            ProviderKind::Zypper => new_provider::<zypper::Provider>(),
        });

        provider
            .as_mut()
            .map(|provider| provider.as_mut())
            .map_err(|err| err.clone())
    }

    fn try_get_provider(
        &mut self,
        id: ProviderId,
    ) -> Option<Result<&mut DynProvider, ProviderError>> {
        self.cache
            .get_mut(&id)
            .map(|res| res.as_deref_mut().map_err(|err| err.clone()))
    }

    fn create_transaction(
        &mut self,
        id: Spanned<ProviderId>,
        args: Args,
        packages: Packages,
        ctx: &mut EvalCtx,
        exec: &ExecCtx,
    ) -> Result<Transaction, Option<TransactionError>> {
        match self.get_raw_provider(id.inner) {
            Ok(transactor) => transactor
                .new_transaction(args, packages, ctx, exec)
                .map(|payload| Transaction {
                    payload,
                    provider: id.inner,
                })
                .map_err(|_| None),
            Err(err) => Err(Some(TransactionError::Provider(id.span, id.inner, err))),
        }
    }
}

/// This struct initializes and holds providers
pub struct Manager {
    factory: Factory,
    server: Server,
}

impl Manager {
    pub fn new() -> Self {
        Self {
            factory: Factory::new(),
            server: Server::new(),
        }
    }

    pub fn new_transaction(
        &mut self,
        args: crate::Args,
        packages: Packages,
        ctx: &mut eval::Ctx,
        exec: &ExecCtx,
    ) -> Result<Transaction, ()> {
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

        match self.factory.create_transaction(
            Spanned::new(provider_id, provider.span),
            args,
            packages,
            &mut EvalCtx(ctx),
            exec,
        ) {
            Ok(ok) => Ok(ok),
            Err(err) => {
                if let Some(err) = err {
                    EvalCtx(ctx).emit(err);
                }
                Err(())
            }
        }
        // .map_err(|err| {
        //     if let Some(err) = err {
        //         ctx.emit(err);
        //     }
        // })
    }

    pub fn new_files(
        &mut self,
        ident_span: Span,
        args: crate::Args,
        packages: Packages,
        ctx: &mut eval::Ctx,
        exec: &ExecCtx,
    ) -> Result<Transaction, ()> {
        let mut ctx = EvalCtx(ctx);
        self.factory
            .create_transaction(
                Spanned::new(ProviderKind::Files.into(), ident_span),
                args,
                packages,
                &mut ctx,
                exec,
            )
            .map_err(|err| {
                if let Some(err) = err {
                    ctx.emit(err);
                }
            })
    }

    pub fn install(&mut self, transaction: &Transaction, ctx: &ExecCtx) -> (OpResult, State) {
        let provider = &mut self
            .factory
            .try_get_provider(transaction.provider)
            .expect("Unitialized provider")
            .expect("Provider not available");

        let (res, state) =
            provider.install(&transaction.payload, &mut self.server.super_ctx(), ctx);
        (
            res,
            State {
                payload: state,
                provider: transaction.provider,
            },
        )
    }

    pub fn remove(
        &mut self,
        transaction: &Transaction,
        state: Option<State>,
        ctx: &ExecCtx,
    ) -> OpResult {
        if let Some(state) = &state {
            assert_eq!(transaction.provider, state.provider);
        }

        let provider = &mut self
            .factory
            .try_get_provider(transaction.provider)
            .expect("Unitialized provider")
            .expect("Provider not available");

        provider.remove(
            &transaction.payload,
            state.map(|s| s.payload),
            &mut self.server.super_ctx(),
            ctx,
        )
    }
}

pub struct Transaction {
    provider: ProviderId,
    payload: Box<dyn Any>,
}

impl std::fmt::Debug for Transaction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Transaction")
            .field("provider", &self.provider)
            .finish_non_exhaustive()
    }
}

pub struct State {
    provider: ProviderId,
    payload: Box<dyn Any>,
}

impl std::fmt::Debug for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TransactionRes")
            .field("provider", &self.provider)
            .finish()
    }
}

/// Context for transaction execution
pub struct ExecCtx<'a> {
    set_msg: &'a dyn Fn(String),
    dir: RelPath,
}

impl<'a> ExecCtx<'a> {
    pub fn new(set_msg: &'a dyn Fn(String), dir: RelPath) -> Self {
        Self { set_msg, dir }
    }

    pub fn dir(&self) -> &RelPath {
        &self.dir
    }

    /// Set a progress  message
    fn set_message(&self, msg: String) {
        (self.set_msg)(msg)
    }
}

struct EvalCtx<'a>(&'a mut eval::Ctx);

impl<'a> EvalCtx<'a> {
    /// Emit a dynamic error
    fn emit<E: Into<Box<dyn eval::IntoReportBoxed + 'static>>>(&mut self, err: E) {
        let boxed = err.into();
        let err = eval::Error::from(boxed);
        self.0.emit(err);
    }
}

impl<'a> std::ops::Deref for EvalCtx<'a> {
    type Target = eval::Ctx;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'a> std::ops::DerefMut for EvalCtx<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}

pub type OpResult = color_eyre::Result<()>;

trait ConstructProvider: Sized + Provider {
    /// Create a new instance of the provider
    fn new() -> Result<Self, ProviderError>;
}

trait Provider {
    type Transaction;
    type State;

    /// Create a new transaction from the block definition.
    fn new_transaction(
        &mut self,
        args: Args,
        packages: Packages,
        eval: &mut EvalCtx,
        ctx: &ExecCtx,
    ) -> Result<Self::Transaction, ()>;

    /// Install a transaction
    fn install(
        &mut self,
        transaction: &Self::Transaction,
        sup: &mut SuperCtx,
        ctx: &ExecCtx,
    ) -> (OpResult, Self::State);

    /// Revert a transaction
    fn remove(
        &mut self,
        transaction: &Self::Transaction,
        state: Option<Self::State>,
        sup: &mut SuperCtx,
        ctx: &ExecCtx,
    ) -> OpResult;
}

struct ProviderAdapter<P>(P);

impl<P> Provider for ProviderAdapter<P>
where
    P: Provider,
    P::Transaction: 'static,
    P::State: 'static,
{
    type Transaction = Box<dyn Any>;
    type State = Box<dyn Any>;

    fn new_transaction(
        &mut self,
        args: Args,
        packages: Packages,
        ctx: &mut EvalCtx,
        exec: &ExecCtx,
    ) -> Result<Self::Transaction, ()> {
        P::new_transaction(&mut self.0, args, packages, ctx, exec)
            .map(|t| Box::new(t) as Box<dyn Any>)
    }

    fn install(
        &mut self,
        transaction: &Self::Transaction,
        sup: &mut SuperCtx,
        ctx: &ExecCtx,
    ) -> (OpResult, Self::State) {
        let (res, state) = P::install(
            &mut self.0,
            transaction.downcast_ref::<P::Transaction>().unwrap(),
            sup,
            ctx,
        );
        (res, Box::new(state))
    }

    fn remove(
        &mut self,
        transaction: &Self::Transaction,
        state: Option<Self::State>,
        sup: &mut SuperCtx,
        ctx: &ExecCtx,
    ) -> OpResult {
        P::remove(
            &mut self.0,
            transaction.downcast_ref::<P::Transaction>().unwrap(),
            state.map(|res| *res.downcast::<P::State>().unwrap()),
            sup,
            ctx,
        )
    }
}
