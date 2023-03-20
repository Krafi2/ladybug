use ariadne::{Color, Fmt, ReportKind};
use parser::Spanned;

use crate::{structures::ConvertError, Value};

use super::{structures::ParamError, Args, Ctx, Packages, Span};
use std::{any::Any, collections::HashMap};

mod files;
mod flatpak;
mod zypper;

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
    // The provider requires root privileges to function
    #[error("Provider requires root permissions")]
    NeedRoot,
}

#[derive(Debug)]
pub(super) enum TransactionError {
    Param(ParamError),
    Provider(Span, ProviderId, ProviderError),
    Zypper(zypper::Error),
    Files(files::Error),
    Flatpak(flatpak::Error),
}

report! {
    TransactionError {
        TransactionError::Param(err) => {
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

/// This struct initializes and holds providers
pub struct Manager {
    cache: HashMap<ProviderId, Result<Box<DynProvider>, ProviderError>>,
}

impl Manager {
    pub fn new() -> Self {
        Self {
            cache: HashMap::default(),
        }
    }

    fn get_raw_provider(
        &mut self,
        id: ProviderId,
        context: &mut Ctx,
    ) -> Result<&mut DynProvider, ProviderError> {
        fn new_provider<T: ConstructProvider + 'static>(
            context: &mut Ctx,
        ) -> Result<Box<DynProvider>, ProviderError> {
            T::new(context.has_root()).map(|p| Box::new(ProviderAdapter(p)) as Box<DynProvider>)
        }

        let provider = self.cache.entry(id).or_insert_with(|| match id.0 {
            ProviderKind::Files => new_provider::<files::Provider>(context),
            ProviderKind::Flatpak => new_provider::<flatpak::Provider>(context),
            ProviderKind::Zypper => new_provider::<zypper::Provider>(context),
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
        context: &mut Ctx,
    ) -> Result<Transaction, Option<TransactionError>> {
        match self.get_raw_provider(id.inner, context) {
            Ok(transactor) => transactor
                .new_transaction(args, packages, context)
                .map(|payload| Transaction {
                    payload,
                    provider: id.inner,
                })
                .map_err(|_| None),
            Err(err) => Err(Some(TransactionError::Provider(id.span, id.inner, err))),
        }
    }

    pub(super) fn new_transaction(
        &mut self,
        args: crate::Args,
        packages: Packages,
        context: &mut Ctx,
    ) -> Result<Transaction, ()> {
        let args_span = args.span;
        let accurate_span = args.accurate_span;
        let mut provider = None;

        // Find the provider and remove the `provider` argument
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
                context.emit(TransactionError::Param(ParamError::NotFound {
                    span: args_span,
                    name: "provider",
                    has_args: accurate_span,
                }));
                return Err(());
            }
        };

        let provider_name = match &provider.inner {
            Value::String(s) => s,
            Value::Error(err) => {
                context.emit(ConvertError::EvalErr {
                    span: provider.span,
                    err: err.clone(),
                });
                return Err(());
            }
            other => {
                context.emit(ConvertError::TypeErr {
                    span: provider.span,
                    expected: super::Type::String,
                    found: other.get_type(),
                });
                return Err(());
            }
        };

        let provider_id = match ProviderId::from_name(&provider_name) {
            Some(id) => id,
            None => {
                context.emit(ConvertError::ValueErr {
                    span: provider.span,
                    err: Box::new(InvalidProvider {
                        found: provider_name.to_owned(),
                    }),
                });
                return Err(());
            }
        };

        self.create_transaction(
            Spanned::new(provider_id, provider.span),
            args,
            packages,
            context,
        )
        .map_err(|err| {
            if let Some(err) = err {
                context.emit(err);
            }
        })
    }

    pub(super) fn new_files(
        &mut self,
        ident_span: Span,
        args: crate::Args,
        packages: Packages,
        context: &mut Ctx,
    ) -> Result<Transaction, ()> {
        self.create_transaction(
            Spanned::new(ProviderKind::Files.into(), ident_span),
            args,
            packages,
            context,
        )
        .map_err(|err| {
            if let Some(err) = err {
                context.emit(err);
            }
        })
    }

    pub fn install(&mut self, transaction: &Transaction, ctx: ExecutionCtx) -> (OpResult, State) {
        let (res, state) = self
            .try_get_provider(transaction.provider)
            .expect("Unitialized provider")
            .expect("Provider not available")
            .install(&transaction.payload, ctx);
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
        ctx: ExecutionCtx,
    ) -> OpResult {
        if let Some(state) = &state {
            assert_eq!(transaction.provider, state.provider);
        }

        self.try_get_provider(transaction.provider)
            .expect("Unitialized provider")
            .expect("Provider not available")
            .remove(&transaction.payload, state.map(|s| s.payload), ctx)
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
            .finish()
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

/// The provider context wraps the interpreter context with additional features
/// realted to the exucution of transactions
pub struct ExecutionCtx<'a> {
    set_msg: Box<dyn FnMut(&str) + 'a>,
}

impl<'a> ExecutionCtx<'a> {
    pub fn new(set_msg: Box<dyn FnMut(&str) + 'a>) -> Self {
        Self { set_msg }
    }

    fn set_message(&mut self, msg: &str) {
        (self.set_msg)(msg)
    }
}

pub type OpResult = color_eyre::Result<()>;

trait ConstructProvider: Sized + Provider {
    /// Create a new instance of the provider
    fn new(root: bool) -> Result<Self, ProviderError>;
}

trait Provider {
    type Transaction;
    type State;

    /// Create a new transaction from the block definition.
    fn new_transaction(
        &mut self,
        args: Args,
        packages: Packages,
        ctx: &mut Ctx,
    ) -> Result<Self::Transaction, ()>;

    /// Install a collection of packages.
    fn install(
        &mut self,
        transaction: &Self::Transaction,
        ctx: ExecutionCtx,
    ) -> (OpResult, Self::State);

    /// Remove a collection of packages from the system.
    fn remove(
        &mut self,
        transaction: &Self::Transaction,
        state: Option<Self::State>,
        ctx: ExecutionCtx,
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
        ctx: &mut Ctx,
    ) -> Result<Self::Transaction, ()> {
        P::new_transaction(&mut self.0, args, packages, ctx).map(|t| Box::new(t) as Box<dyn Any>)
    }

    fn install(
        &mut self,
        transaction: &Self::Transaction,
        ctx: ExecutionCtx,
    ) -> (OpResult, Self::State) {
        let (res, state) = P::install(
            &mut self.0,
            transaction.downcast_ref::<P::Transaction>().unwrap(),
            ctx,
        );
        (res, Box::new(state))
    }

    fn remove(
        &mut self,
        transaction: &Self::Transaction,
        state: Option<Self::State>,
        ctx: ExecutionCtx,
    ) -> OpResult {
        P::remove(
            &mut self.0,
            transaction.downcast_ref::<P::Transaction>().unwrap(),
            state.map(|res| *res.downcast::<P::State>().unwrap()),
            ctx,
        )
    }
}
