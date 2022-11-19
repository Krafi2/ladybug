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
    #[error("Provider is unavailable: {0}")]
    Unavailable(std::rc::Rc<dyn std::error::Error>),
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

pub struct Manager {
    cache: HashMap<ProviderId, Result<Box<dyn ProviderUpcast>, ProviderError>>,
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
    ) -> Result<&mut dyn ProviderUpcast, ProviderError> {
        fn new_provider<T: ProviderPrivate + 'static>(
            context: &mut Ctx,
        ) -> Result<Box<dyn ProviderUpcast>, ProviderError> {
            T::new(context.has_root()).map(|provider| Box::new(provider) as Box<dyn ProviderUpcast>)
        }

        let provider = self.cache.entry(id).or_insert_with(|| match id.0 {
            ProviderKind::Files => new_provider::<files::Provider>(context),
            ProviderKind::Flatpak => new_provider::<flatpak::Provider>(context),
            ProviderKind::Zypper => new_provider::<zypper::Provider>(context),
        });
        provider
            .as_mut()
            .map(|provider| provider.as_mut() as &mut dyn ProviderUpcast)
            .map_err(|err| err.clone())
    }

    pub fn get_provider(
        &mut self,
        id: ProviderId,
    ) -> Option<Result<&mut dyn Provider, ProviderError>> {
        self.cache.get_mut(&id).map(|res| {
            res.as_mut()
                .map(|provider| provider.as_provider())
                .map_err(|err| err.clone())
        })
    }

    fn get_transactor(
        &mut self,
        id: ProviderId,
        context: &mut Ctx,
    ) -> Result<&mut dyn Transactor, ProviderError> {
        self.get_raw_provider(id, context)
            .map(ProviderUpcast::as_new_transaction)
    }

    fn create_transaction(
        &mut self,
        id: Spanned<ProviderId>,
        args: Args,
        packages: Packages,
        context: &mut Ctx,
    ) -> Result<Transaction, Option<TransactionError>> {
        match self.get_transactor(id.inner, context) {
            Ok(transactor) => transactor
                .new_transaction(args, packages, context)
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
        let mut args_iter = args.args.iter();
        let provider = loop {
            match args_iter.next() {
                Some(arg) => {
                    if arg.name.0 == "provider" {
                        break &arg.value;
                    }
                }
                None => {
                    context.emit(TransactionError::Param(ParamError::NotFound {
                        span: args.span,
                        name: "provider",
                        has_args: args.accurate_span,
                    }));
                    return Err(());
                }
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
}

trait ProviderUpcast {
    fn as_new_transaction(&mut self) -> &mut dyn Transactor;
    fn as_provider(&mut self) -> &mut dyn Provider;
}

impl<P: ProviderPrivate> ProviderUpcast for P {
    fn as_new_transaction(&mut self) -> &mut dyn Transactor {
        self
    }

    fn as_provider(&mut self) -> &mut dyn Provider {
        self
    }
}

pub struct Transaction {
    provider: ProviderId,
    payload: Box<dyn Any>,
}

impl Transaction {
    pub fn provider(&self) -> ProviderId {
        self.provider
    }
}

impl std::fmt::Debug for Transaction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Transaction")
            .field("provider", &self.provider)
            .finish()
    }
}

trait ProviderPrivate: Provider + Transactor + Sized {
    fn new(root: bool) -> Result<Self, ProviderError>;
}

trait Transactor {
    /// Create a new transaction from the block definition.
    fn new_transaction(
        &mut self,
        args: Args,
        packages: Packages,
        context: &mut Ctx,
    ) -> Result<Transaction, ()>;
}

pub trait Provider {
    /// Install a collection of packages.
    fn install(&mut self, transaction: &Transaction) -> color_eyre::Result<()>;

    /// Remove a collection of packages from the system.
    fn remove(&mut self, transaction: &Transaction) -> color_eyre::Result<()>;
}
