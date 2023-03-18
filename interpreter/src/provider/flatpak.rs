use parser::span::AriadneSpan;

use crate::error::IntoReport;

use super::ExecutionCtx;

#[derive(Debug)]
pub struct Error;

impl IntoReport for Error {
    fn into_report(self, _filename: &str) -> ariadne::Report<AriadneSpan> {
        todo!()
    }
}

pub struct Provider {}

impl super::ConstructProvider for Provider {
    fn new(_root: bool) -> Result<Self, super::ProviderError> {
        todo!()
    }
}

impl super::Provider for Provider {
    type Transaction = ();
    type State = ();

    fn new_transaction(
        &mut self,
        _args: crate::Args,
        _packages: crate::Packages,
        _ctx: &mut crate::eval::Ctx,
    ) -> Result<Self::Transaction, ()> {
        todo!()
    }

    fn install(
        &mut self,
        _transaction: &Self::Transaction,
        _ctx: ExecutionCtx,
    ) -> (super::OpResult, Self::State) {
        todo!()
    }

    fn remove(
        &mut self,
        _transaction: &Self::Transaction,
        _state: Option<Self::State>,
        _ctx: ExecutionCtx,
    ) -> super::OpResult {
        todo!()
    }
}
