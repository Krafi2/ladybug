use parser::span::AriadneSpan;

use crate::error::IntoReport;

#[derive(Debug)]
pub struct Error;

impl IntoReport for Error {
    fn into_report(self, _filename: &str) -> ariadne::Report<AriadneSpan> {
        todo!()
    }
}

pub struct Provider {}

impl super::Transactor for Provider {
    fn new_transaction(
        &mut self,
        _args: crate::Args,
        _packages: crate::Packages,
        _context: &mut crate::eval::Ctx,
    ) -> Result<super::Transaction, ()> {
        todo!()
    }
}

impl super::Provider for Provider {
    fn install(&mut self, _transaction: &super::Transaction) -> color_eyre::Result<()> {
        todo!()
    }

    fn remove(&mut self, _transaction: &super::Transaction) -> color_eyre::Result<()> {
        todo!()
    }
}

impl super::ProviderPrivate for Provider {
    fn new(_root: bool) -> Result<Self, super::ProviderError> {
        todo!()
    }
}
