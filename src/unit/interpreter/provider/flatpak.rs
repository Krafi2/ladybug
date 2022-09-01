pub struct Provider {}

impl super::NewTransaction for Provider {
    fn new_transaction(
        &mut self,
        _args: crate::unit::interpreter::Args,
        _packages: crate::unit::interpreter::Packages,
        _context: &crate::context::Context,
        _emitter: &mut crate::unit::interpreter::Emitter,
    ) -> Result<super::Transaction, ()> {
        todo!()
    }
}

impl super::Provider for Provider {
    fn install(&mut self, _transaction: super::Transaction) -> color_eyre::Result<()> {
        todo!()
    }

    fn remove(&mut self, _transaction: super::Transaction) -> color_eyre::Result<()> {
        todo!()
    }
}

impl super::ProviderPrivate for Provider {
    fn new(_context: &crate::context::Context) -> Result<Self, super::ProviderError> {
        todo!()
    }
}
