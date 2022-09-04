pub struct Error;

pub struct Provider {}

impl super::Transactor for Provider {
    fn new_transaction(
        &mut self,
        args: crate::Args,
        packages: crate::Packages,
        emitter: &mut crate::error::Emitter,
        eval_ctx: &crate::EvalCtx,
        context: &crate::context::Context,
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
