pub struct Provider {}

impl super::NewTransaction for Provider {
    fn new_transaction(
        &mut self,
        args: crate::topic::interpreter::Args,
        packages: crate::topic::interpreter::Packages,
        context: &crate::context::Context,
        emitter: &mut crate::topic::interpreter::Emitter,
    ) -> Result<super::Transaction, ()> {
        todo!()
    }
}

impl super::Provider for Provider {
    fn install(&mut self, transaction: super::Transaction) -> color_eyre::Result<()> {
        todo!()
    }

    fn remove(&mut self, transaction: super::Transaction) -> color_eyre::Result<()> {
        todo!()
    }
}

impl super::ProviderPrivate for Provider {
    fn new(context: &crate::context::Context) -> Result<Self, super::ProviderError> {
        todo!()
    }
}
