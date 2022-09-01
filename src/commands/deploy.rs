use crate::unit::{Interpreter, Manager, Status};
use color_eyre::eyre::WrapErr;
use std::collections::{HashMap, VecDeque};

#[derive(clap::Parser)]
pub struct Deploy {
    #[clap(short, long)]
    dry_run: bool,
    #[clap(short, long)]
    topics: Option<Vec<String>>,
}

impl Deploy {
    // TODO: Improve error reporting
    pub(super) fn run(self, context: &crate::context::Context) -> super::CmdResult {
        let mut manager = Manager::new();
        let interpreter = Interpreter::new();
        let env = HashMap::new();
        let mut tree = crate::unit::ModuleTree::load(&interpreter, &mut manager, env, context);

        if tree.is_degraded() {
            return Err(color_eyre::eyre::eyre!("Deployment failed"));
        }

        let mut queue = VecDeque::from([tree.root()]);
        while let Some(id) = queue.pop_front() {
            let module = tree.get_mut(id);
            let status = std::mem::replace(
                &mut module.status,
                Status::Err(color_eyre::eyre::eyre!("Unit has been moved")),
            );
            match status {
                Status::Ok(unit) => {
                    for transaction in unit.transactions {
                        let provider = transaction.provider();
                        let provider = manager
                            .get_provider(provider, context)
                            .map_err(|err| color_eyre::eyre::eyre!(err.to_string()))
                            .wrap_err_with(|| format!("Failed to fetch provider '{}'", provider))?;
                        provider
                            .install(transaction)
                            .wrap_err("Transaction failed")?;
                    }
                    queue.extend(&module.members);
                }
                _ => unreachable!("The module tree wasn't supposed to be degraded"),
            }
        }

        Ok(())
    }
}
