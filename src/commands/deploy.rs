use color_eyre::eyre::WrapErr;
use interpreter::{provider::Manager, Interpreter};
use std::collections::{HashMap, VecDeque};

use crate::unit::Status;

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

        let is_degraded = tree.is_degraded();
        if is_degraded {
            eprintln!("Deployment failed!");
            // return Err(color_eyre::eyre::eyre!("Deployment failed"));
        }

        let mut queue = VecDeque::from([tree.root()]);
        while let Some(id) = queue.pop_front() {
            let module = tree.get_mut(id);
            let status = std::mem::replace(
                &mut module.status,
                Status::Err(color_eyre::eyre::eyre!(
                    "Unit has been moved! This is a bug!"
                )),
            );
            match status {
                Status::Ok(unit) if !is_degraded => {
                    for transaction in unit.transactions {
                        let provider = transaction.provider();
                        let provider = manager
                            .get_provider(provider)
                            .expect("Unitialized provider")
                            .map_err(|err| color_eyre::eyre::eyre!(err.to_string()))
                            .wrap_err_with(|| format!("Failed to fetch provider '{}'", provider))?;
                        provider
                            .install(transaction)
                            .wrap_err("Transaction failed")?;
                    }
                    queue.extend(&module.members);
                }
                Status::Err(err) => {
                    assert!(is_degraded);
                    let err = err.wrap_err(format!("Failed to deploy unit {}", &module.path));
                    eprintln!("{:?}", err);
                }
                Status::Degraded(_, errors, src) => {
                    assert!(is_degraded);
                    for err in errors {
                        let filename = &module.path.to_string();
                        let report = err.into_report(&filename);
                        let res = report
                            .eprint::<(&str, ariadne::Source)>((
                                &filename,
                                ariadne::Source::from(&src),
                            ))
                            .wrap_err("Failed to print message");

                        if let Err(err) = res {
                            tracing::warn!("{:?}", err);
                        }
                    }
                    eprintln!()
                }
                _ => (),
            }
        }

        Ok(Ok(()))
    }
}
