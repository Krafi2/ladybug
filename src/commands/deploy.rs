use color_eyre::owo_colors::OwoColorize;
use indicatif::{ProgressBar, ProgressDrawTarget};
use interpreter::provider::{ExecutionCtx, Manager};
use std::{
    collections::{HashMap, HashSet},
    iter::FromIterator,
};
use tracing::debug;

use crate::{commands::Status, unit::loader::UnitId};

use super::{pb_style, print_error, Module};

#[derive(clap::Parser, Debug)]
pub struct Deploy {
    /// If a unit fails, revert all changes made to the system
    #[clap(long)]
    revert_all: bool,
    /// Run the command without making any changes to the system
    #[clap(long)]
    dry_run: bool,
    /// Abort immediately upon encountering an error
    #[clap(long)]
    abort_early: bool,
    /// Topics to deploy
    topics: Option<Vec<String>>,
}

impl Deploy {
    pub(super) fn run(
        self,
        mut manager: Manager,
        mut modules: HashMap<UnitId, Module>,
        root: UnitId,
        context: &crate::context::Context,
    ) -> super::CmdResult {
        let mut errn = 0;

        println!("Deploying units:");
        if let Some(topics) = self.topics.as_ref() {
            let topics = HashSet::from_iter(topics.iter().map(String::as_str));
            filter_modules(topics, root, &mut modules);
        }
        let queue = generate_queue(root, &modules);

        if queue.is_empty() {
            if let Some(topics) = &self.topics {
                super::no_units_match(topics)
            }
        }

        let deployed = self.deploy_modules(queue, &mut modules, &mut manager, context);

        errn += deployed
            .iter()
            .map(|(_, _, is_err)| *is_err as usize)
            .sum::<usize>();

        // Revert changes if the deployment failed
        if errn > 0 {
            let to_remove = deployed
                .into_iter()
                .filter_map(|(id, states, is_err)| {
                    (is_err || self.revert_all)
                        .then(|| (id, states.into_iter().map(Some).collect()))
                })
                .collect();
            println!("\n\nEncountered an error, reverting changes:");
            errn +=
                super::remove_modules(to_remove, &mut modules, self.dry_run, &mut manager, context);
        }

        println!("");
        if errn != 0 {
            if errn == 1 {
                println!("\nDeployment failed due to previous error");
            } else {
                println!("\nDeployment failed due to {errn} previous errors");
            }
        };

        // Print status of units
        super::print_module_status(root, &modules);

        if errn == 0 {
            Ok(())
        } else {
            Err(())
        }
    }

    fn deploy_modules(
        &self,
        queue: Vec<UnitId>,
        modules: &mut HashMap<UnitId, Module>,
        manager: &mut Manager,
        context: &crate::context::Context,
    ) -> Vec<(UnitId, Vec<interpreter::provider::State>, bool)> {
        let mut deployed = Vec::new();
        let queue_len = queue.len();
        for (i, id) in queue.into_iter().enumerate() {
            let module = modules.get_mut(&id).unwrap();

            let style = pb_style(&module.path);
            let pb = ProgressBar::with_draw_target(None, ProgressDrawTarget::stdout());
            pb.set_style(style);
            pb.set_prefix(format!("[{}/{}]", i + 1, queue_len));
            pb.set_message("Starting deployment");

            if self.dry_run {
                pb.finish_with_message("Skipped".bright_black().to_string());
                module.status = Status::Skipped;
            } else {
                let (res, states) = deploy_unit(module, &pb, manager, context);
                let is_err = res.is_err();
                deployed.push((id, states, is_err));
                if let Err(err) = res {
                    pb.finish_with_message("Error".red().to_string());
                    module.status = Status::Err;
                    print_error(err);
                } else {
                    pb.finish_with_message("Done".bright_green().to_string());
                    module.status = Status::Ok;
                }
                if is_err && self.abort_early {
                    break;
                }
            }
        }
        deployed
    }
}

/// Filter modules based on a set of topics. Member units depend on their
/// parents, so we need to propagate deployment up the tree.
fn filter_modules(topics: HashSet<&str>, root: UnitId, modules: &mut HashMap<UnitId, Module>) {
    struct Frame {
        members: Vec<UnitId>,
        current: usize,
        enabled: bool,
    }

    let mut stack = vec![Frame {
        members: vec![root],
        current: 0,
        enabled: false,
    }];

    while let Some(Frame {
        members,
        current,
        enabled: _,
    }) = stack.last_mut()
    {
        match members.get(*current) {
            Some(id) => {
                let module = modules.get_mut(&id).unwrap();
                let enabled = match &module.unit.as_ref().unwrap().topic {
                    // Enable if topics match
                    Some(topic) => topics.contains(topic.as_str()),
                    // Disable if the user requested topics but this unit has none
                    None => false,
                };

                // The status of this unit will be set when this frame is popped
                stack.push(Frame {
                    members: module.members.clone(),
                    current: 0,
                    enabled,
                });
            }
            None => {
                let old = stack.pop().unwrap();
                if let Some(frame) = stack.last_mut() {
                    if !old.enabled {
                        let current = frame.members[frame.current];
                        modules.get_mut(&current).unwrap().status = Status::Skipped;
                    }
                    frame.enabled = frame.enabled || old.enabled;
                    frame.current += 1;
                }
            }
        }
    }
}

/// Generate a queue of units to deploy so that no units are orphaned
fn generate_queue(root: UnitId, modules: &HashMap<UnitId, Module>) -> Vec<UnitId> {
    let mut queue = Vec::new();
    let mut stack = vec![(vec![root], 0)];
    while let Some((members, current)) = stack.last_mut() {
        match members.get(*current) {
            Some(id) => {
                let module = &modules[&id];
                if let Status::Ready = module.status {
                    queue.push(*id);
                }
                *current += 1;
                stack.push((module.members.clone(), 0))
            }
            None => {
                stack.pop();
            }
        }
    }
    queue
}

fn deploy_unit(
    module: &mut Module,
    pb: &ProgressBar,
    manager: &mut Manager,
    ctx: &crate::context::Context,
) -> (
    Result<(), color_eyre::Report>,
    Vec<interpreter::provider::State>,
) {
    debug!("Deploying module {}", &module.path);
    let unit = module.unit.as_ref().expect("Unexpected error");
    let mut states = Vec::new();

    let res = unit
        .transactions
        .iter()
        .try_for_each(|transaction| {
            let ctx = ExecutionCtx::new(Box::new(|msg| pb.set_message(msg.to_owned())));
            let (res, state) = manager.install(transaction, ctx);
            states.push(state);
            res
        })
        .and_then(|_| {
            let shell = &unit.shell.as_ref().unwrap_or(ctx.default_shell());
            let dir = module.path.bind(ctx.dotfile_dir().clone());
            unit.deploy.iter().try_for_each(|hook| {
                hook.run(shell, &dir)
                    .map_err(|err| err.into_report().wrap_err("Deploy hook failed"))
            })
        });

    module.status = if res.is_err() {
        Status::Err
    } else {
        Status::Ok
    };

    (res, states)
}
