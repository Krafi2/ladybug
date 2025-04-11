use color_eyre::owo_colors::OwoColorize;
use data::Topic;
use indicatif::{ProgressBar, ProgressDrawTarget};
use provider::{OpError, OpResult, RuntimeCtx};
use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};
use tracing::debug;

use crate::{commands::Status, context::Context, unit::loader::UnitId};

use super::{pb_style, print_error, Module};

#[derive(clap::Parser, Debug)]
pub struct Deploy {
    /// Revert modules which couldn't be deployed
    #[clap(long)]
    revert: bool,
    /// If a unit fails, revert all changes made to the system
    #[clap(long)]
    revert_all: bool,
    /// Run the command without making any changes to the system
    #[clap(long)]
    dry_run: bool,
    /// Don't clean up unused packages
    #[clap(long)]
    no_clean: bool,
    /// Abort immediately upon encountering an error
    #[clap(long)]
    abort_early: bool,
    /// Topics to deploy
    topics: Option<Vec<String>>,
}

impl Deploy {
    pub(super) fn run(
        self,
        mut modules: HashMap<UnitId, Module>,
        root: UnitId,
        ctx: &mut Context,
    ) -> super::CmdResult {
        let mut errn = 0;

        println!("Deploying units:");
        let topics = self.topics.as_ref().map(|topics| {
            let (topics, errs) = super::register_topics(topics, ctx);
            errn += errs;
            topics
        });

        if errn == 0 {
            // Filter modules based on user query
            if let Some(topics) = &topics {
                filter_modules(topics, root, &mut modules);
            }

            // Report bad user query
            if !modules.values().any(|m| m.status == Status::Ready) {
                if let Some(topics) = &self.topics {
                    super::no_units_match(topics);
                }
            }

            // Deploy requested modules
            let (deployed, errs) = self.deploy_modules(root, &mut modules, ctx);
            errn += errs;

            let mut ids = Vec::new();
            for package in deployed
                .into_iter()
                .flat_map(|(_id, package)| package.completed)
            {
                match ctx.database().installed(&package) {
                    Ok(id) => ids.push(id),
                    Err(err) => tracing::error!("Failed to update package database: {err}"),
                }
            }

            // Clean old deployment
            if errn == 0 && !self.no_clean {
                println!("\n\nCleaning up old packages:");
                super::clean_deployed(ids, topics, ctx);
            }

            // Revert changes if the deployment failed
            if errn > 0 && (self.revert || self.revert_all) {
                println!("\n\nEncountered an error, reverting changes:");
                let (removed, errs) =
                    super::remove_modules(root, &mut modules, self.dry_run, self.revert_all, ctx);

                for package in removed
                    .into_iter()
                    .flat_map(|(_id, package)| package.completed)
                {
                    if let Err(err) = ctx.database().removed(&package) {
                        tracing::error!("Failed to update package database: {err:#}");
                    }
                }
                errn += errs;
            }
        }

        // Report errn
        println!();
        if errn != 0 {
            if errn == 1 {
                println!("\nDeployment failed due to previous error");
            } else {
                println!("\nDeployment failed due to {errn} previous errors");
            }
        };

        // Print status of units
        super::print_module_status(root, &modules, true);

        if errn == 0 {
            Ok(())
        } else {
            Err(())
        }
    }

    /// Deploy these `UnitId`s, return deployed ids and the number of errors
    fn deploy_modules(
        &self,
        root: UnitId,
        modules: &mut HashMap<UnitId, Module>,
        context: &mut Context,
    ) -> (Vec<(UnitId, OpResult)>, usize) {
        let queue_len = modules
            .values()
            .filter(|m| m.status == Status::Ready)
            .count();

        let mut processed = 0;
        let mut deployed = Vec::new();
        let mut errn = 0;
        // (children, current, skip)
        let mut stack = vec![(vec![root], 0, false)];

        while let Some((members, current, skip)) = stack.last_mut() {
            match members.get(*current) {
                Some(id) => {
                    let mut skip = *skip;
                    let module = modules.get_mut(id).unwrap();

                    // Apply skip flag transitively
                    if skip {
                        module.status = Status::Skipped;
                    }

                    // We are processing only skipped and ready packages
                    if let Status::Ready | Status::Skipped = module.status {
                        processed += 1;
                    }

                    if module.status == Status::Ready {
                        let style = pb_style(&module.path);
                        let pb = ProgressBar::with_draw_target(None, ProgressDrawTarget::stdout());
                        pb.set_style(style);
                        pb.set_prefix(format!("[{}/{}]", processed, queue_len));
                        pb.set_message("Starting deployment");

                        if self.dry_run {
                            pb.finish_with_message("Skipped".bright_black().to_string());
                            module.status = Status::Skipped;
                        } else {
                            // Deploy unit
                            let mut res = self.deploy_unit(module, &pb, context);

                            let is_ok = res.is_ok();

                            if is_ok {
                                pb.finish_with_message("Done".bright_green().to_string());
                                module.status = Status::Ok;
                                deployed.push((*id, res));
                            } else {
                                errn += 1;
                                pb.finish_with_message("Error".red().to_string());
                                module.status = Status::Err;

                                for err in res.drain_errors() {
                                    print_error(&color_eyre::eyre::eyre!(err));
                                }
                                deployed.push((*id, res));

                                if self.abort_early {
                                    break;
                                } else {
                                    // Skip this module's children
                                    skip = true;
                                }
                            }
                        }
                    }

                    *current += 1;
                    stack.push((module.members.clone(), 0, skip))
                }
                None => {
                    stack.pop();
                }
            }
        }

        (deployed, errn)
    }

    fn deploy_unit(&self, module: &mut Module, pb: &ProgressBar, ctx: &mut Context) -> OpResult {
        debug!("Deploying module {}", &module.path);
        let unit = module.unit.as_mut().expect("Unexpected error");
        let set_msg = Rc::new(|msg| pb.set_message(msg));
        let runtime = RuntimeCtx::new(
            set_msg,
            module.path.bind(ctx.dotfile_dir().clone()),
            unit.topic.as_ref().map(Topic::id),
            ctx.home_dir().map(ToOwned::to_owned),
            ctx.dotfile_dir().clone(),
            self.abort_early,
        );

        // Install packages
        let packages = std::mem::replace(&mut unit.packages, Vec::new());
        let mut res = ctx.interpreter().install(packages, &runtime);

        // Run deploy hooks
        if res.is_ok() {
            let shell = &unit.shell.as_ref().unwrap_or(ctx.default_shell());
            let dir = module.path.bind(ctx.dotfile_dir().clone());

            for hook in &unit.deploy {
                if let Err(err) = hook.run(shell, &dir) {
                    let err = err.into_report().wrap_err("Deploy hook failed");
                    res.push_err(OpError::Other(err));
                    break;
                }
            }
        }
        res
    }
}

/// Filter modules based on a set of topics. Member units depend on their
/// parents, so we need to propagate deployment up the tree.
fn filter_modules(topics: &HashSet<Topic>, root: UnitId, modules: &mut HashMap<UnitId, Module>) {
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
                let module = modules.get_mut(id).unwrap();
                let enabled = match &module.unit.as_ref().unwrap().topic {
                    // Enable if topics match
                    Some(topic) => topics.contains(&topic),
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
