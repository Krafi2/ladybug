use std::collections::{HashMap, HashSet};

use crate::unit::loader::UnitId;

use super::{Module, Status};

#[derive(clap::Parser, Debug)]
pub struct Remove {
    #[clap(long)]
    dry_run: bool,
    topics: Option<Vec<String>>,
}

impl Remove {
    pub(super) fn run(
        self,
        mut modules: HashMap<UnitId, Module>,
        root: UnitId,
        ctx: &mut crate::context::Context,
    ) -> super::CmdResult {
        println!("Removing units:");
        if let Some(topics) = self.topics.as_ref() {
            let topics = HashSet::from_iter(topics.iter().map(String::as_str));
            filter_modules(topics, root, &mut modules);
        }

        // Report bad user query
        if !modules.values().any(|m| m.status == Status::Ready) {
            if let Some(topics) = &self.topics {
                super::no_units_match(topics)
            }
        }

        let (removed, errn) = super::remove_modules(root, &mut modules, self.dry_run, false, ctx);

        for package in removed
            .into_iter()
            .flat_map(|(_id, package)| package.completed)
        {
            if let Err(err) = ctx.database().removed(&package) {
                tracing::error!("Failed to update package database: {err:#}");
            }
        }

        println!();
        // Print number of errors
        if errn != 0 {
            if errn == 1 {
                println!("\nRemoval failed due to previous error");
            } else {
                println!("\nRemoval failed due to {errn} previous errors");
            }
        }

        super::print_module_status(root, &modules);

        if errn == 0 {
            Ok(())
        } else {
            Err(())
        }
    }
}

/// Filter modules for removal based on a set of topics. Member units depend on
/// their parents, so we need to propagate removal down the tree.
fn filter_modules(topics: HashSet<&str>, root: UnitId, modules: &mut HashMap<UnitId, Module>) {
    let mut stack = vec![(vec![root], false)];
    while let Some((members, remove_parent)) = stack.last_mut() {
        match members.pop() {
            Some(id) => {
                let module = modules.get_mut(&id).unwrap();
                let remove = *remove_parent
                    || match &module.unit.as_ref().unwrap().topic {
                        // Remove if topics match
                        Some(topic) => topics.contains(topic.name()),
                        // Keep otherwise
                        None => false,
                    };
                // Skip units that we don't want to remove
                if !remove {
                    module.status = Status::Skipped;
                }
                stack.push((module.members.clone(), remove));
            }
            None => {
                stack.pop();
            }
        }
    }
}
