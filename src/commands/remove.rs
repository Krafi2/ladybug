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
        let queue = generate_queue(root, &modules)
            .into_iter()
            .map(|id| {
                (
                    id,
                    // Create an empty state to go with each transaction
                    modules[&id]
                        .unit
                        .as_ref()
                        .unwrap()
                        .transactions
                        .iter()
                        .map(|_| None)
                        .collect(),
                )
            })
            .collect::<Vec<_>>();

        if queue.is_empty() {
            if let Some(topics) = &self.topics {
                super::no_units_match(topics)
            }
        }

        let errn = super::remove_modules(queue, &mut modules, self.dry_run, ctx);

        println!("");
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
                        Some(topic) => topics.contains(topic.as_str()),
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

/// Generate a queue of units to remove so that no units are orphaned
fn generate_queue(root: UnitId, modules: &HashMap<UnitId, Module>) -> Vec<UnitId> {
    let mut queue = Vec::new();
    let mut stack = vec![(vec![root], 0)];
    while let Some((members, current)) = stack.last_mut() {
        match members.get(*current) {
            Some(id) => {
                let module = &modules[id];
                let mut members = module.members.clone();
                // Remove in reverse order in case the user relies on the units
                // being deployed in order of declaration
                members.reverse();
                stack.push((members, 0));
            }
            None => {
                let _ = stack.pop();
                if let Some(frame) = stack.last_mut() {
                    let parent = frame.0[frame.1];
                    if let Status::Ready = modules[&parent].status {
                        queue.push(parent);
                    }
                    frame.1 += 1;
                }
            }
        }
    }
    queue
}
