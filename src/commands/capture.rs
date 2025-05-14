use std::collections::{HashMap, HashSet};

use color_eyre::{owo_colors::OwoColorize, Section};
use indicatif::{ProgressBar, ProgressDrawTarget};

use crate::{context::Context, unit::loader::UnitId};

use super::{Module, Status};

#[derive(clap::Parser, Debug)]
pub struct Capture {
    /// Run the command without making any changes to the system
    #[clap(long)]
    dry_run: bool,
    /// Topics to capture
    topics: Option<Vec<String>>,
}

impl Capture {
    pub(super) fn run(
        self,
        mut modules: HashMap<UnitId, Module>,
        root: UnitId,
        ctx: &Context,
    ) -> super::CmdResult {
        filter_modules(
            self.topics
                .as_ref()
                .map(|topics| HashSet::from_iter(topics.iter().map(String::as_str))),
            &mut modules,
        );

        let queue = generate_queue(root, &modules);
        if queue.is_empty() {
            if let Some(topics) = &self.topics {
                super::no_units_match(topics);
            }
        }
        println!("Capturing units:");
        let errn = self.capture_modules(queue, &mut modules, ctx);

        println!();
        // Print number of errors
        if errn != 0 {
            if errn == 1 {
                println!("\nCapture failed due to previous error");
            } else {
                println!("\nCapture failed due to {errn} previous errors");
            }
        }

        super::print_module_status(root, &modules, true);

        if errn == 0 {
            Ok(())
        } else {
            Err(())
        }
    }

    fn capture_modules(
        &self,
        queue: Vec<UnitId>,
        modules: &mut HashMap<UnitId, Module>,
        ctx: &Context,
    ) -> usize {
        let len = queue.len();
        let mut errn = 0;
        for (i, id) in queue.into_iter().enumerate() {
            let module = modules.get_mut(&id).unwrap();

            let style = super::pb_style(&module.path);
            let pb = ProgressBar::with_draw_target(None, ProgressDrawTarget::stdout());
            pb.set_style(style);
            pb.set_prefix(format!("[{}/{}]", i + 1, len));
            pb.set_message("Starting capture");

            if self.dry_run {
                pb.finish_with_message("Skipped".bright_black().to_string());
                module.status = Status::Skipped;
            } else {
                match capture_unit(module, &pb, ctx) {
                    Ok(_) => {
                        pb.finish_with_message("Done".bright_green().to_string());
                        module.status = Status::Ok
                    }
                    Err(err) => {
                        pb.finish_with_message("Error".red().to_string());
                        module.status = Status::Err;
                        super::print_error(&err);
                        errn += 1;
                    }
                }
            }
        }
        errn
    }
}

#[derive(thiserror::Error, Debug)]
#[error("Capture hook failed")]
struct CaptureError(#[source] common::command::Error);

fn capture_unit(module: &mut Module, pb: &ProgressBar, ctx: &Context) -> color_eyre::Result<()> {
    let unit = module.unit.as_ref().expect("Unexpected error");
    let shell = &unit.shell.as_ref().unwrap_or(ctx.default_shell());
    let dir = module.path.bind(ctx.dotfile_dir().clone());

    let mut res = Ok(());
    for (i, hook) in unit.capture.iter().enumerate() {
        pb.set_message(format!("Running hook {i}"));
        if let Err(err) = hook.run(shell, &dir) {
            res = res.error(CaptureError(err));
        }
    }
    res
}

fn filter_modules(topics: Option<HashSet<&str>>, modules: &mut HashMap<UnitId, Module>) {
    for module in modules.values_mut() {
        let unit = &module.unit.as_ref().unwrap();
        let capture = match (
            topics.as_ref(),
            module.unit.as_ref().and_then(|unit| unit.topic.as_ref()),
        ) {
            (None, _) => true,
            // Capture if topics match
            (Some(topics), Some(topic)) => topics.contains(topic.name()),
            // Ignore otherwise
            (Some(_), None) => false,
        };

        if !capture || unit.capture.is_empty() {
            module.status = Status::Skipped;
        }
    }
}

fn generate_queue(root: UnitId, modules: &HashMap<UnitId, Module>) -> Vec<UnitId> {
    let mut queue = Vec::new();
    let mut stack = vec![vec![root]];
    while let Some(members) = stack.last_mut() {
        match members.pop() {
            Some(id) => {
                let module = &modules[&id];
                if let Status::Ready = module.status {
                    queue.push(id);
                }
                let mut members = module.members.clone();
                members.reverse();
                stack.push(members);
            }
            None => {
                stack.pop();
            }
        }
    }
    queue
}
