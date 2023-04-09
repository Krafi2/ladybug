mod capture;
mod deploy;
mod remove;

use std::{collections::HashMap, time::Duration};

use clap::Subcommand;
use color_eyre::{
    eyre::WrapErr,
    owo_colors::{OwoColorize, Style},
};
use indicatif::{ProgressBar, ProgressDrawTarget, ProgressStyle};
use interpreter::{
    provider::{ExecutionCtx, Manager},
    UnitPath,
};

use crate::{
    context::Context,
    unit::{
        loader::{self, UnitId},
        Unit,
    },
};

#[derive(Subcommand, Debug)]
pub enum Command {
    Deploy(deploy::Deploy),
    Remove(remove::Remove),
    Capture(capture::Capture),
}

type CmdResult = Result<(), ()>;

impl Command {
    pub fn run(self, context: &Context) -> CmdResult {
        let mut manager = Manager::new();
        let env = HashMap::new();

        let loader = match loader::Loader::new(env, context.interpreter(), &mut manager, context) {
            Ok(loader) => loader,
            Err(err) => {
                match err {
                    loader::LoaderError::DotfileDirError(err) => eprintln!(
                        "The dotfile directory at '{}' is inaccessible: {}",
                        context.dotfile_dir(),
                        err
                    ),
                    loader::LoaderError::DotfileDirMissing => {
                        eprintln!(
                            "The dotfile directory at '{}' doesn't exist",
                            context.dotfile_dir()
                        )
                    }
                }
                println!("\nOperation aborted due to previous error");
                return Err(());
            }
        };

        let root = loader.root();
        let mut modules = HashMap::new();

        let style = ProgressStyle::with_template("Loading units{spinner}")
            .unwrap()
            .tick_strings(&[".", "..", "...", ""]);
        let pb =
            ProgressBar::with_draw_target(None, ProgressDrawTarget::stdout()).with_style(style);
        pb.enable_steady_tick(Duration::from_millis(500));

        let errn = load_modules(loader, &mut modules);
        pb.finish_and_clear();

        if errn != 0 {
            if errn == 1 {
                println!("\nOperation aborted due to previous error")
            } else if errn > 1 {
                println!("\nOperation aborted due to previous {errn} errors")
            }
            print_module_status(root, &modules);
            return Err(());
        }

        match self {
            Command::Deploy(deploy) => deploy.run(manager, modules, root, context),
            Command::Remove(remove) => remove.run(manager, modules, root, context),
            Command::Capture(capture) => capture.run(manager, modules, root, context),
        }
    }
}

#[derive(Debug)]
enum Status {
    Ready,
    Ok,
    Err,
    Skipped,
}

#[derive(Debug)]
struct Module {
    path: UnitPath,
    unit: Option<Unit>,
    members: Vec<UnitId>,
    status: Status,
}

fn load_modules(loader: loader::Loader, modules: &mut HashMap<UnitId, Module>) -> usize {
    let root = loader.root();
    let mut errn = 0;

    for module in loader {
        let (unit, status) = match module.status {
            loader::Status::Ok(unit) => (Some(unit), Status::Ready),
            loader::Status::Degraded(_, errors, src) => {
                for err in errors {
                    errn += 1;
                    let filename = &module.path.clone().unit_file().to_string();
                    let report = err.into_report(&filename);
                    let res = report
                        .eprint::<(&str, ariadne::Source)>((&filename, ariadne::Source::from(&src)))
                        .wrap_err("Failed to print message");

                    if let Err(err) = res {
                        tracing::warn!("{:?}", err);
                    }
                }
                (None, Status::Err)
            }
            loader::Status::Err(err) => {
                // Non-root modules should have already reported their errors
                // Existence of member units is checked when validating the parent declaration
                if module.id == root {
                    errn += 1;
                    eprintln!("Cannot load root module:");
                    match err {
                        loader::Error::IO(io) => {
                            eprintln!(
                                "    File {} not found: {}",
                                module.path.clone().unit_file(),
                                io
                            )
                        }
                    }
                }
                (None, Status::Err)
            }
        };

        modules.insert(
            module.id,
            Module {
                path: module.path,
                unit,
                members: module.members,
                status,
            },
        );
    }
    errn
}

fn remove_modules(
    deployed: Vec<(UnitId, Vec<Option<interpreter::provider::State>>)>,
    modules: &mut HashMap<UnitId, Module>,
    dry_run: bool,
    manager: &mut Manager,
    context: &crate::context::Context,
) -> usize {
    let mut errn = 0;
    let len = deployed.len();

    for (i, (id, states)) in deployed.into_iter().enumerate() {
        let module = modules.get_mut(&id).unwrap();

        let style = pb_style(&module.path);
        let pb = ProgressBar::with_draw_target(None, ProgressDrawTarget::stdout());
        pb.set_style(style);
        pb.set_prefix(format!("[{}/{}]", i + 1, len));
        pb.set_message("Starting removal");

        if dry_run {
            pb.finish_with_message("Skipped".bright_black().to_string());
            module.status = Status::Skipped;
        } else {
            let errors = remove_unit(module, states, &pb, manager, context);
            errn += errors.len();
            if errors.is_empty() {
                pb.finish_with_message("Done".bright_green().to_string());
                // Change the status if it hasn't been modified
                if let Status::Ready = module.status {
                    module.status = Status::Ok;
                }
            } else {
                pb.finish_with_message("Error".red().to_string());
                module.status = Status::Err;
                for err in errors {
                    print_error(err);
                }
            }
        }
    }
    errn
}

fn remove_unit(
    module: &Module,
    states: Vec<Option<interpreter::provider::State>>,
    pb: &ProgressBar,
    manager: &mut Manager,
    context: &crate::context::Context,
) -> Vec<color_eyre::Report> {
    let unit = module.unit.as_ref().expect("Unexpected error");
    let mut errors = Vec::new();

    for (transaction, state) in unit.transactions.iter().zip(states) {
        let ctx = ExecutionCtx::new(Box::new(|msg| pb.set_message(msg.to_owned())));
        if let Err(err) = manager.remove(transaction, state, ctx) {
            errors.push(err);
        }
    }

    if !unit.remove.is_empty() {
        let shell = &unit.shell.as_ref().unwrap_or(context.default_shell());
        let dir = module.path.bind(context.dotfile_dir().clone());

        for hook in &unit.remove {
            if let Err(err) = hook.run(shell, &dir) {
                errors.push(err.into_report().wrap_err("Uninstall hook failed"));
            }
        }
    }
    errors
}

fn pb_style(path: &UnitPath) -> ProgressStyle {
    let style =
        ProgressStyle::with_template(&format!("{{prefix:.bright.black}} {}: {{wide_msg}}", path))
            .unwrap();
    style
}

fn print_error(err: color_eyre::Report) {
    let msg = format!("Caused by:{err:?}");
    // color-eyre indents using 3 spaces
    let msg = indent::indent_all_by(3, msg);
    eprintln!("{msg}");
}

fn no_units_match(topics: &[String]) {
    eprintln!(
        "No units match the {} {}",
        if topics.len() == 1 { "topic" } else { "topics" },
        if topics.len() == 1 {
            topics.last().unwrap().to_owned()
        } else {
            topics.join(", ")
        }
    )
}

fn print_module_status(root: UnitId, modules: &HashMap<UnitId, Module>) {
    println!("\nUnit status:");
    let mut stack = vec![(vec![root], 0)];

    while let Some((members, n)) = stack.last_mut() {
        match members.get(*n) {
            Some(id) => {
                *n += 1;
                let module = modules.get(&id).unwrap();
                let last = members.len() == *n;
                let depth = stack.len() - 1;

                for _ in 0..depth {
                    print!("  ");
                }

                let part = if depth == 0 {
                    ""
                } else if last {
                    "└─ "
                } else {
                    "├─ "
                };
                let name = module.path.name();
                let (col, status) = match &module.status {
                    Status::Ready => (Style::new().default_color(), ""),
                    Status::Ok => (Style::new().bright_green(), " (Ok)"),
                    Status::Err => (Style::new().red(), " (Error)"),
                    Status::Skipped => (Style::new().bright_black(), " (Skipped)"),
                };

                println!("{part}{}{}", name.style(col), status.style(col));

                if !module.members.is_empty() {
                    stack.push((module.members.clone(), 0));
                }
            }
            None => {
                stack.pop();
            }
        }
    }
}
