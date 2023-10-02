mod capture;
mod deploy;
mod remove;

use std::{collections::HashMap, rc::Rc, time::Duration};

use clap::Subcommand;
use color_eyre::{
    eyre::WrapErr,
    owo_colors::{OwoColorize, Style},
};
use data::Topic;
use eval::IntoReport;
use indicatif::{ProgressBar, ProgressDrawTarget, ProgressStyle};
use interpreter::UnitPath;
use provider::{OpError, OpResult, RuntimeCtx};
use tracing::debug;

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
    pub fn run(self, context: &mut Context) -> CmdResult {
        let env = HashMap::new();

        let mut loader = match loader::Loader::new(env, context) {
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

        let style = ProgressStyle::with_template("Loading units: {wide_msg}{spinner}")
            .unwrap()
            .tick_strings(&[".", "..", "...", ""]);
        let pb =
            ProgressBar::with_draw_target(None, ProgressDrawTarget::stdout()).with_style(style);
        let pb_ref = &pb;
        pb.enable_steady_tick(Duration::from_millis(500));

        let errn = load_modules(
            &mut loader,
            &mut modules,
            Rc::new(move |msg| pb_ref.set_message(msg)),
            context,
        );
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
            Command::Deploy(deploy) => deploy.run(modules, root, context),
            Command::Remove(remove) => remove.run(modules, root, context),
            Command::Capture(capture) => capture.run(modules, root, context),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Status {
    Ready,
    Ok,
    Reverted,
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

fn load_modules(
    loader: &mut loader::Loader,
    modules: &mut HashMap<UnitId, Module>,
    set_msg: Rc<dyn Fn(String) + '_>,
    ctx: &mut Context,
) -> usize {
    let root = loader.root();
    let mut errn = 0;

    while let Some(module) = loader.load(set_msg.clone(), ctx) {
        let (unit, status) = match module.status {
            loader::Status::Ok(unit) => (Some(unit), Status::Ready),
            loader::Status::Degraded(_, errors, src) => {
                for err in errors {
                    errn += 1;
                    let filename = module.path.clone().unit_file().to_string();
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

/// Remove modules that are ready or deployed
fn remove_modules(
    root: UnitId,
    modules: &mut HashMap<UnitId, Module>,
    dry_run: bool,
    revert_all: bool,
    ctx: &mut Context,
) -> (Vec<(UnitId, OpResult)>, usize) {
    let len = modules
        .values()
        .filter(|m| {
            m.status == Status::Err
                || m.status == Status::Ready
                || m.status == Status::Ok && revert_all
        })
        .count();
    let mut errn = 0;
    let mut i = 1;
    let mut stack = vec![(vec![root], 0, None)];
    let mut removed = Vec::new();

    while let Some((members, current, _)) = stack.last_mut() {
        match members.get(*current) {
            Some(&id) => {
                let module = modules.get_mut(&id).unwrap();
                *current += 1;
                let members = module.members.clone();
                stack.push((members, 0, Some(id)));
            }
            None => {
                let (_, _, id) = stack.pop().unwrap();

                // Continue if we've reached the root
                let Some(id) = id else {
                    continue;
                };

                let module = modules.get_mut(&id).unwrap();

                // Remove if the deployment previously failed or the revert_all flag is set
                if !(module.status == Status::Err
                    || module.status == Status::Ready
                    || module.status == Status::Ok && revert_all)
                {
                    continue;
                }

                let style = pb_style(&module.path);
                let pb = ProgressBar::with_draw_target(None, ProgressDrawTarget::stdout());
                pb.set_style(style);
                pb.set_prefix(format!("[{}/{}]", i, len));
                pb.set_message("Starting removal");
                i += 1;

                if dry_run {
                    pb.finish_with_message("Skipped".bright_black().to_string());
                    module.status = Status::Skipped;
                } else {
                    let mut res = remove_unit(module, &pb, ctx);

                    if res.is_ok() {
                        pb.finish_with_message("Done".bright_green().to_string());
                        match module.status {
                            // In this case reversion is the primary operation so let's mark the status as success
                            Status::Ready => module.status = Status::Ok,
                            // Here we are reverting a module becaus something went wrong, so let's mark it as reverted
                            Status::Ok => module.status = Status::Reverted,
                            _ => (),
                        }
                    } else {
                        pb.finish_with_message("Error".red().to_string());
                        errn += 1;
                        module.status = Status::Err;

                        for err in res.drain_errors() {
                            print_error(&color_eyre::eyre::eyre!(err));
                        }
                    }

                    removed.push((id, res));
                }
            }
        }
    }
    (removed, errn)
}

fn remove_unit(module: &mut Module, pb: &ProgressBar, ctx: &mut Context) -> OpResult {
    debug!("Removing module {}", &module.path);
    let unit = module.unit.as_mut().expect("Unexpected error");
    let mut errors = Vec::new();

    if !unit.remove.is_empty() {
        let shell = &unit.shell.as_ref().unwrap_or(ctx.default_shell());
        let dir = module.path.bind(ctx.dotfile_dir().clone());

        for hook in &unit.remove {
            if let Err(err) = hook.run(shell, &dir) {
                let err = err.into_report().wrap_err("Uninstall hook failed");
                errors.push(OpError::Other(err));
            }
        }
    }

    let set_msg = Rc::new(|msg| pb.set_message(msg));
    let run_ctx = RuntimeCtx::new(
        set_msg,
        module.path.bind(ctx.dotfile_dir().clone()),
        unit.topic.as_ref().map(Topic::id),
        ctx.home_dir().map(ToOwned::to_owned),
        ctx.dotfile_dir().clone(),
        false,
    );

    let packages = std::mem::replace(&mut unit.packages, Vec::new());
    let mut res = ctx.interpreter().remove(packages, &run_ctx);
    for err in errors {
        res.push_err(err);
    }
    res
}

fn pb_style(path: &UnitPath) -> ProgressStyle {
    ProgressStyle::with_template(&format!("{{prefix:.bright.black}} {}: {{wide_msg}}", path))
        .unwrap()
}

fn print_error(err: &color_eyre::Report) {
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

    while !stack.is_empty() {
        let (members, n) = stack.last_mut().unwrap();
        match members.get(*n).copied() {
            Some(id) => {
                *n += 1;
                for (i, (members, n)) in stack.iter().enumerate() {
                    match (i, i + 1 == stack.len(), *n == members.len()) {
                        (0, _, _) => (),
                        (_, true, true) => print!("  └─ "),
                        (_, true, false) => print!("  ├─ "),
                        (_, false, false) => print!("  │ "),
                        (_, false, true) => print!("    "),
                    }
                }

                let module = modules.get(&id).unwrap();
                let name = module.path.name();
                let (col, status) = match &module.status {
                    Status::Ready => (Style::new().default_color(), ""),
                    Status::Ok => (Style::new().bright_green(), " (Ok)"),
                    Status::Reverted => (Style::new().bright_black(), " (Reverted)"),
                    Status::Err => (Style::new().red(), " (Error)"),
                    Status::Skipped => (Style::new().bright_black(), " (Skipped)"),
                };

                println!("{}{}", name.style(col), status.style(col));

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
