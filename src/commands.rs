mod capture;
mod deploy;
mod remove;

use std::{
    cell::Cell,
    collections::{HashMap, HashSet},
    rc::Rc,
    time::Duration,
};

use clap::Subcommand;
use color_eyre::{
    eyre::WrapErr,
    owo_colors::{OwoColorize, Style},
};
use data::{PackageId, Topic};
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
            true, // TODO: Add some way to configure this with a flag
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
            print_module_status(root, &modules, true);
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
    Disabled,
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
    silence_disabled: bool, // Silence errors from disabled units except parsing errors
    set_msg: Rc<dyn Fn(String) + '_>,
    ctx: &mut Context,
) -> usize {
    let root = loader.root();
    let mut errn = 0;

    while let Some(module) = loader.load(set_msg.clone(), ctx) {
        let (unit, status) = match module.status {
            loader::Status::Ok(unit) => {
                let disabled = unit.disabled;
                (
                    Some(unit),
                    if disabled {
                        Status::Disabled
                    } else {
                        Status::Ready
                    },
                )
            }
            loader::Status::Degraded(figment, errors, src) => {
                let mut report_err = false;
                for err in errors {
                    // Silence more advanced errors if the caller asks us to
                    if figment.disabled && silence_disabled && err.kind() != eval::ErrorKind::Parse
                    {
                        continue;
                    }

                    report_err = true;
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

                match (figment.disabled, report_err) {
                    (true, false) => (None, Status::Disabled),
                    _ => (None, Status::Err),
                }
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

fn clean_deployed(
    deployed: Vec<PackageId>,
    topics: Option<HashSet<Topic>>,
    ctx: &mut Context,
) -> usize {
    let mut errn = 0;
    let cached = topics
        .as_ref()
        .map(|topics| {
            topics
                .iter()
                .filter_map(|topic| {
                    ctx.database()
                        .get_topic(topic.id())
                        .map_err(|err| {
                            tracing::error!(
                                "Failed to get packages belonging to topic '{topic}': {err:#}"
                            );
                            errn += 1;
                        })
                        .ok()
                })
                .flatten()
                .collect()
        })
        .unwrap_or_else(|| {
            ctx.database()
                .get_all()
                .map_err(|err| {
                    tracing::error!("Failed to get packages: {err:#}");
                    errn += 1;
                })
                .unwrap_or_default()
        });

    let queue = generate_clean_queue(deployed, cached);

    let i = Cell::new(1);
    let len = queue.len();
    let style =
        ProgressStyle::with_template("{prefix:.bright.black} Cleaning up packages: {wide_msg}}")
            .unwrap();
    let pb = ProgressBar::with_draw_target(None, ProgressDrawTarget::stdout());
    pb.set_style(style);
    pb.set_message("Starting removal");
    // TODO: `i` can be greater than len if a package sets multiple messages
    let set_msg = Rc::new(|msg| {
        pb.set_message(msg);
        let val = i.take();
        pb.set_prefix(format!("[{val}/{len}]",));
        i.replace(val + 1);
    });
    // TODO: remove some of this contect from RuntimeCtx
    let run_ctx = RuntimeCtx::new(
        set_msg,
        // TODO: remove
        ctx.dotfile_dir().clone(),
        // TODO: remove
        None,
        ctx.home_dir().map(ToOwned::to_owned),
        ctx.dotfile_dir().clone(),
        false,
    );
    let res = ctx.interpreter().remove(queue, &run_ctx);
    for package in res.completed {
        if let Err(err) = ctx.database().removed(&package) {
            tracing::error!("Failed to update package database: {err:#}");
        }
    }

    errn
}

fn generate_clean_queue(
    mut deployed: Vec<data::PackageId>,
    mut cached: Vec<(data::PackageId, data::Package)>,
) -> Vec<data::Package> {
    deployed.sort_unstable();
    cached.sort_unstable_by_key(|(id, _pacakage)| *id);

    let mut cached = cached.drain(..);
    let mut deployed = deployed.drain(..);
    let mut deployed_cur = deployed.next();
    let mut cached_cur = cached.next();
    let mut queue = Vec::new();

    // Find elements that are in `cached` but not `deployed`
    loop {
        match (cached_cur.take(), deployed_cur.take()) {
            (Some((_, package)), None) => {
                queue.push(package);
                cached_cur = cached.next();
            }
            (Some((cached_id, package)), Some(deployed_id)) => {
                // Ids match, continue
                if cached_id == deployed_id {
                    cached_cur = cached.next();
                    deployed_cur = deployed.next();
                // The cached id is not deployed
                } else if deployed_id > cached_id {
                    queue.push(package);
                    cached_cur = cached.next();
                    deployed_cur = Some(deployed_id);
                // The deployed id is not cached
                } else {
                    cached_cur = Some((cached_id, package));
                    deployed_cur = deployed.next();
                }
            }
            _ => break,
        }
    }
    queue
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

fn print_module_status(root: UnitId, modules: &HashMap<UnitId, Module>, filter_disabled: bool) {
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
                    Status::Disabled => (Style::new().black(), " (Disabled)"),
                };

                println!("{}{}", name.style(col), status.style(col));

                // Filter out disabled modules
                let members = module
                    .members
                    .iter()
                    .cloned()
                    .filter(|memb| {
                        filter_disabled
                            && modules
                                .get(memb)
                                .is_some_and(|unit| unit.status != Status::Disabled)
                    })
                    .collect::<Vec<_>>();

                if !members.is_empty() {
                    stack.push((members, 0));
                }
            }
            None => {
                stack.pop();
            }
        }
    }
}

fn register_topics(topics: &[String], ctx: &mut Context) -> (HashSet<Topic>, usize) {
    let mut errn = 0;
    let topics = HashSet::from_iter(topics.iter().filter_map(|topic| {
        ctx.database()
            .new_topic(topic.clone())
            .map_err(|err| {
                eprintln!("Failed to register topic '{topic}': {err:#}");
                errn += 1;
            })
            .ok()
    }));
    (topics, errn)
}
