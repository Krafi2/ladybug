use color_eyre::{
    eyre::WrapErr,
    owo_colors::{OwoColorize, Style},
};
use interpreter::{provider::Manager, UnitPath};
use std::{
    collections::{HashMap, HashSet},
    io::Write,
    iter::FromIterator,
};

use crate::unit::{
    loader::{self, UnitId},
    Unit,
};

#[derive(clap::Parser, Debug)]
pub struct Deploy {
    /// If a unit fails, revert all changes made to the system
    #[clap(long)]
    revert_all: bool,
    /// Run the command without making any changes to the system
    #[clap(long)]
    dry_run: bool,
    /// Exit immediately upon encountering an error
    #[clap(long)]
    exit_early: bool,
    /// Topics to deploy
    topics: Option<Vec<String>>,
}

#[derive(Debug)]
enum Status {
    Undeployed,
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

impl Deploy {
    pub(super) fn run(self, context: &crate::context::Context) -> super::CmdResult {
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
                failure_message(1, true);
                return Ok(Err(()));
            }
        };

        let root = loader.root();
        let mut modules = HashMap::new();

        let mut errn = load_modules(loader, &mut modules);
        let parse_failed = errn != 0;

        // Continue if there were no errors
        if errn == 0 {
            println!("Deploying units:");
            let mut deployed = self.deploy_modules(root, &mut modules, &mut manager, context);
            errn += deployed
                .iter()
                .map(|(_, _, is_err)| *is_err as usize)
                .sum::<usize>();

            // Revert changes if the deployment failed
            if errn > 0 {
                let to_remove = deployed
                    .into_iter()
                    .filter_map(|(id, states, is_err)| {
                        (is_err || self.revert_all).then_some((id, states))
                    })
                    .collect();
                println!("");
                println!("Encountered an error, reverting changes:");
                errn += remove_modules(to_remove, &modules, &mut manager, context);
            }
        }

        failure_message(errn, parse_failed);

        // Print status of units
        println!("\nUnit status:");
        let mut stdout = std::io::stdout().lock();
        write_tree(&mut stdout, root, modules).unwrap();

        Ok(Ok(()))
    }

    fn deploy_modules(
        &self,
        root: UnitId,
        modules: &mut HashMap<UnitId, Module>,
        manager: &mut Manager,
        context: &crate::context::Context,
    ) -> Vec<(UnitId, Vec<interpreter::provider::State>, bool)> {
        if let Some(topics) = self.topics.as_ref() {
            let topics = HashSet::from_iter(topics.iter().map(String::as_str));
            filter_modules(topics, root, modules);
        }
        let queue = generate_queue(root, modules);

        if queue.is_empty() {
            if let Some(mut topics) = self.topics.clone() {
                eprintln!(
                    "No units match the {} {}",
                    if topics.len() == 1 { "topic" } else { "topics" },
                    if topics.len() == 1 {
                        topics.pop().unwrap()
                    } else {
                        topics.join(", ")
                    }
                )
            }
        }

        let mut deployed = Vec::new();
        let queue_len = queue.len();

        for (i, id) in queue.into_iter().enumerate() {
            let module = modules.get_mut(&id).unwrap();
            print!("[{}/{}] Deploying unit {}:", i + 1, queue_len, &module.path);
            // Manually flush stdout to make sure that the user sees the message
            flush_stdout();

            if self.dry_run {
                println!(" Skipped");
            } else {
                let (res, states) = deploy_unit(module, manager, context);
                let is_err = res.is_err();
                deployed.push((id, states, is_err));
                if let Err(err) = res {
                    println!(" Error");
                    eprintln!("Error:{err:?}");
                } else {
                    println!(" Done");
                }
                if is_err && self.exit_early {
                    break;
                }
            }
        }
        deployed
    }
}

fn flush_stdout() {
    if let Err(err) = std::io::stdout().flush() {
        tracing::warn!(?err)
    }
}

/// Filter modules based on the requested topics
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
fn generate_queue(root: UnitId, modules: &mut HashMap<UnitId, Module>) -> Vec<UnitId> {
    let mut queue = Vec::new();
    let mut stack = vec![(vec![root], 0)];

    while let Some((members, current)) = stack.last_mut() {
        match members.get(*current) {
            Some(id) => {
                let module = &modules[&id];
                if let Status::Undeployed = module.status {
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

fn failure_message(errn: usize, aborted: bool) {
    // Print number of errors
    if errn != 0 {
        let verb = if aborted { "aborted" } else { "failed" };
        if errn == 1 {
            println!("\nDeployment {verb} due to previous error");
        } else {
            println!("\nDeployment {verb} due to {errn} previous errors");
        }
    }
}

fn remove_modules(
    deployed: Vec<(UnitId, Vec<interpreter::provider::State>)>,
    modules: &HashMap<UnitId, Module>,
    manager: &mut Manager,
    context: &crate::context::Context,
) -> usize {
    let mut errn = 0;
    let len = deployed.len();
    for (i, (id, states)) in deployed.into_iter().enumerate() {
        let module = modules.get(&id).unwrap();
        print!("[{}/{}] Removing unit {}:", i + 1, len, module.path);
        flush_stdout();
        let errors = remove_unit(module, states, manager, context);
        if errors == 0 {
            println!(" Done")
        }
        errn += errors;
    }
    errn
}

fn load_modules(loader: loader::Loader, modules: &mut HashMap<UnitId, Module>) -> usize {
    let root = loader.root();
    let mut errn = 0;

    for module in loader {
        let (unit, status) = match module.status {
            loader::Status::Ok(unit) => (Some(unit), Status::Undeployed),
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
                if module.id == root {
                    errn += 1;
                    eprintln!("Cannot load root module:");
                    match err {
                        loader::Error::IO(io) => {
                            eprintln!("    File {} not found: {}", module.path, io)
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

fn remove_unit(
    module: &Module,
    states: Vec<interpreter::provider::State>,
    manager: &mut Manager,
    context: &crate::context::Context,
) -> usize {
    let mut errn = 0;
    let unit = module.unit.as_ref().expect("Unexpected error");

    for (transaction, state) in unit.transactions.iter().zip(states) {
        if let Err(err) = manager.remove(transaction, Some(state)) {
            if errn == 0 {
                println!(" Error");
            }
            eprintln!("Error:{err:?}");
            errn += 1;
        }
    }

    if !unit.remove.is_empty() {
        let shell = &unit.shell.as_ref().unwrap_or(context.default_shell());
        let dir = module.path.bind(context.dotfile_dir().clone());

        for hook in &unit.remove {
            if let Err(err) = hook.run(shell, &dir) {
                if errn == 0 {
                    println!(" Error");
                }
                eprintln!(
                    "Error:{:?}",
                    err.into_report().wrap_err("Uninstall hook failed")
                );
                errn += 1;
            }
        }
    }
    errn
}

fn deploy_unit(
    module: &mut Module,
    manager: &mut Manager,
    ctx: &crate::context::Context,
) -> (
    Result<(), color_eyre::Report>,
    Vec<interpreter::provider::State>,
) {
    let unit = module.unit.as_ref().expect("Unexpected error");
    let mut states = Vec::new();

    let res = unit
        .transactions
        .iter()
        .try_for_each(|transaction| {
            let (res, state) = manager.install(transaction);
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

fn write_tree(
    writer: &mut impl Write,
    root: UnitId,
    modules: HashMap<UnitId, Module>,
) -> std::io::Result<()> {
    let mut stack = vec![(vec![root], 0)];

    while let Some((members, n)) = stack.last_mut() {
        match members.get(*n) {
            Some(id) => {
                *n += 1;
                let module = modules.get(&id).unwrap();
                let last = members.len() == *n;
                let depth = stack.len() - 1;

                for _ in 0..depth {
                    write!(writer, "    ")?;
                }

                let part = if depth == 0 {
                    "  "
                } else if last {
                    "└─"
                } else {
                    "├─"
                };
                let name = module.path.name();
                let (col, status) = match &module.status {
                    Status::Undeployed => (Style::new().default_color(), "(Undeployed)"),
                    Status::Ok => (Style::new().bright_green(), "(Ok)"),
                    Status::Err => (Style::new().red(), "(Error)"),
                    Status::Skipped => (Style::new().bright_black(), "(Skipped)"),
                };

                writeln!(writer, "{part} {} {}", name.style(col), status.style(col))?;

                if !module.members.is_empty() {
                    stack.push((module.members.clone(), 0));
                }
            }
            None => {
                stack.pop();
            }
        }
    }
    Ok(())
}
