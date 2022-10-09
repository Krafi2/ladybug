use color_eyre::{
    eyre::WrapErr,
    owo_colors::{OwoColorize, Style},
};
use interpreter::{provider::Manager, Interpreter, UnitPath};
use std::{
    collections::{HashMap, HashSet},
    io::Write,
    iter::FromIterator,
};

use crate::unit::{
    loader::{self, UnitId},
    Unit,
};

#[derive(clap::Parser)]
pub struct Deploy {
    #[clap(long)]
    dry_run: bool,
    topics: Option<Vec<String>>,
}

enum Status {
    Ok,
    Err,
    Skipped,
}

struct Module {
    path: UnitPath,
    unit: Option<Unit>,
    members: Vec<UnitId>,
    status: Status,
}

impl Deploy {
    pub(super) fn run(self, context: &crate::context::Context) -> super::CmdResult {
        let mut manager = Manager::new();
        let interpreter = Interpreter::new();
        let env = HashMap::new();
        let loader = loader::Loader::new(env, &interpreter, &mut manager, context);
        let root = loader.root();
        let mut modules = HashMap::new();
        let mut errn = 0;

        for module in loader {
            let (unit, status) = match module.status {
                loader::Status::Ok(unit) => (Some(unit), Status::Ok),
                loader::Status::Degraded(_, errors, src) => {
                    for err in errors {
                        errn += 1;
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
                    (None, Status::Err)
                }
                loader::Status::Err(err) => {
                    // Non-root modules should have already reported errors
                    if module.id == root {
                        errn += 1;
                        eprintln!("Cannot load root module:");
                        match err {
                            loader::Error::IO(io) => eprintln!("    File not found: {}", io),
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
        let parse_failed = errn != 0;

        if errn == 0 {
            let mut queue = Vec::new();
            let mut stack = vec![vec![root]];
            let mut deployed = Vec::new();
            let topics = self
                .topics
                .as_ref()
                .map(|topics| HashSet::<&String>::from_iter(topics.iter()));

            while let Some(frame) = stack.last_mut() {
                match frame.pop() {
                    Some(id) => {
                        let module = modules.get_mut(&id).unwrap();

                        let skip = match (&topics, &module.unit.as_ref().unwrap().topic) {
                            (Some(topics), Some(topic)) => !topics.contains(topic),
                            _ => false,
                        };

                        if skip {
                            module.status = Status::Skipped;
                        } else {
                            queue.push(id);
                            stack.push(module.members.clone());
                        }
                    }
                    None => {
                        stack.pop();
                    }
                }
            }

            if queue.is_empty() {
                if let Some(mut topics) = self.topics {
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

            let queue_len = queue.len();
            let res = queue.into_iter().enumerate().try_for_each(|(i, id)| {
                let module = modules.get_mut(&id).unwrap();
                print!("[{}/{}] Deploying unit {}:", i + 1, queue_len, &module.path);
                let unit = module.unit.as_ref().expect("Unexpected error");

                let mut n = 0;
                let res = unit
                    .transactions
                    .iter()
                    .try_for_each(|transaction| {
                        n += 1;
                        let provider = transaction.provider();
                        let provider = manager
                            .get_provider(provider)
                            .expect("Unitialized provider")
                            .map_err(|err| color_eyre::eyre::eyre!(err.to_string()))
                            .wrap_err_with(|| format!("Failed to fetch provider '{}'", provider));

                        provider.and_then(|provider| provider.install(&transaction))
                    })
                    .and_then(|_| {
                        if let Some(hook) = &unit.deploy {
                            let shell = &unit.shell.as_ref().unwrap_or(context.default_shell());
                            hook.run(shell)
                                .map_err(|err| err.into_report().wrap_err("Deploy hook failed"))
                        } else {
                            Ok(())
                        }
                    });

                println!(" Done");
                deployed.push((id, n));
                if res.is_err() {
                    module.status = Status::Err;
                }

                res
            });

            if let Err(err) = res {
                eprintln!("Error:{err:?}");
                eprintln!("Encountered an error, reverting changes");
                for (i, (id, n)) in deployed.iter().enumerate() {
                    let module = modules.get(&id).unwrap();
                    print!(
                        "[{}/{}] Removing unit {}:",
                        i + 1,
                        deployed.len(),
                        module.path
                    );
                    let unit = module.unit.as_ref().expect("Unexpected error");

                    for transaction in &unit.transactions[..*n] {
                        let provider = transaction.provider();
                        let provider = manager
                            .get_provider(provider)
                            .expect("Unitialized provider")
                            .map_err(|err| color_eyre::eyre::eyre!(err.to_string()))
                            .wrap_err_with(|| format!("Failed to fetch provider '{}'", provider));

                        if let Err(err) =
                            provider.and_then(|provider| provider.install(&transaction))
                        {
                            eprintln!("{err:?}");
                            errn += 1;
                        }
                    }

                    if let Some(hook) = &unit.remove {
                        let shell = &unit.shell.as_ref().unwrap_or(context.default_shell());
                        if let Err(err) = hook.run(shell) {
                            errn += 1;
                            eprintln!(
                                "Error:{:?}",
                                err.into_report().wrap_err("Uninstall hook failed")
                            );
                        }
                    }
                    println!(" Done");
                }
            }
        }
        if errn != 0 {
            let verb = if parse_failed { "aborted" } else { "failed" };
            if errn == 1 {
                eprintln!("\nDeployment {verb} due to previous error");
            } else {
                eprintln!("\nDeployment {verb} due to {errn} previous errors");
            }
        }

        println!("\nUnit status:");
        let mut stdout = std::io::stdout().lock();
        write_tree(&mut stdout, root, modules).unwrap();
        Ok(Ok(()))
    }
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
                    Status::Ok => (Style::new().default_color(), "(Ok)"),
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
