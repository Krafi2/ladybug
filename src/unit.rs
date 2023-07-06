use std::{path::Path, process::Stdio};

use crate::shell::Shell;

use common::rel_path::RelPath;
use provider::Transaction;

#[derive(Debug)]
pub struct Unit {
    pub name: String,
    pub desc: String,
    pub topic: Option<String>,
    pub shell: Option<Shell>,
    pub transactions: Vec<Transaction>,
    pub deploy: Vec<Routine>,
    pub remove: Vec<Routine>,
    pub capture: Vec<Routine>,
}

impl Unit {
    fn from_figment(figment: interpreter::UnitFigment) -> Option<Self> {
        let [deploy, remove, capture] =
            [figment.deploy, figment.remove, figment.capture].map(|figments| {
                figments
                    .into_iter()
                    .map(Routine::from_figment)
                    .collect::<Option<Vec<_>>>()
            });

        Some(Self {
            name: figment.name?,
            desc: figment.desc?,
            topic: figment.topic,
            shell: figment.shell.map(Into::into),
            transactions: figment.transactions,
            deploy: deploy?,
            remove: remove?,
            capture: capture?,
        })
    }
}

#[derive(Debug)]
pub struct Routine {
    shell: Option<Shell>,
    stdout: bool,
    workdir: Option<RelPath>,
    code: String,
}

impl Routine {
    fn from_figment(figment: interpreter::RoutineFigment) -> Option<Self> {
        Some(Self {
            shell: figment.shell.map(Into::into),
            stdout: figment.stdout.unwrap_or(false),
            workdir: figment.workdir,
            code: figment.body,
        })
    }

    pub fn run(&self, shell: &Shell, dir: &Path) -> Result<(), common::command::Error> {
        let stdout = if self.stdout {
            Stdio::inherit()
        } else {
            Stdio::null()
        };
        let shell = self.shell.as_ref().unwrap_or(shell);
        let dir = &self
            .workdir
            .as_ref()
            .map(|dir| dir.as_path())
            .unwrap_or(dir);

        let mut command = shell.new_command(&self.code);
        command
            .current_dir(dir)
            .stdin(Stdio::null())
            .stdout(stdout)
            .stderr(Stdio::piped());

        common::command::run_command(&mut command).map(|_| ())
    }
}

pub mod loader {
    use super::Unit;
    use crate::context::Context;
    use eval::Env;
    use interpreter::{self, UnitFigment, UnitPath};

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct UnitId(u32);

    pub enum Error {
        IO(std::io::Error),
    }

    pub enum Status {
        Ok(Unit),
        Degraded(UnitFigment, Vec<eval::Error>, String),
        Err(Error),
    }

    pub struct Module {
        pub id: UnitId,
        pub path: UnitPath,
        pub status: Status,
        pub members: Vec<UnitId>,
    }

    #[derive(Debug)]
    struct Frame {
        env: Env,
        queue: Vec<(UnitId, UnitPath)>,
    }

    pub struct Loader {
        stack: Vec<Frame>,
        env: Option<Env>,
        next_id: u32,
    }

    pub enum LoaderError {
        /// Encountered an error while accessing the dotfile directory
        DotfileDirError(std::io::Error),
        /// The dotfile directory doesn't exist
        DotfileDirMissing,
    }

    impl Loader {
        pub fn new(env: Env, context: &Context) -> Result<Self, LoaderError> {
            match context.dotfile_dir().try_exists() {
                Ok(true) => (),
                Ok(false) => {
                    return Err(LoaderError::DotfileDirMissing);
                }
                Err(err) => {
                    return Err(LoaderError::DotfileDirError(err));
                }
            }

            Ok(Self {
                stack: Vec::new(),
                env: Some(env),
                next_id: 0,
            })
        }

        pub fn root(&self) -> UnitId {
            UnitId(0)
        }

        fn next_id(&mut self) -> UnitId {
            let id = self.next_id;
            self.next_id += 1;
            UnitId(id)
        }

        fn load_module(
            &mut self,
            id: UnitId,
            path: UnitPath,
            env: Env,
            set_msg: &dyn Fn(String),
            ctx: &mut Context,
        ) -> Module {
            let src =
                std::fs::read_to_string(path.clone().unit_file().bind(ctx.dotfile_dir().clone()))
                    .map_err(Error::IO);

            let (status, env, queue) = match src {
                Ok(src) => {
                    let home_dir = ctx.home_dir().map(ToOwned::to_owned);
                    let dotfile_dir = ctx.dotfile_dir().clone();
                    let data = ctx.interpreter().eval(
                        &src,
                        path.clone(),
                        env,
                        home_dir,
                        dotfile_dir,
                        set_msg,
                    );
                    let status =
                        if data.errors.is_empty() {
                            Status::Ok(Unit::from_figment(data.figment).expect(
                                "There were no errors, unit generation shouldn't have failed",
                            ))
                        } else {
                            Status::Degraded(data.figment, data.errors, src)
                        };

                    let queue = data
                        .members
                        .unwrap_or_default()
                        .into_iter()
                        .map(|unit| (self.next_id(), unit))
                        .collect();

                    (status, data.env, queue)
                }
                Err(err) => (Status::Err(err), env, vec![]),
            };

            let members = queue.iter().map(|&(id, _)| id).collect();
            self.stack.push(Frame { env, queue });
            Module {
                id,
                path,
                status,
                members,
            }
        }

        pub fn load(&mut self, set_msg: &dyn Fn(String), ctx: &mut Context) -> Option<Module> {
            loop {
                match self.stack.last_mut() {
                    Some(frame) => match frame.queue.pop() {
                        Some((id, path)) => {
                            let env = frame.env.clone();
                            let module = self.load_module(id, path, env, set_msg, ctx);
                            break Some(module);
                        }
                        None => {
                            self.stack.pop();
                            continue;
                        }
                    },
                    None => match self.env.take() {
                        // This is the first iteration so we load the root node
                        Some(env) => {
                            let id = self.next_id();
                            let module = self.load_module(id, UnitPath::root(), env, set_msg, ctx);
                            break Some(module);
                        }
                        // This is the last iteration
                        None => break None,
                    },
                }
            }
        }
    }
}
