use std::process::Stdio;

use crate::shell::Shell;

use color_eyre::{
    eyre::eyre,
    section::{Section, SectionExt},
};

pub use interpreter::{
    provider::{Manager, Provider, Transaction},
    Env, Interpreter,
};

pub struct Unit {
    pub name: String,
    pub desc: String,
    pub topic: Option<String>,
    pub shell: Option<Shell>,
    pub transactions: Vec<Transaction>,
    pub deploy: Option<Routine>,
    pub remove: Option<Routine>,
    pub capture: Option<Routine>,
}

impl Unit {
    fn from_figment(figment: interpreter::UnitFigment) -> Option<Self> {
        let [deploy, remove, capture] =
            [figment.deploy, figment.remove, figment.capture].map(|figment| match figment {
                Some(figment) => Routine::from_figment(figment).map(Some),
                None => Some(None),
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
    code: String,
}

#[derive(Debug)]
pub enum CmdError {
    Spawn(std::io::Error),
    IO(std::io::Error),
    Failed(std::process::Output),
}

impl Routine {
    fn from_figment(figment: interpreter::RoutineFigment) -> Option<Self> {
        Some(Self {
            shell: figment.shell.map(Into::into),
            stdout: figment.stdout.unwrap_or(true),
            code: figment.body,
        })
    }

    pub fn run(&self, shell: &Shell) -> Result<(), CmdError> {
        let stdout = if self.stdout {
            Stdio::piped()
        } else {
            Stdio::null()
        };
        let shell = self.shell.as_ref().unwrap_or(shell);
        let child = shell
            .new_command(&self.code)
            .stdin(Stdio::null())
            .stdout(stdout)
            .stderr(Stdio::piped())
            .spawn()
            .map_err(CmdError::Spawn)?;
        let output = child.wait_with_output().map_err(CmdError::IO)?;

        if output.status.success() {
            Ok(())
        } else {
            Err(CmdError::Failed(output))
        }
    }
}

impl CmdError {
    pub fn into_report(self) -> color_eyre::Report {
        match self {
            CmdError::Spawn(err) => eyre!(err).wrap_err("Cannot start process"),
            CmdError::IO(err) => eyre!(err),
            CmdError::Failed(output) => match output.status.code() {
                Some(code) => eyre!("Process failed with exit code {code}"),
                None => eyre!("Process was terminated by a signal"),
            }
            .section(
                String::from_utf8_lossy(&output.stderr)
                    .as_ref()
                    .trim()
                    .to_owned()
                    .header("Stderr:"),
            ),
        }
    }
}

pub mod loader {
    use super::Unit;
    use crate::context::Context;
    use interpreter::{self, provider::Manager, Env, Interpreter, UnitFigment, UnitPath};

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct UnitId(u32);

    pub enum Error {
        IO(std::io::Error),
    }

    pub enum Status {
        Ok(Unit),
        Degraded(UnitFigment, Vec<interpreter::error::Error>, String),
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

    pub struct Loader<'a> {
        stack: Vec<Frame>,
        env: Option<Env>,
        interpreter: &'a Interpreter,
        manager: &'a mut Manager,
        context: &'a Context,
        next_id: u32,
    }

    impl<'a> Loader<'a> {
        pub fn new(
            env: Env,
            interpreter: &'a Interpreter,
            manager: &'a mut Manager,
            context: &'a Context,
        ) -> Self {
            Self {
                stack: Vec::new(),
                env: Some(env),
                interpreter,
                manager,
                context,
                next_id: 0,
            }
        }

        pub fn root(&self) -> UnitId {
            UnitId(0)
        }

        fn next_id(&mut self) -> UnitId {
            let id = self.next_id;
            self.next_id += 1;
            UnitId(id)
        }

        fn load_module(&mut self, id: UnitId, path: UnitPath, env: Env) -> Module {
            let src = std::fs::read_to_string(
                path.clone()
                    .unit_file()
                    .bind(self.context.dotfile_dir().clone()),
            )
            .map_err(Error::IO);

            let (status, env, queue) = match src {
                Ok(src) => {
                    let data = self.interpreter.eval(
                        &src,
                        path.clone(),
                        self.manager,
                        env,
                        self.context.home_dir(),
                        self.context.dotfile_dir(),
                        self.context.is_root(),
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
    }

    impl<'a> Iterator for Loader<'a> {
        type Item = Module;

        fn next(&mut self) -> Option<Self::Item> {
            match self.stack.last_mut() {
                Some(frame) => match frame.queue.pop() {
                    Some((id, path)) => {
                        let env = frame.env.clone();
                        let module = self.load_module(id, path, env);
                        Some(module)
                    }
                    None => {
                        self.stack.pop();
                        None
                    }
                },
                None => match self.env.take() {
                    Some(env) => {
                        let id = self.next_id();
                        let module = self.load_module(id, UnitPath::root(), env);
                        Some(module)
                    }
                    None => None,
                },
            }
        }
    }

    impl<'a> std::iter::FusedIterator for Loader<'a> {}
}
