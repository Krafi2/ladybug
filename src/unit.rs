mod interpreter;
use crate::{rel_path::RelPath, shell::Shell};
pub use interpreter::{
    provider::{Manager, Provider, Transaction},
    Env, Interpreter,
};
use std::path::PathBuf;

pub struct Unit {
    pub name: String,
    pub desc: String,
    pub target: RelPath,
    pub topic: Option<String>,
    pub shell: Option<Shell>,
    pub transactions: Vec<Transaction>,
    pub deploy: Option<Routine>,
    pub remove: Option<Routine>,
    pub capture: Option<Routine>,
}

enum DeployMethod {
    SoftLink,
    HardLink,
    Copy,
}

enum ConflictStrat {
    Abort,
    Rename,
    Remove,
}

struct Files {
    method: DeployMethod,
    conflicts: ConflictStrat,
    source: RelPath,
    target: RelPath,
    files: Vec<PathBuf>,
}

#[derive(Debug)]
pub struct Routine {
    shell: Option<Shell>,
    stdin: bool,
    stdout: bool,
    code: String,
}

pub use tree::{Module, ModuleTree, Status, UnitId};
mod tree {
    use super::{
        interpreter::{provider::Manager, Env, Interpreter, UnitFigment, Value},
        Unit,
    };
    use crate::{context::Context, rel_path::RelPath};
    use color_eyre::eyre::WrapErr;
    use std::{
        collections::HashMap,
        path::{Path, PathBuf},
    };

    #[derive(Debug, Clone, Copy)]
    pub struct UnitId(u32);

    pub enum Status {
        Ok(Unit),
        Degraded(UnitFigment),
        Err(color_eyre::Report),
    }

    pub struct Module {
        pub status: Status,
        pub env: Env,
        pub members: Vec<UnitId>,
    }

    pub struct ModuleTree {
        modules: Vec<Module>,
        degraded: bool,
    }

    struct Frame {
        status: Status,
        env: Env,
        members: Vec<UnitId>,
        queue: Vec<RelPath>,
    }

    impl ModuleTree {
        pub fn load(
            interpreter: &Interpreter,
            manager: &mut Manager,
            env: Env,
            context: &Context,
        ) -> Self {
            let mut units = Vec::new();
            let mut stack = vec![load_frame(
                context.dotfile_dir().join("main.unit"),
                manager,
                env,
                interpreter,
                context,
            )];
            let mut degraded = false;

            while let Some(frame) = stack.last_mut() {
                match frame.queue.pop() {
                    Some(path) => {
                        let env = frame.env.clone();
                        stack.push(load_frame(path, manager, env, interpreter, context))
                    }
                    None => {
                        let frame = stack.pop().unwrap();
                        let unit = Module {
                            status: frame.status,
                            env: frame.env,
                            members: frame.members,
                        };
                        if let Status::Err(_) | Status::Degraded(_) = &unit.status {
                            degraded = true;
                        }

                        let id = units.len();
                        units.push(unit);
                        if let Some(frame) = stack.last_mut() {
                            frame.members.push(UnitId(id as u32));
                        }
                    }
                }
            }

            Self {
                modules: units,
                degraded,
            }
        }

        pub fn get(&self, id: UnitId) -> &Module {
            &self.modules[id.0 as usize]
        }

        pub fn get_mut(&mut self, id: UnitId) -> &mut Module {
            &mut self.modules[id.0 as usize]
        }

        pub fn is_degraded(&self) -> bool {
            self.degraded
        }

        pub fn root(&self) -> UnitId {
            UnitId(self.modules.len() as u32 - 1)
        }
    }

    fn load_frame(
        path: RelPath,
        manager: &mut Manager,
        env: HashMap<String, Value>,
        interpreter: &Interpreter,
        context: &Context,
    ) -> Frame {
        let src = std::fs::read_to_string(&path)
            .wrap_err_with(|| format!("Failed to read unit {}", path));

        let (status, env, queue) = match src {
            Ok(src) => {
                let data = interpreter.eval(&src, manager, env, context);
                let status = if data.errors.is_empty() {
                    Status::Ok(
                        data.figment
                            .into_unit()
                            .expect("There were no errors, unit generation shouldn't have failed"),
                    )
                } else {
                    Status::Degraded(data.figment)
                };
                let queue = data
                    .members
                    .unwrap_or_default()
                    .into_iter()
                    .map(|unit| {
                        let unit = unit.0;
                        let mut path = path.clone();
                        assert!(path.pop());
                        path.push(&unit);
                        path.push(format!(
                            "{}.unit",
                            Path::display(unit.components().last().unwrap().as_ref())
                        ));
                        path
                    })
                    .collect();
                (status, data.env, queue)
            }
            Err(err) => (Status::Err(err), env, vec![]),
        };

        Frame {
            status,
            env,
            members: Vec::new(),
            queue,
        }
    }
}