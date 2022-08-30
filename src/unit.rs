mod interpreter;

use crate::{rel_path::RelPath, shell::Shell};
use std::path::PathBuf;

pub struct Unit {
    name: String,
    desc: String,
    target: RelPath,
    topic: Option<String>,
    shell: Option<Shell>,
    members: Vec<tree::UnitId>,
    env: interpreter::Env,
    transactions: Vec<interpreter::Transaction>,
    deploy: Option<Routine>,
    remove: Option<Routine>,
    capture: Option<Routine>,
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

struct Routine {
    shell: Option<Shell>,
    stdin: bool,
    stdout: bool,
    code: String,
}

pub use tree::{UnitId, UnitTree};
mod tree {
    use super::Unit;

    pub struct UnitId(u32);

    pub struct UnitTree {
        units: Vec<Unit>,
    }
}

struct EvalContext {}

impl EvalContext {
    fn register_unit(&mut self, name: String) -> UnitId {
        todo!()
    }
}
