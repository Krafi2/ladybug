mod interpreter;

use crate::{rel_path::RelPath, shell::Shell};
use std::path::PathBuf;

pub struct Topic {
    name: String,
    desc: String,
    target: RelPath,
    shell: Option<Shell>,
    children: Vec<tree::TopicId>,
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

pub use tree::{TopicId, TopicTree};
mod tree {
    use super::Topic;

    pub struct TopicId(u32);

    pub struct TopicTree {
        topics: Vec<Topic>,
    }
}

struct EvalContext {}

impl EvalContext {
    fn register_topic(&mut self, name: String) -> TopicId {
        todo!()
    }
}
