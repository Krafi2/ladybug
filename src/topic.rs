mod parser;

use crate::{rel_path::RelPath, shell::Shell};
use std::{collections::HashMap, path::PathBuf};

pub struct Topic {
    name: String,
    desc: String,
    env: HashMap<String, String>,
    packages: Vec<Packages>,
    files: Vec<Files>,
    deploy: Routine,
    remove: Routine,
    capture: Routine,
    children: Vec<tree::TopicId>,
}

struct Package {
    name: String,
    version: String,
}

trait Provider {
    fn is_installed(&self, package: &Package) -> color_eyre::Result<bool>;
    fn install(&self, package: &Package) -> color_eyre::Result<()>;
    fn remove(&self, package: &Package) -> color_eyre::Result<()>;
}

struct Packages {
    provider: Box<dyn Provider>,
    packages: Vec<Package>,
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
    shell: Shell,
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
