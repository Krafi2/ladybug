use super::{Env, Topic};
use anyhow::Result;

pub trait Deployer {
    fn deploy(&mut self, topic: Topic) -> Result<Env>;
}

pub struct StandardDeployer {
    dry_run: bool,
}

impl StandardDeployer {
    pub fn new(dry_run: bool) -> Self {
        Self { dry_run }
    }
}

impl Deployer for StandardDeployer {
    fn deploy(&mut self, topic: Topic) -> Result<Env> {
        topic.deploy(self.dry_run)
    }
}
