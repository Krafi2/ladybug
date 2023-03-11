mod deploy;

use crate::context::Context;
use clap::Subcommand;

#[derive(Subcommand, Debug)]
pub enum Command {
    Deploy(deploy::Deploy),
}

type CmdResult = color_eyre::Result<Result<(), ()>>;

impl Command {
    pub fn run(self, context: &Context) -> CmdResult {
        match self {
            Command::Deploy(deploy) => deploy.run(context),
        }
    }
}
