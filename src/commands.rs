mod add;
mod deploy;
mod topic;

use crate::config::Context;
use clap::{Parser, Subcommand};

#[derive(Parser)]
#[clap(author = "Krafi", version = "0.1.0", about = "A cute dotfile manager", long_about = None)]
pub(super) struct Opts {
    #[clap(subcommand)]
    command: SubCommand,
}

#[derive(Subcommand)]
enum SubCommand {
    Topic(topic::Topic),
    Add(add::Add),
    Deploy(deploy::Deploy),
}

type CmdResult = color_eyre::Result<()>;

pub(super) fn run(config: &Context) -> CmdResult {
    let opts = Opts::parse();
    match opts.command {
        SubCommand::Topic(topic) => topic.run(),
        SubCommand::Add(add) => add.run(),
        SubCommand::Deploy(deploy) => deploy.run(config),
    }
}
