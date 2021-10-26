mod add;
mod deploy;
mod topic;

use crate::config::Config;
use clap::{AppSettings, Clap};

#[derive(Clap)]
#[clap(version = "0.1", author = "Krafi")]
#[clap(setting = AppSettings::ColoredHelp)]
pub(super) struct Opts {
    #[clap(subcommand)]
    subcmd: SubCommand,
}

#[derive(Clap)]
enum SubCommand {
    Topic(topic::Topic),
    Add(add::Add),
    Deploy(deploy::Deploy),
}

pub(super) fn run(config: &Config) -> Result<(), ()> {
    let opts = Opts::parse();
    match opts.subcmd {
        SubCommand::Topic(topic) => topic.run(),
        SubCommand::Add(add) => add.run(),
        SubCommand::Deploy(deploy) => deploy.run(config),
    }
}
