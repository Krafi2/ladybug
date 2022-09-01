mod deploy;

use std::path::PathBuf;

use crate::context::Context;
use clap::{Parser, Subcommand};

#[derive(Parser)]
#[clap(author = "Krafi", version = "0.1.0", about = "A cute dotfile manager", long_about = None)]
pub(super) struct Opts {
    #[clap(action)]
    pub no_root: bool,
    #[clap(action)]
    pub root: bool,
    #[clap(value_parser)]
    pub config: Option<PathBuf>,
    #[clap(value_parser)]
    pub dotfiles: Option<PathBuf>,
    #[clap(subcommand)]
    pub command: SubCommand,
}

#[derive(Subcommand)]
pub enum SubCommand {
    Deploy(deploy::Deploy),
}

type CmdResult = color_eyre::Result<()>;

pub(super) fn run(context: &Context) -> CmdResult {
    let opts = Opts::parse();
    match opts.command {
        SubCommand::Deploy(deploy) => deploy.run(context),
    }
}
