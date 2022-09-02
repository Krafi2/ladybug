mod deploy;

use std::path::PathBuf;

use crate::context::Context;
use clap::{Parser, Subcommand};

#[derive(Parser)]
#[clap(author = "Krafi", version = "0.1.0", about = "A cute dotfile manager", long_about = None)]
pub(super) struct Opts {
    #[clap(long, action)]
    pub no_root: bool,
    #[clap(long, action)]
    pub root: bool,
    #[clap(long, value_parser)]
    pub config: Option<PathBuf>,
    #[clap(long, value_parser)]
    pub dotfiles: Option<PathBuf>,
    #[clap(subcommand)]
    pub command: SubCommand,
}

#[derive(Subcommand)]
pub enum SubCommand {
    Deploy(deploy::Deploy),
}

type CmdResult = color_eyre::Result<Result<(), ()>>;

pub(super) fn run(context: &Context) -> CmdResult {
    let opts = Opts::parse();
    match opts.command {
        SubCommand::Deploy(deploy) => deploy.run(context),
    }
}
