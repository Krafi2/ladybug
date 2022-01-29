use clap::{Parser, Subcommand};

#[derive(Parser)]
pub(super) struct Topic {
    #[clap(subcommand)]
    subcmd: SubCommand,
}

impl Topic {
    pub(super) fn run(self) -> Result<(), ()> {
        match self.subcmd {
            SubCommand::Add(_) => todo!(),
            SubCommand::Remove(_) => todo!(),
            SubCommand::Edit(_) => todo!(),
        }
    }
}

#[derive(Subcommand)]
enum SubCommand {
    Add(Add),
    Remove(Remove),
    Edit(Edit),
}

#[derive(Parser)]
struct Add {
    #[clap(short, long)]
    dry_run: bool,
    #[clap(short, long)]
    root: Option<String>,
}

#[derive(Parser)]
struct Remove {
    #[clap(short, long)]
    dry_run: bool,
}

#[derive(Parser)]
struct Edit {
    #[clap(short, long)]
    root: Option<String>,
}
