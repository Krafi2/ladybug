use clap::Clap;

#[derive(Clap)]
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

#[derive(Clap)]
enum SubCommand {
    Add(Add),
    Remove(Remove),
    Edit(Edit),
}

#[derive(Clap)]
struct Add {
    #[clap(short, long)]
    dry_run: bool,
    #[clap(short, long)]
    root: Option<String>,
}

#[derive(Clap)]
struct Remove {
    #[clap(short, long)]
    dry_run: bool,
}

#[derive(Clap)]
struct Edit {
    #[clap(short, long)]
    root: Option<String>,
}
