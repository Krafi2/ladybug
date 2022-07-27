use clap::Parser;

#[derive(Parser)]
pub(super) struct Add {
    #[clap(short, long)]
    dry_run: bool,
    #[clap(short, long)]
    topic: Option<String>,
}

impl Add {
    pub(super) fn run(self) -> super::CmdResult {
        todo!()
    }
}
