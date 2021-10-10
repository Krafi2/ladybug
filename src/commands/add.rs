use anyhow::Result;
use clap::Clap;

#[derive(Clap)]
pub(super) struct Add {
    #[clap(short, long)]
    dry_run: bool,
    #[clap(short, long)]
    topic: Option<String>,
}

impl Add {
    pub(super) fn run(self) -> Result<()> {
        todo!()
    }
}
