#[derive(clap::Parser)]
pub(super) struct Deploy {
    #[clap(short, long)]
    dry_run: bool,
    #[clap(short, long)]
    topics: Option<Vec<String>>,
}

impl Deploy {
    pub(super) fn run(self, context: &crate::config::Context) -> super::CmdResult {
        todo!()
    }
}
