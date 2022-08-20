#[derive(clap::Parser)]
pub struct Deploy {
    #[clap(short, long)]
    dry_run: bool,
    #[clap(short, long)]
    topics: Option<Vec<String>>,
}

impl Deploy {
    pub(super) fn run(self, context: &crate::context::Context) -> super::CmdResult {
        todo!()
    }
}
