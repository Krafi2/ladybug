use clap::Clap;

#[derive(Clap)]
pub(super) struct Deploy {
    #[clap(short, long)]
    dry_run: bool,
    #[clap(short, long, default_value = todo!())]
    topic: Vec<String>,
}

impl Deploy {
    pub(super) fn run(self) {
        todo!()
    }
}
