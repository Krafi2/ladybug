use clap::Clap;

#[derive(Clap)]
pub(super) struct Add {
    #[clap(short, long)]
    dry_run: bool,
    #[clap(short, long, default_value = todo!())]
    topic: String,
}

impl Add {
    pub(super) fn run(self) {
        todo!()
    }
}
