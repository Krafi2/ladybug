use crate::{
    config::Config,
    context::Context,
    resolver::{Node, NodeId, Resolver},
    topic::{Topic, TopicId},
};
use clap::Clap;

#[derive(Clap)]
pub(super) struct Deploy {
    #[clap(short, long)]
    dry_run: bool,
    #[clap(short, long, default_value = todo!())]
    topic: Vec<String>,
}

impl Deploy {
    pub(super) fn run(self, ctx: Context) {
        todo!()
    }
}
