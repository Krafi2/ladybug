use crate::{
    config::Config,
    context,
    env::Env,
    glob,
    resolver::{Node, NodeId, Resolver},
    topic::{Topic, TopicId},
};
use anyhow::{anyhow, Context, Result};
use clap::Clap;
use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

#[derive(Clap)]
pub(super) struct Deploy {
    #[clap(short, long)]
    dry_run: bool,
    #[clap(short, long)]
    topics: Option<Vec<String>>,
}

impl Deploy {
    fn get_all_topics(root: &Path) -> Result<impl Iterator<Item = Result<TopicId>>> {
        // Look for topics in all top-level directories
        crate::topic::find_topics(root, ["*/"])
    }

    fn deploy_topic<'a>(
        &self,
        resolver: &'a mut Resolver,
        topic: &TopicId,
        config: &Config,
        env: &Env,
    ) -> Result<&'a Env> {
        //
        struct Nodule {
            node: NodeId,
            idx: usize,
            env: Env,
        }

        impl Nodule {
            fn new(node: NodeId, env: Env) -> Self {
                Self { node, idx: 0, env }
            }
        }

        fn unwind(stack: &mut Vec<Nodule>, resolver: &mut Resolver, mut cause: Arc<anyhow::Error>) {
        }

        let root = resolver.open(topic.clone(), config);
        let mut stack = Vec::<Nodule>::from([Nodule::new(root, env.clone())]);

        while !stack.is_empty() {
            let head = stack.last_mut().unwrap();
            let node = resolver.get(head.node, config);
            let idx = head.idx;
            match node {
                Node::Open { children, topic } => match children.get(idx) {
                    Some(id) => {
                        head.idx += 1;
                        for Nodule { node, .. } in &stack {
                            if node == id {
                                let name = topic.id().name();
                                let cycle = topic.dependencies()[idx].name();
                                resolver.error(
                                    head.node,
                                    Arc::new(
                                        anyhow!("Dependency cycle detected")
                                            .context(format!("{} -> {}", name, cycle)),
                                    ),
                                );
                                continue;
                            }
                        }
                        stack.push(Nodule::new(*id, env.clone()))
                    }
                    None => match topic.deploy(&head.env) {
                        Ok(env) => resolver.close(head.node, env),
                        Err(err) => resolver.error(head.node, Arc::new(err)),
                    },
                },
                Node::Closed(env) => {
                    stack.pop();
                    match stack.last_mut() {
                        Some(parent) => parent.env.merge(&env),
                        None => (),
                    }
                }
                Node::Err(err) => {
                    stack.pop();
                    let err = Arc::clone(err);
                    for Nodule { node, idx, .. } in stack.drain(..).rev() {
                        let idx = idx - 1;
                        let (name, cause) =
                            match resolver.try_get(node).expect("Failed to get node") {
                                Node::Open { topic, children } => {
                                    let name = topic.dependencies()[idx].name();
                                    let error = match resolver.try_get(children[idx]) {
                                        Some(Node::Err(err)) => err,
                                        Some(_) => panic!("Node should be error"),
                                        None => panic!("Failed to get node"),
                                    };
                                    (name, error)
                                }
                                _ => panic!("Unwinding a closed node"),
                            };
                        resolver.error(
                            node,
                            Arc::new(
                                anyhow::Error::msg(Arc::clone(cause))
                                    .context(format!("Failed to deploy dependency: {}", name)),
                            ),
                        )
                    }
                }
            };
        }

        match resolver.get(root, config) {
            Node::Closed(env) => Ok(env),
            Node::Err(err) => Err(anyhow::Error::msg(Arc::clone(err))),
            _ => panic!("Root node should be closed"),
        }
    }

    pub(super) fn run(self, ctx: context::Context) -> Result<Vec<Result<TopicId>>> {
        let topics = match &self.topics {
            Some(topics) => topics
                .iter()
                .map(|s| TopicId::new(Path::new(s)).with_context(|| anyhow!("No topic: '{}'", s)))
                .collect::<Vec<_>>(),
            None => Self::get_all_topics(&ctx.config.dotfile_dir)
                .context("Failed to create topic iterator")?
                .collect(),
        };

        let mut resolver = Resolver::new();

        Ok(topics
            .into_iter()
            .map(|topic_id| match topic_id {
                Ok(id) => self
                    .deploy_topic(&mut resolver, &id, ctx.config, ctx.env)
                    .map(|_| id),
                Err(err) => Err(err).with_context(|| "Failed to deploy topic"),
            })
            .collect())
    }
}
