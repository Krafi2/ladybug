use crate::{
    config::Config,
    context, glob,
    resolver::{Node, NodeDesc, NodeId, Resolver},
    topic::{Env, EnvBuilder, Topic, TopicId},
};
use anyhow::{anyhow, Context, Result};
use clap::Clap;
use std::{
    cell::Cell,
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

    /// Try to deploy a single topic and return its final enviroment, Used `env` as the default
    /// topic enviroment. Results are cached by the resolver.
    fn deploy_topic<'a>(
        &self,
        resolver: &'a mut Resolver,
        topic: &TopicId,
        config: &Config,
    ) -> Result<&'a Env> {
        /// This struct stores information for each layer of the dependency tree.
        struct Nodule {
            /// The node we are evaluating
            node: NodeId,
            /// The index of the next child to evaluate
            idx: Cell<usize>,
        }

        impl Nodule {
            fn new(node: NodeId) -> Self {
                Self {
                    node,
                    idx: Cell::new(0),
                }
            }
        }

        let desc = resolver.new_node(topic);
        // The root node we actually want to deploy
        let root = resolver.open(desc, config);
        // All the nodes in the stack should be open with the exception of the head node
        let mut stack = Vec::<Nodule>::from([Nodule::new(root)]);

        // We use a depth first search algorithm to make sure all dependencies are satisfied before
        // deploying a node. Any error are propagated up towards the root node.
        'outer: while !stack.is_empty() {
            let head = stack.last().unwrap();
            let node = resolver.get(head.node);
            match node {
                // If the node is open, try to go deeper or deploy it
                Node::Open { children, topic } => {
                    let idx = head.idx.get();
                    head.idx.set(idx + 1);
                    match children.get(idx) {
                        // A child is available so we will try to push it onto the stack
                        Some(&desc) => {
                            // Check for cycles
                            // NOTE: this has quadratic complexity so we may want to replace this
                            // with a hasmap lookup in the future
                            for Nodule { node, .. } in &stack {
                                if NodeDesc::from(*node) == desc {
                                    // Name of the current node
                                    let name = topic.id().name();
                                    // Name of the dependecy causing the cycle
                                    let cycle = topic.dependencies()[idx].name();
                                    let error = Arc::new(anyhow!(
                                        "Dependency cycle detected: {} -> {}",
                                        name,
                                        cycle
                                    ));
                                    // The error will be processed in the next iteration
                                    resolver.error(head.node, error);
                                    continue 'outer;
                                }
                            }
                            let id = resolver.open(desc, config);
                            // Everything is ok, so we push the new node
                            stack.push(Nodule::new(id))
                        }
                        // All children have been deployed so we can do the same with the node
                        None => {
                            resolver.replace_with(head.node, |node| match node {
                                Node::Open { topic, .. } => match topic.deploy(self.dry_run) {
                                    // Close the node to be popped on the next iteration
                                    Ok(env) => Node::Closed(env),
                                    // Mark the node as an error. It will be propagated on the next
                                    // iteration
                                    Err(err) => Node::Err(Arc::new(err)),
                                },
                                _ => unreachable!("Node must be open"),
                            });
                        }
                    };
                }
                // The node is closed so we pop it and update its parent
                Node::Closed(env) => {
                    stack.pop();
                    // TODO: get rid of this clone
                    let env = env.clone();
                    match stack.last_mut() {
                        // Merge the node's enviroment into its parent
                        Some(parent) => match resolver.get_mut(parent.node) {
                            Node::Open { topic, .. } => topic.env_mut().extend(&env),
                            _ => panic!("Node should be open"),
                        },
                        // We are finised
                        None => (),
                    }
                }
                // There is an error so we need to propagate it up to the node's parents
                Node::Err(err) => {
                    // Last one already is an error
                    stack.pop();
                    for Nodule { node, idx, .. } in stack.drain(..).rev() {
                        // The index points to the _next_ child node to be checked, but we want the
                        // current one
                        let idx = idx.get() - 1;
                        // Extract the cause and the name of the bad child
                        let (name, cause) = match resolver.get(node) {
                            Node::Open { topic, children } => {
                                let name = topic.dependencies()[idx].name();
                                let child = resolver
                                    .upgrade_desc(children[idx])
                                    .map(|id| resolver.get(id));

                                let error = match child {
                                    Some(Node::Err(err)) => Arc::clone(err),
                                    Some(_) => panic!("Node should be an error"),
                                    None => panic!("Failed to get node"),
                                };
                                (name, error)
                            }
                            _ => panic!("Unwinding a closed node"),
                        };
                        let error = Arc::new(
                            anyhow::Error::msg(cause)
                                .context(format!("Failed to deploy dependency: {}", name)),
                        );
                        resolver.error(node, error)
                    }
                }
            };
        }

        // Retrieve the result
        match resolver.get(root) {
            Node::Closed(env) => Ok(env),
            Node::Err(err) => Err(anyhow::Error::msg(Arc::clone(err))),
            _ => panic!("Root node should be closed"),
        }
    }

    /// Try to deploy the provided topics, otherwise look for them in the dotfile directory. If a
    /// topic fails to deploy, still try the next ones.
    pub(super) fn run(self, ctx: context::Context) -> Result<Vec<Result<TopicId>>> {
        // Get the topics to deploy
        // We potentially don't have to collect this in order to conserve memory, however the
        // filesystem walkers might keep some handles open so we prioritize getting rid of them
        // as soon as possible
        let topics = match &self.topics {
            // Try to resolver the provided names
            Some(topics) => topics
                .iter()
                .map(|s| TopicId::new(Path::new(s)).with_context(|| anyhow!("No topic: '{}'", s)))
                .collect::<Vec<_>>(),
            // None have been provided so find all the top-level ones
            None => Self::get_all_topics(&ctx.config.dotfile_dir)
                .context("Failed to create topic iterator")?
                .collect(),
        };

        let mut resolver = Resolver::new();

        // Try to deploy all the topics that have been successfully resolved
        Ok(topics
            .into_iter()
            .map(|topic_id| match topic_id {
                Ok(id) => self
                    .deploy_topic(&mut resolver, &id, ctx.config)
                    .map(|_| id),
                Err(err) => Err(err).with_context(|| "Failed to deploy topic"),
            })
            .collect())
    }
}
