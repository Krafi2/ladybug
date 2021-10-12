use crate::{
    config::Config,
    glob,
    resolver::{Node, NodeDesc, NodeId, Resolver},
    topic::{Env, TemplateContext, Topic, TopicId},
};
use anyhow::{anyhow, Context, Result};
use clap::Clap;
use std::{
    cell::Cell,
    fmt::{Debug, Display},
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
        resolver: &'a mut Resolver,
        topic: &TopicId,
        config: &Config,
        dry_run: bool,
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
                                Node::Open { topic, .. } => match topic.deploy(dry_run) {
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
                            Node::Open { topic, .. } => topic.import(&env),
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
    pub(super) fn run(self, config: &Config) -> Result<()> {
        // We store the name of the topic and its TopicId, if it could be created
        let topics: Vec<(String, Result<TopicId>)> = match self.topics {
            // Try to resolver the provided names
            Some(topics) => topics
                .iter()
                .map(|s| {
                    let path = Path::new(s);
                    let topic = TopicId::new(path).context("Topic not found");
                    (s.to_owned(), topic)
                })
                .collect(),
            // None have been provided so find all the top-level ones
            None => Self::get_all_topics(&config.dotfile_dir)
                .context("Failed to create topic iterator")?
                .filter_map(|topic| match topic {
                    Ok(topic) => Some((topic.name().to_string(), Ok(topic))),
                    // Any errors here are because of bugs or filesystem privilege issues, so we
                    // will log them, but not print them in the final summary                    Err(err) => todo!("Implement logging"),
                    Err(err) => todo!("Implement logging"),
                })
                .collect(),
        };

        let mut resolver = Resolver::new();
        let dry_run = self.dry_run;

        // Try to deploy all the topics that have been successfully resolved
        let results = topics
            .into_iter()
            .filter_map(|(name, topic)| {
                match topic
                    .and_then(|id| Self::deploy_topic(&mut resolver, &id, config, dry_run))
                    .with_context(|| format!("Failed to deploy topic: '{}'", name))
                {
                    Ok(_) => None,
                    Err(err) => Some(err),
                }
            })
            .collect::<Vec<_>>();

        match results.len() {
            0 => Ok(()),
            _ => Err(anyhow::Error::msg(DeployError(results))),
        }
    }
}

pub struct DeployError(Vec<anyhow::Error>);

impl Display for DeployError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Failed to deploy the following topics:\n")?;
        for err in &self.0 {
            Display::fmt(err, f)?;
        }
        Ok(())
    }
}

impl Debug for DeployError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("DeployError").field(&self.0).finish()
    }
}
