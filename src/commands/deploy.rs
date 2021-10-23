use crate::{
    config::Config,
    glob,
    resolver::{Node, NodeErr, NodeId, Resolver},
    topic::{
        deployer::{Deployer, StandardDeployer},
        factory::TopicFactory,
        registry::Registry,
        registry::TopicId,
        Env, TemplateContext, Topic, TopicDesc,
    },
};
use anyhow::{anyhow, Context, Result};
use clap::Clap;
use std::{
    cell::Cell,
    fmt::{Debug, Display},
    path::{Path, PathBuf},
    sync::Arc,
};
type StdResult<T, E> = std::result::Result<T, E>;

#[derive(Clap)]
pub(super) struct Deploy {
    #[clap(short, long)]
    dry_run: bool,
    #[clap(short, long)]
    topics: Option<Vec<String>>,
}

/// The stack of the depth first dependency resolution algorithm
#[derive(Clone)]
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

type Stack = Vec<Nodule>;

#[derive(Clone)]
enum DeployError {
    Cycle {
        stack: Stack,
        /// The position in stack where the cycle was detected
        pos: usize,
    },
    DepError {
        stack: Stack,
    },
    // Custom(anyhow::Error),
}

impl Deploy {
    /// Try to deploy a single topic and return its final enviroment.
    /// Results are cached by the resolver.
    fn deploy_topic<F: TopicFactory, D: Deployer>(
        // registry: &mut Registry,
        resolver: &mut Resolver,
        factory: &mut F,
        deployer: &mut D,
        topic: TopicId,
    ) -> StdResult<NodeId, NodeId> {
        // The root node we want to deploy
        let root = resolver.open(topic, factory);
        // All the nodes in the stack should be open with the exception of the head node
        let mut stack = Stack::from([Nodule::new(root)]);

        // We use a depth first search algorithm to make sure all dependencies are satisfied before
        // deploying a node. Any errors are propagated up towards the root node.
        loop {
            let head = stack.last().unwrap();
            match resolver.get(head.node) {
                // If the node is open, try to go deeper or deploy it
                Node::Open(topic) => {
                    let idx = head.idx.get();
                    head.idx.set(idx + 1);
                    match topic.dependencies().get(idx) {
                        // A child is available so we will try to push it onto the stack
                        Some(&child) => {
                            let child = resolver.open(child, factory);

                            // Check for cycles
                            // NOTE: this has quadratic complexity so we may want to replace this
                            // with a more efficient implementation in the future. What comes to
                            // mind is either a hashset, or a `Storage` keeping track of active
                            // nodes
                            let mut cycle = None;
                            for (i, Nodule { node, .. }) in stack.iter().enumerate() {
                                if node == &child {
                                    cycle = Some(i);
                                    break;
                                }
                            }

                            // There is a cycle so we will mark the appropriate nodes and remove
                            // them from the stack
                            if let Some(pos) = cycle {
                                let first = stack[pos].clone();
                                let mut iter = stack.drain(pos..).peekable();
                                loop {
                                    match iter.next() {
                                        Some(Nodule { node, .. }) => {
                                            let next = iter.peek().unwrap_or(&first).node;
                                            resolver.error(node, NodeErr::Cycle(next));
                                        }
                                        None => break,
                                    }
                                }
                            } else {
                                // Everything is ok, so we push the new node
                                stack.push(Nodule::new(child));
                            }
                        }
                        // All children have been deployed so we can do the same with the node
                        None => {
                            resolver.replace(head.node, |node| match node {
                                Node::Open(topic) => match deployer.deploy(*topic) {
                                    // Close the node to be popped on the next iteration
                                    Ok(env) => Node::Closed(env),
                                    // Mark the node as an error. It will be propagated on the next
                                    // iteration
                                    Err(err) => Node::Err(NodeErr::Custom(err)),
                                },
                                _ => unreachable!("Node should be open"),
                            });
                        }
                    };
                }
                // The node is closed so we pop it and update its parent
                Node::Closed(env) => {
                    let head = stack.pop().unwrap().node;
                    // TODO: get rid of this clone
                    let env = env.clone();
                    match stack.last() {
                        // Merge the node's enviroment into its parent
                        Some(parent) => match resolver.get_mut(parent.node) {
                            Node::Open(topic) => topic.import(&env),
                            _ => unreachable!("Node should be open"),
                        },
                        // We are finished
                        None => break Ok(head),
                    }
                }
                // There is an error so we need to mark the parent as having an unsatisfied
                // dependency
                Node::Err(err) => {
                    // Last one already is an error so we remove it
                    let head = stack.pop().unwrap().node;
                    match stack.last() {
                        Some(Nodule { node, .. }) => {
                            resolver.error(*node, NodeErr::Unsatisfied(head))
                        }
                        // The error is on the root, nothing to do
                        None => break Err(head),
                    };
                }
            }
        }
    }

    /// Try to deploy the provided topics, otherwise look for them in the dotfile directory. If a
    /// topic fails to deploy, still try the next ones.
    pub(super) fn run(self, config: &Config) -> Result<()> {
        let mut registry = Registry::new();

        // We store the name by which the topic was requested, and its TopicId, if it could be created
        let topics =
            get_topic_ids(&mut registry, config, self.topics).context("Failed to fetch topics")?;

        let mut factory = crate::topic::factory::StandardFactory::new(&mut registry, config);
        let mut deployer = StandardDeployer::new(self.dry_run);
        let mut resolver = Resolver::new();
        let dry_run = self.dry_run;

        // Try to deploy all the topics that have been successfully resolved and collect errors
        let errors = topics
            .into_iter()
            .filter_map(|(name, id)| {
                let id = match id {
                    Ok(id) => {
                        match Self::deploy_topic(&mut resolver, &mut factory, &mut deployer, id) {
                            Ok(_) => None,
                            Err(id) => Some(Ok(id)),
                        }
                    }
                    Err(err) => Some(Err(err)),
                };
                id.map(|id| (name, id))
            })
            .collect::<Vec<_>>();

        let err_len = errors.len();
        logging::log_errors(errors, &registry, &resolver);

        // TODO: figure out how commands should report status
        match err_len {
            0 => Ok(()),
            _ => Err(anyhow::Error::msg("")),
        }
    }
}

fn get_all_topics<'a>(
    registry: &'a mut Registry,
    root: &Path,
) -> Result<impl Iterator<Item = TopicId> + 'a> {
    // Look for topics in all top-level directories
    crate::topic::find_topics(registry, root, ["*/"]).map(|iter| {
        iter.map(move |topic| match topic {
            Ok(topic) => topic,
            // Any errors here are bugs so we crash
            Err(err) => {
                panic!("Failed to construct topic: {:?}", err);
            }
        })
    })
}

fn get_topic_ids(
    registry: &mut Registry,
    config: &Config,
    topics: Option<Vec<String>>,
) -> Result<Vec<(String, Result<TopicId>)>> {
    let res = match topics {
        // Try to resolver the provided names
        Some(topics) => topics
            .iter()
            .map(|s| {
                let path = Path::new(s);
                let id = TopicDesc::new(path)
                    .context("Topic not found")
                    .map(|desc| registry.register(desc));
                (s.to_owned(), id)
            })
            .collect(),
        // None have been provided so find all the top-level ones
        None => {
            let topics = get_all_topics(registry, &config.dotfile_dir)
                .context("Failed to create topic iterator")?
                .collect::<Vec<_>>();
            topics
                .into_iter()
                .map(|id| (registry[id].dir().display().to_string(), Ok(id)))
                .collect()
        }
    };
    Ok(res)
}

mod logging {
    use crate::{
        resolver::{Node, NodeErr, NodeId, Resolver},
        topic::registry::Registry,
    };
    use anyhow::{Context, Result};
    use std::fmt::Write;

    pub fn log_errors<I>(errors: I, registry: &Registry, resolver: &Resolver)
    where
        I: IntoIterator<Item = (String, Result<NodeId>)>,
    {
        for (name, id) in errors {
            let mut buffer = String::new();
            writeln!(&mut buffer, "\nFailed to deploy topic '{}':", name);

            match id {
                Ok(id) => match resolver.get(id) {
                    Node::Err(err) => match err {
                        NodeErr::Unsatisfied(_) => {
                            handle_unsatisfied(&mut buffer, registry, resolver, id)
                        }
                        NodeErr::Cycle(_) => handle_cycle(&mut buffer, registry, resolver, id),
                        NodeErr::Custom(err) => handle_anyhow(&mut buffer, err),
                    },
                    _ => panic!("Not an error node"),
                },
                Err(err) => handle_anyhow(&mut buffer, &err),
            }

            writeln!(&mut buffer, "");
            log::error!("{}", buffer);
        }
    }

    fn handle_unsatisfied(
        buffer: &mut String,
        registry: &Registry,
        resolver: &Resolver,
        mut id: NodeId,
    ) {
        loop {
            match resolver.get(id) {
                Node::Err(err) => match err {
                    NodeErr::Unsatisfied(next) => {
                        let name = registry[id.into()].name();
                        writeln!(buffer, "Couldn't satisfy dependency '{}'", name);
                        id = *next;
                    }
                    NodeErr::Cycle(id) => {
                        handle_cycle(buffer, registry, resolver, *id);
                        break;
                    }
                    NodeErr::Custom(err) => {
                        handle_anyhow(buffer, err);
                        break;
                    }
                },
                _ => panic!("Expected error"),
            }
        }
    }

    fn handle_cycle(buffer: &mut String, registry: &Registry, resolver: &Resolver, id: NodeId) {
        fn write_entry(
            buffer: &mut String,
            resolver: &Resolver,
            registry: &Registry,
            id: NodeId,
            prefix: &str,
        ) -> NodeId {
            let name = registry[id.into()].name();
            writeln!(buffer, "{}{}", prefix, name);
            match resolver.get(id) {
                Node::Err(NodeErr::Cycle(id)) => *id,
                _ => panic!("Expected a cycle error"),
            }
        }

        writeln!(buffer, "\tDependency cycle detected:").unwrap();
        let mut next = write_entry(buffer, resolver, registry, id, "\t┌-> ");
        loop {
            if id == next {
                next = write_entry(buffer, resolver, registry, id, "\t└─  ");
                break;
            } else {
                next = write_entry(buffer, resolver, registry, id, "\t│   ");
            }
        }
    }

    fn handle_anyhow(buffer: &mut String, err: &anyhow::Error) {
        for err in err.chain() {
            writeln!(buffer, "\t{}", err).unwrap();
        }
    }
}
