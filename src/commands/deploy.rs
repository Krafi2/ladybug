use crate::CmdStatus;
use crate::{
    config::Config,
    resolver::{self, Resolver},
    topic::{registry::TopicId, DescRegistry, Env, Topic, TopicDesc},
};
use anyhow::{Context, Result};
use clap::Clap;
use std::path::Path;

#[derive(Clap)]
pub(super) struct Deploy {
    #[clap(short, long)]
    dry_run: bool,
    #[clap(short, long)]
    topics: Option<Vec<String>>,
}

struct InterfaceImpl<'a> {
    pub registry: &'a mut DescRegistry,
    pub config: &'a Config,
    pub dry_run: bool,
}

impl<'a> resolver::Interface for InterfaceImpl<'a> {
    type Open = Topic;
    type Closed = Env;
    type OpenError = anyhow::Error;
    type CloseError = anyhow::Error;

    fn open(&mut self, id: TopicId) -> Result<Self::Open, Self::OpenError> {
        let desc = self.registry.get_desc(id).clone();
        Topic::from_desc(self.registry, self.config, &desc)
    }

    fn close(&mut self, topic: Self::Open) -> Result<Self::Closed, Self::CloseError> {
        topic.deploy(self.dry_run)
    }

    fn dependencies<'b>(&'b mut self, topic: &'b Self::Open) -> &'b [TopicId] {
        topic.dependencies()
    }

    fn satisfy(&mut self, open: &mut Self::Open, dep: &mut Self::Closed) {
        open.import(dep)
    }
}

impl Deploy {
    fn run_(self, config: &Config) -> Result<CmdStatus> {
        let mut registry = DescRegistry::new();

        // We store the name by which the topic was requested, and its TopicId, if it could be created
        let topics =
            get_topic_ids(&mut registry, config, self.topics).context("Failed to fetch topics")?;

        let mut resolver = Resolver::new();
        let mut interface = InterfaceImpl {
            registry: &mut registry,
            config,
            dry_run: self.dry_run,
        };

        // Try to deploy all the topics that have been successfully resolved and collect errors
        let errors = {
            topics
                .into_iter()
                .filter_map(|(name, id)| {
                    let id = match id {
                        Ok(id) => match resolver::resolve(&mut resolver, &mut interface, id) {
                            Ok(_) => None,
                            Err(id) => Some(Ok(id)),
                        },
                        Err(err) => Some(Err(err)),
                    };
                    id.map(|id| (name, id))
                })
                .collect::<Vec<_>>()
        };

        let err_len = errors.len();
        logging::log_errors(errors, &registry, &resolver);

        match err_len {
            0 => Ok(CmdStatus::Ok),
            _ => Ok(CmdStatus::Err),
        }
    }

    /// Try to deploy the provided topics, otherwise look for them in the dotfile directory. If a
    /// topic fails to deploy, still try the next ones.
    pub(super) fn run(self, config: &Config) -> std::result::Result<(), ()> {
        match self.run_(config) {
            Ok(CmdStatus::Ok) => Ok(()),
            Ok(CmdStatus::Err) => Err(()),
            Err(err) => {
                let err = err.context("Failed to deploy topics");
                log::error!("{:?}", err);
                Err(())
            }
        }
    }
}

fn get_all_topics<'a>(
    registry: &'a mut DescRegistry,
    root: &Path,
) -> Result<impl Iterator<Item = TopicId> + 'a> {
    // Look for topics in all top-level directories
    crate::topic::find_topics(registry, root, root, ["*"]).map(|iter| {
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
    registry: &mut DescRegistry,
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
                    .map(|desc| registry.new_id(&desc));
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
                .map(|id| (registry.get_desc(id).dir().display().to_string(), Ok(id)))
                .collect()
        }
    };
    Ok(res)
}

mod logging {
    use crate::{
        resolver::{Node, NodeErr, NodeId},
        topic::{DescRegistry, Env, Topic},
    };
    use anyhow::Result;
    use std::fmt::Write;

    type Resolver = crate::resolver::Resolver<Topic, Env, anyhow::Error, anyhow::Error>;

    pub fn log_errors<I>(errors: I, registry: &DescRegistry, resolver: &Resolver)
    where
        I: IntoIterator<Item = (String, Result<NodeId>)>,
    {
        for (name, id) in errors {
            let mut buffer = String::new();
            writeln!(&mut buffer, "\nFailed to deploy topic '{}':", name).unwrap();

            match id {
                Ok(id) => match &resolver[id] {
                    Node::Err(err) => match err {
                        NodeErr::Unsatisfied(_) => {
                            handle_unsatisfied(&mut buffer, registry, resolver, id)
                        }
                        NodeErr::Cycle(_) => handle_cycle(&mut buffer, registry, resolver, id),
                        NodeErr::OpenError(e) | NodeErr::CloseError(e) => {
                            handle_anyhow(&mut buffer, e)
                        }
                    },
                    _ => panic!("Not an error node"),
                },
                Err(err) => handle_anyhow(&mut buffer, &err),
            }

            writeln!(&mut buffer, "").unwrap();
            log::error!("{}", buffer);
        }
    }

    fn handle_unsatisfied(
        buffer: &mut String,
        registry: &DescRegistry,
        resolver: &Resolver,
        mut id: NodeId,
    ) {
        loop {
            match &resolver[id] {
                Node::Err(err) => match err {
                    NodeErr::Unsatisfied(next) => {
                        let name = registry.get_desc(id.into()).name();
                        writeln!(buffer, "Couldn't satisfy dependency '{}'", name).unwrap();
                        id = *next;
                    }
                    NodeErr::Cycle(id) => {
                        handle_cycle(buffer, registry, resolver, *id);
                        break;
                    }
                    NodeErr::OpenError(e) | NodeErr::CloseError(e) => handle_anyhow(buffer, e),
                },
                _ => panic!("Expected error"),
            }
        }
    }

    fn handle_cycle(buffer: &mut String, registry: &DescRegistry, resolver: &Resolver, id: NodeId) {
        fn write_entry<'a>(
            buffer: &mut String,
            registry: &DescRegistry,
            resolver: &Resolver,
            id: NodeId,
            prefix: &str,
        ) -> NodeId {
            let name = registry.get_desc(id.into()).name();
            writeln!(buffer, "{}{}", prefix, name).unwrap();
            match &resolver[id] {
                Node::Err(NodeErr::Cycle(id)) => *id,
                _ => panic!("Expected a cycle error"),
            }
        }

        writeln!(buffer, "\tDependency cycle detected:").unwrap();
        let mut next = write_entry(buffer, registry, resolver, id, "\t┌-> ");
        loop {
            if id == next {
                write_entry(buffer, registry, resolver, id, "\t└─  ");
                break;
            } else {
                next = write_entry(buffer, registry, resolver, id, "\t│   ");
            }
        }
    }

    fn handle_anyhow(buffer: &mut String, err: &anyhow::Error) {
        for err in err.chain() {
            writeln!(buffer, "\t{}", err).unwrap();
        }
    }
}
