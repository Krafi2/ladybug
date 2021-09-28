use crate::{
    config::Config,
    topic::{Env, Topic, TopicId},
};
use anyhow::{anyhow, Context};
use std::{
    collections::hash_map::{DefaultHasher, HashMap},
    hash::{BuildHasherDefault, Hash, Hasher},
    sync::Arc,
};

#[derive(Debug)]
pub enum Node {
    Open {
        children: Vec<NodeDesc>,
        topic: Box<Topic>,
    },
    Closed(Env),
    Err(Arc<anyhow::Error>),
}

#[derive(Debug)]
enum NodeStatus {
    Ready(Node),
    Waiting(TopicId),
    /// This variant is necessary for when we want to take ownership of a `Node`. All absent nodes
    /// are temporary and must be replaced with a valid `NodeStatus` before the function eits.
    Absent,
}

pub use storage::{NodeDesc, NodeId};
mod storage {
    #[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
    // Describes a node that may still be uninitialized
    pub struct NodeDesc(usize);

    impl From<NodeId> for NodeDesc {
        fn from(id: NodeId) -> Self {
            id.0
        }
    }

    #[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
    // Describes a node that is known to be initialized
    pub struct NodeId(NodeDesc);

    use super::{Node, NodeStatus};
    use crate::topic::TopicId;
    use std::{
        collections::HashMap,
        ops::{Index, IndexMut},
    };

    #[derive(Debug, Default)]
    pub(super) struct Storage {
        nodes: Vec<NodeStatus>,
    }

    impl Storage {
        pub fn get_desc(&self, desc: NodeDesc) -> Option<&NodeStatus> {
            self.nodes.get(desc.0)
        }

        pub fn get_desc_mut(&mut self, desc: NodeDesc) -> Option<&mut NodeStatus> {
            self.nodes.get_mut(desc.0)
        }

        pub fn get_id(&self, id: NodeId) -> Option<&Node> {
            self.get_desc(id.0).map(|node| match node {
                NodeStatus::Ready(node) => node,
                NodeStatus::Waiting(_) => panic!("Node should be ready"),
                NodeStatus::Absent => panic!("Node must not be absent"),
            })
        }

        pub fn get_id_mut(&mut self, id: NodeId) -> Option<&mut Node> {
            self.get_desc_mut(id.0).map(|node| match node {
                NodeStatus::Ready(node) => node,
                NodeStatus::Waiting(_) => panic!("Node should be ready"),
                NodeStatus::Absent => panic!("Node must not be absent"),
            })
        }

        pub fn new_status(&mut self, status: NodeStatus) -> NodeDesc {
            let idx = self.nodes.len();
            self.nodes.push(status);
            NodeDesc(idx)
        }

        pub fn upgrade_desc(&self, desc: NodeDesc) -> Option<NodeId> {
            match self[desc] {
                NodeStatus::Ready(_) => Some(NodeId(desc)),
                _ => None,
            }
        }

        // pub fn new_node(&mut self, node: Node) {}
    }

    impl Index<NodeDesc> for Storage {
        type Output = NodeStatus;

        fn index(&self, index: NodeDesc) -> &Self::Output {
            self.get_desc(index).expect("Node not found")
        }
    }

    impl IndexMut<NodeDesc> for Storage {
        fn index_mut(&mut self, index: NodeDesc) -> &mut Self::Output {
            self.get_desc_mut(index).expect("Node not found")
        }
    }

    impl Index<NodeId> for Storage {
        type Output = Node;

        fn index(&self, index: NodeId) -> &Self::Output {
            self.get_id(index).expect("Node not found")
        }
    }

    impl IndexMut<NodeId> for Storage {
        fn index_mut(&mut self, index: NodeId) -> &mut Self::Output {
            self.get_id_mut(index).expect("Node not found")
        }
    }
}

#[derive(Debug, Default)]
pub struct Resolver {
    id_map: HashMap<TopicId, NodeDesc>,
    storage: storage::Storage,
}

impl Resolver {
    pub fn new() -> Self {
        Self::default()
    }

    fn get_or_insert_node_with<F>(&mut self, topic: &TopicId, func: F) -> NodeDesc
    where
        F: FnOnce(&mut Self) -> NodeStatus,
    {
        match self.id_map.get(topic) {
            Some(id) => *id,
            None => {
                let node = func(self);
                self.storage.new_status(node)
            }
        }
    }

    fn construct_node(&mut self, topic_id: TopicId, config: &Config) -> Node {
        match Topic::from_id(topic_id, config) {
            Ok(topic) => {
                let children = topic
                    .dependencies()
                    .iter()
                    .map(|id| self.get_or_insert_node_with(id, |_| NodeStatus::Waiting(id.clone())))
                    .collect::<Vec<_>>();

                Node::Open {
                    children,
                    topic: Box::new(topic),
                }
            }
            Err(error) => Node::Err(Arc::new(error)),
        }
    }

    pub fn new_node(&mut self, topic: &TopicId) -> NodeDesc {
        self.get_or_insert_node_with(topic, |_| NodeStatus::Waiting(topic.clone()))
    }

    pub fn open(&mut self, desc: NodeDesc, config: &Config) -> NodeId {
        match &mut self.storage[desc] {
            NodeStatus::Ready(_) => (),
            node @ NodeStatus::Waiting(_) => {
                let old = std::mem::replace(node, NodeStatus::Absent);
                let new = match old {
                    NodeStatus::Waiting(topic_id) => Self::construct_node(self, topic_id, config),
                    _ => unreachable!(),
                };
                self.storage[desc] = NodeStatus::Ready(new);
            }
            NodeStatus::Absent => panic!("Node must not be absent"),
        };
        self.storage
            .upgrade_desc(desc)
            .expect("Failed to upgrade descriptor")
    }

    pub fn replace_with<F>(&mut self, id: NodeId, func: F)
    where
        F: FnOnce(Node) -> Node,
    {
        match &mut self.storage[NodeDesc::from(id)] {
            node @ NodeStatus::Ready(_) => {
                let old = match std::mem::replace(node, NodeStatus::Absent) {
                    NodeStatus::Ready(node) => node,
                    _ => unreachable!("Node must be ready"),
                };
                let new = func(old);
                *node = NodeStatus::Ready(new);
            }
            _ => unreachable!("Node must be ready"),
        }
    }

    pub fn get(&self, id: NodeId) -> &Node {
        &self.storage[id]
    }

    pub fn get_mut(&mut self, id: NodeId) -> &mut Node {
        &mut self.storage[id]
    }

    pub fn error(&mut self, id: NodeId, error: Arc<anyhow::Error>) {
        match &mut self.storage[id] {
            node @ Node::Open { .. } => *node = Node::Err(error),
            _ => panic!("Attemted to set an error on a node that wasn't open"),
        }
    }

    pub fn upgrade_desc(&self, desc: NodeDesc) -> Option<NodeId> {
        self.storage.upgrade_desc(desc)
    }
}
