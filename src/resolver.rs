use crate::topic::{factory::TopicFactory, registry::TopicId, Env, Topic, TopicDesc};
use anyhow::{anyhow, Context};
use std::{
    collections::hash_map::{DefaultHasher, HashMap},
    hash::{BuildHasherDefault, Hash, Hasher},
    sync::Arc,
};

#[derive(Debug)]
pub enum NodeErr {
    /// A dependency couldn't be satisfied
    Unsatisfied(NodeId),
    /// This node is a part of a cycle
    Cycle(NodeId),
    /// Something is wrong with this node
    Custom(anyhow::Error),
}

#[derive(Debug)]
pub enum Node {
    Open(Box<Topic>),
    Closed(Env),
    Err(NodeErr),
}

pub use storage::NodeId;
mod storage {
    use super::Node;
    use crate::topic::registry::{self, Handle, TopicId};
    use std::{
        collections::HashMap,
        ops::{Index, IndexMut},
    };

    /// Describes a node that is known to be initialized
    #[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
    pub struct NodeId(Handle<Node>);

    impl Into<TopicId> for NodeId {
        fn into(self) -> TopicId {
            self.0.into()
        }
    }

    #[derive(Debug, Default)]
    pub(super) struct Storage {
        inner: registry::Storage<Node>,
    }

    impl Storage {
        pub fn get_id<F>(&mut self, id: TopicId, func: F) -> NodeId
        where
            F: FnOnce() -> Node,
        {
            let handle = self.inner.get_handle(id, func);
            NodeId(handle)
        }

        pub fn get(&self, id: NodeId) -> &Node {
            &self.inner[id.0]
        }

        pub fn get_mut(&mut self, id: NodeId) -> &mut Node {
            &mut self.inner[id.0]
        }

        pub fn replace<F>(&mut self, id: NodeId, func: F)
        where
            F: FnOnce(Node) -> Node,
        {
            let node = self.inner.get_raw_mut(id.0);
            let old = std::mem::replace(node, None);
            std::mem::replace(node, Some(func(old.expect("Node is absent"))));
        }
    }
}

#[derive(Debug, Default)]
pub struct Resolver {
    storage: storage::Storage,
}

impl Resolver {
    pub fn new() -> Self {
        Self::default()
    }

    fn new_node<T: TopicFactory>(id: TopicId, factory: &mut T) -> Node {
        match factory.new_topic(id) {
            Ok(topic) => Node::Open(Box::new(topic)),
            Err(error) => Node::Err(NodeErr::Custom(error)),
        }
    }

    pub fn open<T: TopicFactory>(&mut self, id: TopicId, factory: &mut T) -> NodeId {
        self.storage.get_id(id, || Self::new_node(id, factory))
    }

    pub fn get(&self, id: NodeId) -> &Node {
        self.storage.get(id)
    }

    pub fn get_mut(&mut self, id: NodeId) -> &mut Node {
        self.storage.get_mut(id)
    }

    pub fn replace<F>(&mut self, id: NodeId, func: F)
    where
        F: FnOnce(Node) -> Node,
    {
        self.storage.replace(id, func)
    }

    pub fn error(&mut self, id: NodeId, error: NodeErr) {
        match self.storage.get_mut(id) {
            node @ Node::Open { .. } => *node = Node::Err(error),
            _ => panic!("Attemted to set an error on a node that wasn't open"),
        }
    }
}
