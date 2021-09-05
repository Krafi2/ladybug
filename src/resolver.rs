use crate::{
    config::Config,
    topic::{Topic, TopicId},
};
use anyhow::{anyhow, Context, Error};
use std::{
    collections::hash_map::{DefaultHasher, HashMap},
    hash::{BuildHasherDefault, Hash, Hasher},
};

#[derive(Debug)]
pub enum Node {
    Open {
        children: Vec<NodeId>,
        topic: Box<Topic>,
    },
    Closed,
    Err(Error),
}

#[derive(Debug)]
enum NodeStatus {
    Ready(Node),
    Waiting(TopicId),
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct NodeId(usize);

#[derive(Debug)]
pub struct Resolver {
    id_map: HashMap<TopicId, NodeId>,
    nodes: Vec<NodeStatus>,
}

impl Resolver {
    fn insert_node(&mut self, topic: TopicId, node: NodeStatus) -> NodeId {
        let idx = self.nodes.len();
        let id = NodeId(idx);
        if let Some(_) = self.id_map.insert(topic, id) {
            panic!("Node already exists")
        }
        self.nodes.push(node);
        id
    }

    fn get_or_insert_node_with<F>(&mut self, topic: TopicId, func: F) -> NodeId
    where
        F: FnOnce(&mut Self) -> NodeStatus,
    {
        match self.id_map.get(&topic) {
            Some(id) => *id,
            None => {
                let node = func(self);
                self.insert_node(topic, node)
            }
        }
    }

    fn new_node(_self: &mut Self, topic_id: TopicId, config: &Config) -> NodeStatus {
        match Topic::from_id(topic_id, config) {
            Ok(topic) => {
                let children = topic
                    .dependencies()
                    .iter()
                    .map(|id| {
                        let id_ = id.clone();
                        _self.get_or_insert_node_with(id.clone(), |_| NodeStatus::Waiting(id_))
                    })
                    .collect::<Vec<_>>();
                NodeStatus::Ready(Node::Open {
                    children,
                    topic: Box::new(topic),
                })
            }
            Err(error) => NodeStatus::Ready(Node::Err(error)),
        }
    }

    pub fn open(&mut self, topic_id: TopicId, config: &Config) -> NodeId {
        self.get_or_insert_node_with(topic_id.clone(), |_self| {
            Self::new_node(_self, topic_id.clone(), &config)
        })
    }

    pub fn get<'a>(&'a mut self, id: NodeId, config: &Config) -> &'a Node {
        if let NodeStatus::Waiting(topic) = &self.nodes[id.0] {
            let topic = topic.clone();
            let ready_node = Self::new_node(self, topic.clone(), config);
            self.nodes[id.0] = ready_node;
        }

        match &self.nodes[id.0] {
            NodeStatus::Ready(node) => node,
            _ => panic!("Node should be ready"),
        }
    }

    pub fn close(&mut self, id: NodeId) {
        match &mut self.nodes[id.0] {
            NodeStatus::Ready(node @ Node::Open { .. }) => *node = Node::Closed,
            _ => panic!("Attemted to close a node that wasn't open"),
        }
    }

    pub fn error(&mut self, id: NodeId, error: Error) {
        match &mut self.nodes[id.0] {
            NodeStatus::Ready(node @ Node::Open { .. }) => *node = Node::Err(error),
            _ => panic!("Attemted to sett an error on a node that wasn't open"),
        }
    }
}
