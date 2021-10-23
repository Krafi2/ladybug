use super::{registry::Registry, Topic, TopicId};
use crate::config::Config;
use anyhow::Result;

pub trait TopicFactory {
    fn new_topic(&mut self, id: TopicId) -> Result<Topic>;
}

pub struct StandardFactory<'a> {
    pub registry: &'a mut Registry,
    pub config: &'a Config,
}

impl<'a> StandardFactory<'a> {
    pub fn new(registry: &'a mut Registry, config: &'a Config) -> Self {
        Self { registry, config }
    }
}

impl<'a> TopicFactory for StandardFactory<'a> {
    fn new_topic(&mut self, id: TopicId) -> Result<Topic> {
        let desc = self.registry[id].clone();
        Topic::from_desc(self.registry, self.config, desc)
    }
}
