use super::{Topic, TopicId};
use crate::config::Config;
use anyhow::Result;

pub trait TopicFactory {
    fn new_topic(&mut self, id: TopicId) -> Result<Topic>;
}

pub struct StandardFactory<'a> {
    config: &'a Config,
}

impl<'a> StandardFactory<'a> {
    pub fn new(config: &'a Config) -> Self {
        Self { config }
    }
}

impl<'a> TopicFactory for StandardFactory<'a> {
    fn new_topic(&mut self, id: TopicId) -> Result<Topic> {
        Topic::from_id(id, self.config)
    }
}
