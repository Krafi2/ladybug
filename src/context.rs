use figment::Figment;

use crate::{config::Config, resolver::Resolver, topic::Env};

pub struct Context {
    config: Config,
}

impl Context {
    /// Get a reference to the context's config.
    pub fn config(&self) -> &Config {
        &self.config
    }

    /// Get a reference to the context's topic.
    pub fn topic(&self) -> &Figment {
        &self.topic
    }
}
