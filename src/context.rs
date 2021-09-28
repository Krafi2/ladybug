use crate::{config::Config, resolver::Resolver, topic::Env};

pub struct Context<'a> {
    pub config: &'a Config,
    pub env: &'a Env,
}
