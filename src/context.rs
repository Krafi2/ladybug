use crate::config::Config;
use crate::env::Env;
use crate::resolver::Resolver;

pub struct Context<'a> {
    pub config: &'a Config,
    pub env: &'a Env,
}
