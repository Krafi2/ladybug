use crate::config::Config;
use crate::resolver::Resolver;

pub struct Context {
    pub config: Config,
    pub resolver: Resolver,
}
