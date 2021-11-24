mod deploy;
pub mod registry;
pub use deploy::{Env, TemplateContext};
use globwalk::GlobWalker;

use crate::{
    config::{paths, Config},
    glob::GlobBuilder,
};
use anyhow::{anyhow, Context, Result};
use figment::providers::{Format, Toml};
use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    fmt::Display,
    path::{Path, PathBuf},
};

use self::registry::{Registry, Storage, TopicId};
type Dict = toml::map::Map<String, toml::Value>;

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct TopicConfig {
    /// The directory into which to deploy files
    target: PathBuf,
    /// Topics to deploy before this one
    dependencies: Vec<String>,
    /// A command to run before deploying
    pre_hook: Vec<String>,
    /// A command to run after deploying
    post_hook: Vec<String>,
    /// A list of globs to ignore files
    ignore: Vec<String>,
    /// A list of globs for files to template using tera
    template: Vec<String>,
    /// What to do if a file already exists
    duplicates: Duplicates,
    /// These values are injected into the tera enviroment
    env: Dict,
    /// The values are exported from the enviroment for other topics to use
    export: Vec<String>,
}

impl Default for TopicConfig {
    fn default() -> Self {
        Self {
            target: paths::user_config(),
            dependencies: Vec::new(),
            pre_hook: Vec::new(),
            post_hook: Vec::new(),
            ignore: Vec::new(),
            template: Vec::new(),
            duplicates: Duplicates::Rename,
            env: Dict::default(),
            export: Vec::new(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TopicDesc(
    /// Path to the topic configuration file
    PathBuf,
);

impl TopicDesc {
    pub fn new(path: &Path) -> Result<Self> {
        if path.ends_with("ladybug.toml") {
            Ok(Self(path.to_owned()))
        } else {
            Err(anyhow!(
                "Path doesn't point to a topic configuration file: {}",
                path.to_string_lossy()
            ))
        }
    }
    /// Path to the topic's configuration file
    pub fn config(&self) -> &Path {
        self.0.as_path()
    }

    /// Path to the topic's directory
    pub fn dir(&self) -> &Path {
        self.0.parent().expect("Topic path has no parent")
    }

    /// Get the topic's name
    pub fn name<'a>(&'a self) -> impl Display + 'a {
        self.dir().display()
    }
}

#[derive(Debug)]
pub struct Topic {
    id: TopicId,
    dir: PathBuf,
    target: PathBuf,
    dependencies: Vec<TopicId>,
    links: GlobBuilder,
    template: TemplateContext,
    duplicates: Duplicates,
    pre_hook: Vec<String>,
    post_hook: Vec<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
pub enum Duplicates {
    Delete,
    Keep,
    Rename,
}

impl Topic {
    fn new(
        registry: &mut DescRegistry,
        desc: &TopicDesc,
        config: TopicConfig,
        global: &Config,
    ) -> Result<Self> {
        let dependencies = find_topics(
            registry,
            &global.dotfile_dir,
            desc.dir(),
            config.dependencies.iter().map(AsRef::as_ref),
        )
        .context("Failed to construct dependency iterator")?
        .collect::<Result<Vec<_>>>()
        .context("Failed to collect dependencies")?;

        let dir = desc.dir().to_owned();
        let id = registry.new_id(&desc);

        let templates = GlobBuilder::new(dir.clone())
            .extend(&config.template, true)
            .extend(&config.ignore, false);

        let links = GlobBuilder::new(dir.clone())
            .extend(&config.template, false)
            .extend(&config.ignore, false);

        Ok(Topic {
            links,
            id,
            dir,
            target: config.target,
            dependencies,
            template: TemplateContext::new(templates, config.env, config.export),
            duplicates: config.duplicates,
            pre_hook: config.pre_hook,
            post_hook: config.post_hook,
        })
    }

    pub fn from_desc(
        registry: &mut DescRegistry,
        config: &Config,
        desc: &TopicDesc,
    ) -> Result<Self> {
        let t_config = config
            .topic_config
            .clone()
            .merge(Toml::file(desc.config()))
            .extract()
            .context("Failed to load config")?;
        Self::new(registry, desc, t_config, config)
    }

    pub fn import(&mut self, env: &Env) {
        self.template.extend(env)
    }

    pub fn dependencies(&self) -> &[TopicId] {
        &self.dependencies
    }

    pub fn id(&self) -> &TopicId {
        &self.id
    }

    /// Get a reference to the topic's dir.
    pub fn dir(&self) -> &PathBuf {
        &self.dir
    }
}

#[derive(Default)]
pub struct DescRegistry {
    registry: Registry,
    storage: Storage<TopicDesc>,
    map: HashMap<TopicDesc, TopicId>,
}

impl DescRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn new_id(&mut self, desc: &TopicDesc) -> TopicId {
        match self.map.get(desc) {
            Some(id) => *id,
            None => {
                let id = self.registry.new_id();
                self.map.insert(desc.clone(), id);
                let _ = self.storage.get_handle(id, || desc.clone());
                id
            }
        }
    }

    pub fn get_desc(&self, id: TopicId) -> &TopicDesc {
        let handle = self.storage.try_get_handle(id).expect("Invalid TopicId");
        &self.storage[handle]
    }
}

struct TopicIter<'a> {
    registry: &'a mut DescRegistry,
    topic: GlobWalker,
    root: GlobWalker,
}

impl<'a> Iterator for TopicIter<'a> {
    type Item = Result<TopicId>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.root.next().or_else(|| self.topic.next()) {
                Some(Ok(entry)) => {
                    let id = TopicDesc::new(entry.path())
                        .context("Failed to construct TopicId")
                        .map(|desc| self.registry.new_id(&desc));
                    return Some(id);
                }
                // The walker may return errors if it tries to search folders that are inaccesible
                // due to filesystem permissions, which we can safely log and ignore.
                Some(Err(err)) => {
                    log::info!("Walker encountered an error: {}", err);
                }
                None => return None,
            }
        }
    }
}

pub fn find_topics<'a, 'b, I>(
    registry: &'a mut DescRegistry,
    root: &Path,
    topic: &Path,
    globs: I,
) -> Result<impl Iterator<Item = Result<TopicId>> + 'a>
where
    I: IntoIterator<Item = &'b str>,
{
    let mut root_glob = GlobBuilder::new(root.to_owned());
    let mut topic_glob = GlobBuilder::new(topic.to_owned());

    for s in globs {
        let mut s = s.to_owned();
        if !s.ends_with("/") {
            s.push('/');
        }
        s.push_str("ladybug.toml");

        // Consider globs starting with '/' as relative to dotfile root
        if s.starts_with('/') {
            root_glob = root_glob.add(s, true);
        } else {
            topic_glob = topic_glob.add(s, true);
        }
    }

    let root = root_glob.build().context("Failed to construct root glob")?;
    let topic = topic_glob.build().context("Failed to build topic glob")?;

    Ok(TopicIter {
        registry,
        topic,
        root,
    })
}
