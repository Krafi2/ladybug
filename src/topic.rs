pub mod deploy;
pub mod deployer;
pub mod factory;
pub mod registry;

pub use deploy::{Env, TemplateContext};

use crate::{
    config::{paths, Config},
    glob::GlobBuilder,
};
use anyhow::{anyhow, Context, Error, Result};
use figment::{
    providers::{Format, Toml},
    Figment,
};
use globwalk::GlobWalker;
use serde::{Deserialize, Serialize};
use std::{
    borrow::Cow,
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
};

use self::registry::{Registry, TopicId};
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
            target: paths::config_dir(),
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
pub struct TopicDesc(PathBuf);

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
    pub fn path(&self) -> &Path {
        self.0.as_path()
    }
    pub fn dir(&self) -> &Path {
        self.0.parent().expect("Topic path has no parent")
    }
    pub fn name(&self) -> std::path::Display {
        self.dir().display()
    }
}

#[derive(Debug)]
pub struct Topic {
    id: TopicId,
    name: String,
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
        registry: &mut Registry,
        desc: TopicDesc,
        config: TopicConfig,
        global: &Config,
    ) -> Result<Self> {
        let dependencies = find_topics(
            registry,
            &global.dotfile_dir,
            config.dependencies.iter().map(AsRef::as_ref),
        )
        .context("Failed to construct dependency iterator")?
        .collect::<Result<Vec<_>>>()
        .context("Failed to collect dependencies")?;

        let dir = desc.dir().to_owned();
        let name = desc.name().to_string();
        let id = registry.register(desc);

        let mut templates = GlobBuilder::new(dir.clone())
            .extend(&config.template, true)
            .extend(&config.ignore, false);

        let mut links = GlobBuilder::new(dir.clone())
            .extend(&config.template, false)
            .extend(&config.ignore, false);

        Ok(Topic {
            links,
            id,
            name,
            dir,
            target: config.target,
            dependencies,
            template: TemplateContext::new(templates, config.env, config.export),
            duplicates: config.duplicates,
            pre_hook: config.pre_hook,
            post_hook: config.post_hook,
        })
    }

    pub fn from_desc(registry: &mut Registry, config: &Config, desc: TopicDesc) -> Result<Self> {
        let dir = desc.dir();
        let t_config = config
            .topic_config
            .clone()
            .merge(Toml::file(desc.path()))
            .extract()
            .context("Failed to load config")?;
        Self::new(registry, desc, t_config, config)
    }

    pub fn dependencies(&self) -> &[TopicId] {
        &self.dependencies
    }

    pub fn id(&self) -> &TopicId {
        &self.id
    }

    pub fn import(&mut self, env: &Env) {
        self.template.extend(env)
    }

    /// Get a reference to the topic's dir.
    pub fn dir(&self) -> &PathBuf {
        &self.dir
    }

    /// Get a reference to the topic's name.
    pub fn name(&self) -> &str {
        self.name.as_str()
    }
}

pub fn find_topics<'a, 'b, I>(
    registry: &'a mut Registry,
    root: &Path,
    globs: I,
) -> Result<impl Iterator<Item = Result<TopicId>> + 'a>
where
    I: IntoIterator<Item = &'b str>,
{
    let mut builder = GlobBuilder::new(root.to_owned());
    for s in globs {
        let mut s = s.to_owned();
        if !s.ends_with("/") {
            s.push('/');
        }
        s.push_str("ladybug.toml");
        builder = builder.add(s, true);
    }

    let iter = builder
        .build()
        .context("Failed to construct GlobWalker")?
        .filter_map(move |r| match r {
            Ok(entry) => {
                let id = TopicDesc::new(entry.path())
                    .context("Failed to construct TopicId")
                    .map(|desc| registry.register(desc));
                Some(id)
            }
            // The walker may return errors if it tries to search folders that are inaccesible due to
            // filesystem permissions, which we can safely log and ignore.
            Err(err) => {
                log::info!("Walker encountered an error: {}", err);
                None
            }
        });
    Ok(iter)
}
