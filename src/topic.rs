use crate::{config::Config, glob};
use anyhow::{anyhow, Context, Error, Result};
use ignore::{
    overrides::{Override, OverrideBuilder},
    DirEntry,
};
use serde::Deserialize;
use std::{
    fs,
    path::{Path, PathBuf},
};

#[derive(Deserialize, Debug, Clone)]
pub struct TopicConfig {
    root_dir: PathBuf,
    dependencies: Vec<String>,
    pre_hook: Option<String>,
    post_hook: Option<String>,
    #[serde(default = "default_value")]
    private: toml::Value,
    #[serde(default = "default_value")]
    public: toml::Value,
    ignore: Vec<String>,
    template: Vec<String>,
}

fn default_value() -> toml::Value {
    std::collections::BTreeMap::<String, toml::Value>::new().into()
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TopicId(PathBuf);

impl TopicId {
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
}

#[derive(Debug, Clone)]
pub struct Topic {
    root_dir: PathBuf,
    dependencies: Vec<TopicId>,
    links: Override,
    templates: Override,
    pre_hook: Option<String>,
    post_hook: Option<String>,
    private: toml::Value,
    public: toml::Value,
}

impl Topic {
    fn make_dependencies<'a, I>(root: &Path, globs: I) -> Result<Vec<TopicId>>
    where
        I: IntoIterator<Item = &'a str>,
    {
        let mut builder = OverrideBuilder::new(root);
        for s in globs {
            let mut s = s.to_owned();
            if !s.ends_with("/") {
                s.push('/');
            }
            s.push_str("ladybug.toml");
            builder
                .add(&s)
                .with_context(|| anyhow!("Failed to construct globs"))?;
        }
        let overrides = builder
            .build()
            .with_context(|| anyhow!("Failed to construct globset"))?;

        glob::new_walker(overrides)
            .build()
            .map(|r| match r {
                Ok(e) => {
                    TopicId::new(e.path()).with_context(|| anyhow!("Failed to construct TopicId"))
                }
                Err(e) => Err(Error::new(e))
                    .with_context(|| anyhow!("Directory walker encountered an error")),
            })
            .collect::<Result<Vec<_>>>()
    }

    fn make_ignore<'a, I>(root: &Path, globs: I) -> Result<OverrideBuilder>
    where
        I: IntoIterator<Item = &'a str>,
    {
        let mut builder = OverrideBuilder::new(root);
        for s in globs.into_iter() {
            builder
                .add(["!", s].join("").as_str())
                .with_context(|| anyhow!("Falied to construct glob"))?;
        }
        Ok(builder)
    }

    fn make_glob(root: &Path, ignore: &[String], globs: &[String]) -> Result<Override> {
        let mut builder = Self::make_ignore(root, ignore.iter().map(AsRef::as_ref))?;
        glob::extend_glob(&mut builder, globs.iter().map(AsRef::as_ref))
    }

    fn new(conf: TopicConfig, global: &Config, root: &Path) -> Result<Self> {
        Ok(Topic {
            root_dir: conf.root_dir,
            dependencies: Self::make_dependencies(
                &global.dotfile_dir,
                conf.dependencies.iter().map(AsRef::as_ref),
            )?,
            links: Self::make_glob(root, &conf.ignore, &[])?,
            templates: Self::make_glob(root, &conf.ignore, &conf.template)?,
            pre_hook: conf.pre_hook,
            post_hook: conf.post_hook,
            private: conf.private,
            public: conf.public,
        })
    }

    pub fn from_id(topic: TopicId, conf: &Config) -> Result<Self> {
        let path = topic.0;
        let root = path
            .parent()
            .ok_or_else(|| anyhow!("Path too short: {}", path.to_string_lossy()))?;
        let string =
            fs::read_to_string(&path).with_context(|| anyhow!("Failed to read topic config"))?;
        let t_conf = toml::from_str(&string).with_context(|| anyhow!("Couldn't parse config"))?;
        Self::new(t_conf, conf, root)
    }

    pub fn dependencies(&self) -> &[TopicId] {
        &self.dependencies
    }

    fn deploy(&self) -> Result<()> {
        todo!()
    }
}
