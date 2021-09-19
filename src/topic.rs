use crate::{config::Config, env::Env, glob};
use anyhow::{anyhow, Context, Error, Result};
use ignore::{
    overrides::{Override, OverrideBuilder},
    DirEntry,
};
use serde::Deserialize;
use std::{
    borrow::Cow,
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
    pub fn path(&self) -> &Path {
        self.0.as_path()
    }
    pub fn name(&self) -> Cow<str> {
        self.path().to_string_lossy()
    }
}

#[derive(Debug, Clone)]
pub struct Topic {
    id: TopicId,
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
    fn make_ignore<'a, I>(root: &Path, globs: I) -> Result<OverrideBuilder>
    where
        I: IntoIterator<Item = &'a str>,
    {
        let mut builder = OverrideBuilder::new(root);
        for s in globs.into_iter() {
            builder
                .add(["!", s].join("").as_str())
                .context("Falied to construct glob")?;
        }
        Ok(builder)
    }

    fn make_glob(root: &Path, ignore: &[String], globs: &[String]) -> Result<Override> {
        let mut builder = Self::make_ignore(root, ignore.iter().map(AsRef::as_ref))?;
        glob::extend_glob(&mut builder, globs.iter().map(AsRef::as_ref))
    }

    fn new(topic: TopicId, conf: TopicConfig, global: &Config) -> Result<Self> {
        let dependencies = find_topics(
            &global.dotfile_dir,
            conf.dependencies.iter().map(AsRef::as_ref),
        )
        .context("Failed to construct dependency iterator")?
        .collect::<Result<Vec<_>>>()
        .context("Failed to collect dependencies")?;

        let path = &topic.0;
        let root = path
            .parent()
            .ok_or_else(|| anyhow!("Path too short: {}", path.to_string_lossy()))?;

        Ok(Topic {
            root_dir: conf.root_dir,
            dependencies,
            links: Self::make_glob(root, &conf.ignore, &[])?,
            templates: Self::make_glob(root, &conf.ignore, &conf.template)?,
            pre_hook: conf.pre_hook,
            post_hook: conf.post_hook,
            private: conf.private,
            public: conf.public,
            id: topic,
        })
    }

    pub fn from_id(topic: TopicId, conf: &Config) -> Result<Self> {
        let path = &topic.0;
        let root = path
            .parent()
            .ok_or_else(|| anyhow!("Path too short: {}", path.to_string_lossy()))?;
        let string = fs::read_to_string(path).context("Failed to read topic config")?;
        let t_conf = toml::from_str(&string).context("Couldn't parse config")?;
        Self::new(topic, t_conf, conf)
    }

    pub fn dependencies(&self) -> &[TopicId] {
        &self.dependencies
    }

    pub fn deploy(&self, env: &Env) -> Result<Env> {
        todo!()
    }

    pub fn id(&self) -> &TopicId {
        &self.id
    }
}

pub fn find_topics<'a, I>(root: &Path, globs: I) -> Result<impl Iterator<Item = Result<TopicId>>>
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
            .with_context(|| anyhow!("Failed to construct glob: '{}'", &s))?;
    }
    let overrides = builder.build().context("Failed to construct globset")?;

    Ok(glob::new_walker(overrides).build().map(|r| match r {
        Ok(entry) => TopicId::new(entry.path()).with_context(|| {
            anyhow!(
                "Failed to construct TopicId: '{}'",
                entry.path().to_string_lossy()
            )
        }),
        Err(err) => Err(err).context("Directory walker encountered an error"),
    }))
}
