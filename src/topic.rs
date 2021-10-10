use crate::{
    config::{paths, Config},
    glob,
};
use anyhow::{anyhow, Context, Error, Result};
use figment::{
    providers::{Format, Toml},
    Figment,
};
use ignore::{
    overrides::{Override, OverrideBuilder},
    DirEntry,
};
use serde::{Deserialize, Serialize};
use std::{
    borrow::Cow,
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
};
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
    pub fn dir(&self) -> &Path {
        self.0.parent().expect("Topic path has no parent")
    }
    pub fn name(&self) -> Cow<str> {
        self.path().to_string_lossy()
    }
}

#[derive(Debug, Clone)]
pub struct Topic {
    id: TopicId,
    target: PathBuf,
    dependencies: Vec<TopicId>,
    links: Override,
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

    fn new(topic: TopicId, config: TopicConfig, global: &Config) -> Result<Self> {
        let dependencies = find_topics(
            &global.dotfile_dir,
            config.dependencies.iter().map(AsRef::as_ref),
        )
        .context("Failed to construct dependency iterator")?
        .collect::<Result<Vec<_>>>()
        .context("Failed to collect dependencies")?;

        let dir = topic.dir();
        let templates = Self::make_glob(dir, &config.ignore, &config.template)?;
        let template = TemplateContext::new(templates, config.env, config.export);

        Ok(Topic {
            links: Self::make_glob(dir, &config.ignore, &[])?,
            id: topic,
            target: config.target,
            dependencies,
            template,
            duplicates: config.duplicates,
            pre_hook: config.pre_hook,
            post_hook: config.post_hook,
        })
    }

    pub fn from_id(topic: TopicId, config: &Config) -> Result<Self> {
        let dir = topic.dir();
        let t_config = config
            .topic_config
            .clone()
            .merge(Toml::file(topic.path()))
            .extract()
            .context("Failed to load config")?;
        Self::new(topic, t_config, config)
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
}

mod deploy {
    use std::{
        io::stderr,
        path::Path,
        process::{Command, ExitStatus, Stdio},
    };

    use super::{Duplicates, Env, TemplateContext, Topic};
    use anyhow::{anyhow, Context, Result};
    use ignore::overrides::Override;

    fn deploy_links(
        globs: Override,
        target: &Path,
        duplicates: Duplicates,
        dry_run: bool,
    ) -> Result<()> {
        let cur_dir = globs.path().to_owned();
        for entry in crate::glob::new_walker(globs).build() {
            match entry {
                Ok(entry) => {
                    let path = entry.path();
                    match path.is_file() {
                        true => {
                            let target = crate::fs::rebase_path(path, &cur_dir, target)
                                .expect("Failed to rebase path");

                            if !dry_run {
                                crate::fs::place_symlink(path, &target, duplicates).with_context(
                                    || {
                                        format!(
                                            "Failed to symlink dotfile: '{}' -> '{}'",
                                            path.display(),
                                            target.display()
                                        )
                                    },
                                )?;
                            }
                        }
                        false => {
                            todo!("Implement logging");
                        }
                    }
                }
                Err(_) => {
                    todo!("Implement logging!");
                }
            };
        }
        Ok(())
    }

    fn run_cmd(
        dir: &Path,
        cmd: &[String],
        stdin: Stdio,
        stdout: Stdio,
        stderr: Stdio,
    ) -> Option<Result<ExitStatus>> {
        let cmd_ = cmd;
        // Make `cmd` `None` if the slice is empty, or `(cmd, None)` if the commands is available
        // but there are no args, or finally `(cmd, args)` if both are available.
        let cmd = cmd
            .split_first()
            .map(|(cmd, args)| (cmd, Some(args)))
            .or_else(|| cmd.first().map(|first| (first, None)));

        cmd.map(|(cmd, args)| {
            let args = args.unwrap_or(&[]);
            Command::new(cmd)
                .args(args)
                .current_dir(dir)
                .stdin(stdin)
                .stdout(stdout)
                .stderr(stderr)
                .status()
                .map_err(anyhow::Error::new)
                .and_then(|status| match status.success() {
                    true => Ok(status),
                    false => Err(anyhow!("Process exited with status: {}", status)),
                })
                .with_context(|| format!("Failed to run command: '{:?}'", cmd_))
        })
    }

    fn run_hook(dir: &Path, cmd: &[String]) -> Option<Result<ExitStatus>> {
        let stdin = Stdio::inherit();
        let stdout = Stdio::inherit();
        let stderr = Stdio::inherit();

        run_cmd(dir, cmd, stdin, stdout, stderr)
    }

    impl Topic {
        pub fn deploy(self, dry_run: bool) -> Result<Env> {
            let dir = self.id.dir();

            if !dry_run {
                run_hook(dir, &self.pre_hook)
                    .transpose()
                    .context("Failed to run pre-hook")?;
            }

            deploy_links(self.links, &self.target, self.duplicates, dry_run)
                .context("Failed to deploy symlinks")?;

            let env = self
                .template
                .render(&self.target, self.duplicates, dry_run)
                .context("Failed to deploy templates")?;

            if !dry_run {
                run_hook(dir, &self.post_hook)
                    .transpose()
                    .context("Failed to run post-hook")?;
            }

            Ok(env)
        }
    }
}

pub use env::{Env, TemplateContext};
mod env {
    use super::{Dict, Duplicates};
    use anyhow::{anyhow, Context, Result};
    use ignore::overrides::Override;
    use std::{
        borrow::Cow,
        collections::HashMap,
        fs::File,
        path::{Path, PathBuf},
    };
    use tera::Tera;
    use toml::{value::Map, Value};

    #[derive(Debug, Clone)]
    pub struct TemplateContext {
        context: tera::Context,
        env: Dict,
        export: Vec<String>,
        templates: Override,
    }

    impl TemplateContext {
        pub(super) fn new(templates: Override, env: Dict, export: Vec<String>) -> Self {
            Self {
                context: tera::Context::new(),
                env,
                export,
                templates,
            }
        }

        pub fn extend(&mut self, env: &Env) {
            self.context.extend(env.context.clone())
        }

        pub(super) fn render(
            self,
            target: &Path,
            duplicates: Duplicates,
            dry_run: bool,
        ) -> Result<Env> {
            let mut context = self.context;
            for (key, value) in self.env {
                context.insert(key, &value);
            }

            let mut id = 0_usize;
            // Map templates to their target paths. I would love to do this more efficiently but
            // tera allows you to refer to templatess only via strings.
            let mut path_map = HashMap::<String, PathBuf>::new();

            let cur_dir = self.templates.path().to_owned();
            let templates = crate::glob::new_walker(self.templates)
                .build()
                .filter_map(|entry| match entry {
                    Ok(entry) => {
                        let path = entry.path();
                        match path.is_file() {
                            true => {
                                let target = crate::fs::rebase_path(path, &cur_dir, target)
                                    .expect("Failed to rebase path");
                                let name = path
                                    .strip_prefix(&cur_dir)
                                    .expect("Failed to strip prefix")
                                    .display()
                                    .to_string();
                                path_map.insert(name.clone(), target);
                                Some((path.to_owned(), Some(name)))
                            }
                            false => {
                                todo!("Implement logging");
                                None
                            }
                        }
                    }
                    Err(_) => {
                        todo!("Implement logging!");
                        None
                    }
                });

            let mut tera = Tera::default();
            tera.add_template_files(templates);

            for (name, target) in path_map {
                let render = tera
                    .render(&name, &context)
                    .with_context(|| format!("Failed to render template: '{}'", &name))?;

                if !dry_run {
                    crate::fs::place_file(&target, render.as_bytes(), duplicates).with_context(
                        || {
                            format!(
                                "Failed to write template '{}' to '{}'",
                                cur_dir.join(name).display(),
                                target.display()
                            )
                        },
                    )?;
                }
            }

            let mut env = tera::Context::new();
            for var in self.export {
                let val = context
                    .get(&var)
                    .ok_or_else(|| anyhow!("Value not found in env"))
                    .with_context(|| format!("Failed to export value: '{}'", &var))?;
                env.insert(&var, val)
            }

            Ok(Env { context: env })
        }
    }

    #[derive(Debug, Clone)]
    pub struct Env {
        context: tera::Context,
    }

    impl Env {
        pub(super) fn new(context: tera::Context) -> Self {
            Self { context }
        }
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
        Ok(entry) => TopicId::new(entry.path())
            .with_context(|| anyhow!("Failed to construct TopicId: '{}'", entry.path().display())),
        Err(err) => Err(err).context("Directory walker encountered an error"),
    }))
}
