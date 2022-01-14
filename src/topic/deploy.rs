use super::{Dict, Duplicates, Topic};
use crate::{config::Config, glob::GlobBuilder};
use anyhow::{anyhow, Context, Result};
use globwalk::GlobWalker;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};
use tera::Tera;

#[derive(Debug, Clone)]
pub struct Env {
    context: tera::Context,
}

impl Topic {
    pub fn deploy(self, dry_run: bool, config: &Config) -> Result<Env> {
        if !dry_run {
            log::info!("Running pre-hook");
            self.hook
                .pre
                .map(|hook| hook.run(&self.dir, &self.target, &config.shell))
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
            log::info!("Running post-hook");
            self.hook
                .post
                .map(|hook| hook.run(&self.dir, &self.target, &config.shell))
                .transpose()
                .context("Failed to run post-hook")?;
        }

        Ok(env)
    }
}

fn walk_files(walker: GlobWalker) -> impl Iterator<Item = PathBuf> {
    walker.filter_map(|entry| match entry {
        Ok(entry) => {
            let path = entry.path();
            path.is_file().then(|| path.to_owned())
        }
        Err(err) => {
            log::warn!("Walker error: {}", err);
            None
        }
    })
}

fn deploy_links(
    globs: GlobBuilder,
    target: &Path,
    duplicates: Duplicates,
    dry_run: bool,
) -> Result<()> {
    log::info!("Linking files");
    let cur_dir = globs.base().to_owned();
    let walker = globs.build().context("Failed to build GlobWalker")?;

    for path in walk_files(walker) {
        let target =
            crate::fs::rebase_path(&path, &cur_dir, target).expect("Failed to rebase path");

        log::debug!(
            "Creating link '{}' -> '{}'",
            path.display(),
            target.display(),
        );
        if !dry_run {
            crate::fs::place_symlink(&path, &target, duplicates).with_context(|| {
                format!(
                    "Failed to create symlink: '{}' -> '{}'",
                    path.display(),
                    target.display(),
                )
            })?;
        }
    }

    Ok(())
}

#[derive(Debug)]
pub struct TemplateContext {
    context: tera::Context,
    env: Dict,
    export: Vec<String>,
    templates: GlobBuilder,
}

impl TemplateContext {
    pub(super) fn new(templates: GlobBuilder, env: crate::topic::EnvConfig) -> Self {
        let export = env.public.keys().cloned().collect();
        let env = env.private.into_iter().chain(env.public).collect();

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
        log::info!("Rendering templates");

        let mut context = self.context;
        for (key, value) in self.env {
            context.insert(key, &value);
        }

        // Map templates to their target paths. I would love to do this more efficiently but
        // tera only allows you to refer to templatess via strings.
        let mut path_map = HashMap::<String, PathBuf>::new();
        let dir = self.templates.base().to_owned();
        let walker = self
            .templates
            .build()
            .context("Failed to construct GlobWalker")?;

        let templates = walk_files(walker).map(|path| {
            let target =
                crate::fs::rebase_path(&path, &dir, target).expect("Failed to rebase path");
            log::debug!(
                "Rendering template '{}' to '{}'",
                path.display(),
                target.display()
            );
            let name = path
                .strip_prefix(&dir)
                .expect("Failed to strip prefix")
                .display()
                .to_string();

            path_map.insert(name.clone(), target);

            (path, Some(name))
        });

        let mut tera = Tera::default();
        tera.add_template_files(templates)?;

        for (name, target) in path_map {
            let render = tera
                .render(&name, &context)
                .with_context(|| format!("Failed to render template: '{}'", &name))?;

            if !dry_run {
                crate::fs::place_file(&target, render.as_bytes(), duplicates).with_context(
                    || {
                        format!(
                            "Failed to write template '{}' to '{}'",
                            dir.join(name).display(),
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
