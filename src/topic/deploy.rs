use super::{Dict, Duplicates, Topic};
use crate::glob::GlobBuilder;
use anyhow::{anyhow, Context, Result};
use std::{
    collections::HashMap,
    io::stderr,
    path::{Path, PathBuf},
    process::{Command, ExitStatus, Stdio},
};
use tera::Tera;

fn deploy_links(
    globs: GlobBuilder,
    target: &Path,
    duplicates: Duplicates,
    dry_run: bool,
) -> Result<()> {
    let cur_dir = globs.base().to_owned();
    for entry in globs.build().context("Failed to build GlobWalker")? {
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
    pub(super) fn deploy(self, dry_run: bool) -> Result<Env> {
        if !dry_run {
            run_hook(&self.dir, &self.pre_hook)
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
            run_hook(&self.dir, &self.post_hook)
                .transpose()
                .context("Failed to run post-hook")?;
        }

        Ok(env)
    }
}

#[derive(Debug)]
pub struct TemplateContext {
    context: tera::Context,
    env: Dict,
    export: Vec<String>,
    templates: GlobBuilder,
}

impl TemplateContext {
    pub(super) fn new(templates: GlobBuilder, env: Dict, export: Vec<String>) -> Self {
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

        let cur_dir = self.templates.base().to_owned();
        let templates = self
            .templates
            .build()
            .context("Failed to construct GlobWalker")?
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
