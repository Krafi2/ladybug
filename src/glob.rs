use anyhow::{Context, Result};
use globwalk::{GlobWalker, GlobWalkerBuilder};
use std::{
    borrow::Cow,
    path::{Path, PathBuf},
};

#[derive(Debug, Clone)]
pub struct GlobBuilder {
    base: PathBuf,
    patterns: Vec<String>,
    max_depth: Option<usize>,
}

impl GlobBuilder {
    pub fn new(base: PathBuf) -> Self {
        Self {
            base,
            patterns: Vec::new(),
            max_depth: None,
        }
    }

    pub fn add<'a, S>(mut self, glob: S, whitelist: bool) -> Self
    where
        S: Into<Cow<'a, str>>,
    {
        let glob = glob.into();
        let pattern = match whitelist {
            true => glob.into_owned(),
            false => {
                let mut pattern = String::from("!");
                pattern.push_str(&glob);
                pattern
            }
        };
        self.patterns.push(pattern);
        self
    }

    pub fn extend<'a, I, T>(mut self, globs: I, whitelist: bool) -> Self
    where
        I: IntoIterator<Item = T>,
        T: Into<Cow<'a, str>>,
    {
        for s in globs {
            self = self.add(s, whitelist);
        }
        self
    }

    pub fn base(&self) -> &Path {
        &self.base
    }

    pub fn max_depth(mut self, depth: usize) -> Self {
        self.max_depth.replace(depth);
        self
    }

    pub fn build(self) -> Result<GlobWalker> {
        let mut builder = GlobWalkerBuilder::from_patterns(&self.base, &self.patterns);

        if let Some(depth) = self.max_depth {
            builder = builder.max_depth(depth);
        }

        builder
            .min_depth(1)
            .build()
            .context("Failed to build GlobWalker")
    }
}
