use anyhow::{anyhow, Context, Error, Result};
use globwalk::{GlobWalker, GlobWalkerBuilder};
use std::{
    borrow::Cow,
    path::{Path, PathBuf},
};

#[derive(Debug)]
pub struct GlobBuilder {
    base: PathBuf,
    patterns: Vec<String>,
}

impl GlobBuilder {
    pub fn new(base: PathBuf) -> Self {
        Self {
            base,
            patterns: Vec::new(),
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

    pub fn build(mut self) -> Result<GlobWalker> {
        GlobWalkerBuilder::from_patterns(&self.base, &self.patterns)
            .build()
            .context("Failed to build GlobWalker")
    }
}
