#![allow(unused)]

mod commands;
mod config;
mod env;
mod fs;
mod glob;
mod resolver;
mod topic;

use std::path::Path;

use anyhow::{anyhow, Context, Result};

use crate::config::Config;

fn main() {
    match run() {
        Ok(_) => (),
        Err(e) => {
            eprintln!("{:?}", e);
            std::process::exit(1);
        }
    }
}

fn run() -> Result<()> {
    // println!("Hee");
    // let mut glob = crate::glob::GlobBuilder::new(Path::new("/home/jakub/ladybug"));
    // glob.add("*/ladybug.toml", true)?;
    // glob.add("**", false)?;
    // let glob = glob.build()?;
    // dbg!(&glob);
    // for file in glob.walk() {
    //     println!("file: {:?}", file?);
    // }
    // return Ok(());

    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("info")).init();

    check_dirs()?;
    let config = Config::new().context("Failed to load config")?;
    commands::run(&config).context("Failed to run command")
}

fn check_dirs() -> Result<()> {
    let dir = config::paths::dotfile_dir();
    match dir.metadata() {
        Ok(metadata) => match metadata.is_dir() {
            true => Ok(()),
            false => Err(anyhow!("'{}' isn't a directory", dir.display())),
        },
        Err(err) => Err(anyhow!(err))
            .with_context(|| format!("The dotfile directory '{}' isn't accessible", dir.display())),
    }
}
