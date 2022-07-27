mod commands;
mod config;
mod fs;
mod glob;
mod print;
mod resolver;
mod serde;
mod topic;

use crate::config::Config;
use anyhow::{anyhow, Context, Result};

pub enum CmdStatus {
    Ok,
    Err,
}

fn main() {
    match run() {
        Ok(CmdStatus::Ok) => (),
        Ok(CmdStatus::Err) => std::process::exit(1),
        Err(e) => {
            log::error!("{:?}", e);
            std::process::exit(1);
        }
    }
}

fn run() -> Result<CmdStatus> {
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("warn")).init();

    check_dirs()?;
    let config = Config::new().context("Failed to load config")?;
    match commands::run(&config) {
        Ok(_) => Ok(CmdStatus::Ok),
        Err(_) => Ok(CmdStatus::Err),
    }
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
