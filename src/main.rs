#![allow(unused)]

mod commands;
mod config;
mod env;
mod fs;
mod glob;
mod resolver;
mod topic;

use anyhow::{anyhow, Context, Result};

use crate::config::Config;

fn main() {
    match run() {
        Ok(_) => (),
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    }
}

fn run() -> Result<()> {
    env_logger::init();

    let config = Config::new().context("Failed to load config")?;
    commands::run(&config).context("Failed to run command")
}
