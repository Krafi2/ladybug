#![allow(unused)]

mod commands;
mod config;
mod context;
mod env;
mod fs;
mod glob;
mod resolver;
mod topic;

use anyhow::Result;

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
    let ctx = todo!();
    commands::run(ctx);
    todo!()
}
