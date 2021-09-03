#![allow(unused)]
mod commands;
mod config;
mod glob;
mod resolver;
mod topic;

fn main() {
    match run() {
        Ok(_) => (),
        Err(_) => (),
    }
}

fn run() -> Result<(), ()> {
    commands::run();
    todo!()
}
