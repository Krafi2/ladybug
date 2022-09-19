#[derive(Debug, Clone)]
pub struct Command {
    pub cmd: String,
    pub args: Vec<String>,
}

impl Command {
    pub fn new(cmd: String, args: Vec<String>) -> Self {
        Self { cmd, args }
    }
}
