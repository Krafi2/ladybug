use common::command::Command;
use serde::Deserialize;

#[derive(Debug, Clone)]
pub struct Shell(Command);

impl Shell {
    pub fn new(cmd: String, args: Vec<String>) -> Self {
        Self::from(Command::new(cmd, args))
    }
}

impl From<common::command::Command> for Shell {
    fn from(cmd: common::command::Command) -> Self {
        Self(cmd)
    }
}

impl<'de> Deserialize<'de> for Shell {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let vec = Vec::<String>::deserialize(deserializer)?;
        let (cmd, args) = vec
            .split_first()
            .ok_or(serde::de::Error::custom("Expected at least one element"))?;

        Ok(Self::new(cmd.to_owned(), args.to_vec()))
    }
}
