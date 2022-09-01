use serde::Deserialize;

#[derive(Debug, Clone)]
pub struct Shell {
    cmd: String,
    args: Vec<String>,
}

impl Shell {
    pub fn new(cmd: String, args: Vec<String>) -> Self {
        Self { cmd, args }
    }

    pub fn from_vec(_cmd: Vec<String>) -> Self {
        todo!()
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

        Ok(Self {
            cmd: cmd.to_owned(),
            args: args.to_vec(),
        })
    }
}
