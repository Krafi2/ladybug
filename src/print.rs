use anyhow::{anyhow, Result};
use std::io::Write;

#[derive(Debug, Clone)]
pub struct Formatter<'a> {
    prefixes: &'a [&'static str],
}

impl<'a> Formatter<'a> {
    pub fn new(prefixes: &'a [&'static str]) -> Self {
        Self { prefixes }
    }

    pub fn descend<'b>(&'b self) -> Result<Formatter<'b>> {
        // We can descend alright
        if self.prefixes.len() > 0 {
            Ok(Self::new(&self.prefixes[1..]))
        // We're out of levels
        } else {
            Err(anyhow!("Formatter cannot descend any further"))
        }
    }
}

impl<'a> std::fmt::Write for Formatter<'a> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        let handle = std::io::stdout();
        let mut handle = handle.lock();

        let prefix = self.prefixes[0];
        // TODO: Fnd a way to improve the prefix writing
        let mut prepend = "\n".to_owned();
        prepend.push_str(prefix);
        let s = s.replace('\n', &prepend);

        handle.write_all(s.as_bytes()).map_err(|_| std::fmt::Error)
    }
}
