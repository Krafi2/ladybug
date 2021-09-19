#[derive(Debug, Clone)]
pub struct Env {}

impl Env {
    pub fn merge(&mut self, other: &Self) {
        todo!()
    }
    pub fn empty() -> Self {
        Self::default()
    }
}

impl Default for Env {
    fn default() -> Self {
        todo!()
    }
}
