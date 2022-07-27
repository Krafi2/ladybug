use std::path::PathBuf;

pub struct RelPath(PathBuf);

impl From<PathBuf> for RelPath {
    fn from(path: PathBuf) -> Self {
        Self(path)
    }
}

impl RelPath {
    pub fn new(path: PathBuf) -> Self {
        Self::from(path)
    }

    pub fn canonize(
        self,
        context: &crate::config::Context,
        topic: &crate::topic::Topic,
    ) -> PathBuf {
        todo!()
    }
}
