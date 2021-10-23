use super::TopicDesc;
use std::{
    collections::HashMap,
    hash::Hash,
    marker::PhantomData,
    ops::{Index, IndexMut},
    path::PathBuf,
};

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct TopicId(usize);

impl TopicId {
    fn new(idx: usize) -> Self {
        Self(idx)
    }
}

#[derive(Clone, Debug, Default)]
pub struct Registry {
    map: HashMap<TopicDesc, TopicId>,
    storage: Vec<TopicDesc>,
}

impl Registry {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn register(&mut self, desc: TopicDesc) -> TopicId {
        match self.map.get(&desc) {
            Some(id) => *id,
            None => {
                let idx = self.storage.len();
                let id = TopicId(idx);
                self.storage.push(desc.clone());
                self.map.insert(desc, id);
                id
            }
        }
    }
    pub fn get_desc(&self, id: TopicId) -> &TopicDesc {
        &self.storage[id.0]
    }
}

impl Index<TopicId> for Registry {
    type Output = TopicDesc;

    fn index(&self, index: TopicId) -> &Self::Output {
        self.get_desc(index)
    }
}

#[derive(Debug)]
pub struct Handle<T>(TopicId, PhantomData<*const T>);

impl<T> Handle<T> {
    fn new(idx: TopicId) -> Self {
        Self(idx, PhantomData)
    }
}

impl<T> Clone for Handle<T> {
    fn clone(&self) -> Self {
        Self::new(self.0)
    }
}

impl<T> Copy for Handle<T> {}

impl<T> PartialEq for Handle<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Hash for Handle<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<T> Eq for Handle<T> {}
impl<T> Into<TopicId> for Handle<T> {
    fn into(self) -> TopicId {
        self.0
    }
}

#[derive(Clone, Debug, Default)]
pub struct Storage<T> {
    vec: Vec<T>,
}

impl<T> Storage<T> {
    pub fn get_handle<F>(&mut self, id: TopicId, func: F) -> Handle<T>
    where
        F: FnMut(TopicId) -> T,
    {
        let idx = id.0;
        let len = self.vec.len();
        let to_fill = (idx + 1).saturating_sub(len);
        self.vec
            .extend((len..=idx).take(to_fill).map(TopicId::new).map(func));
        Handle::new(id)
    }
}

impl<T> Index<Handle<T>> for Storage<T> {
    type Output = T;

    fn index(&self, index: Handle<T>) -> &Self::Output {
        &self.vec[index.0 .0]
    }
}

impl<T> IndexMut<Handle<T>> for Storage<T> {
    fn index_mut(&mut self, index: Handle<T>) -> &mut Self::Output {
        &mut self.vec[index.0 .0]
    }
}
