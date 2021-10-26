use std::{
    hash::Hash,
    marker::PhantomData,
    ops::{Index, IndexMut},
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
    next_id: usize,
}

impl Registry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn new_id(&mut self) -> TopicId {
        let id = self.next_id;
        self.next_id += 1;
        TopicId::new(id)
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

#[derive(Clone, Debug)]
pub struct Storage<T> {
    vec: Vec<Option<T>>,
}

impl<T> Storage<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_handle<F>(&mut self, id: TopicId, func: F) -> Handle<T>
    where
        F: FnOnce() -> T,
    {
        let idx = id.0;
        let len = self.vec.len();
        if idx >= len {
            let to_fill = idx - len + 1;
            self.vec
                .extend(std::iter::repeat_with(|| None).take(to_fill))
        }
        match &mut self.vec[idx] {
            Some(_) => (),
            t @ None => *t = Some(func()),
        }
        Handle::new(id)
    }

    pub fn try_get_handle(&self, id: TopicId) -> Option<Handle<T>> {
        self.vec
            .get(id.0)
            .map(<&Option<T> as Into<Option<&T>>>::into)
            .flatten()
            .map(|_| Handle::new(id))
    }

    pub fn get_raw(&self, handle: Handle<T>) -> &Option<T> {
        &self.vec[handle.0 .0]
    }

    pub fn get_raw_mut(&mut self, handle: Handle<T>) -> &mut Option<T> {
        &mut self.vec[handle.0 .0]
    }

    pub fn replace<F>(&mut self, handle: Handle<T>, f: F)
    where
        F: FnOnce(T) -> T,
    {
        let raw = self.get_raw_mut(handle);
        let old = std::mem::replace(raw, None).expect("Invalid handle");
        *raw = Some(f(old));
    }
}

impl<T> Index<Handle<T>> for Storage<T> {
    type Output = T;

    fn index(&self, index: Handle<T>) -> &Self::Output {
        self.get_raw(index).as_ref().expect("Handle not available")
    }
}

impl<T> IndexMut<Handle<T>> for Storage<T> {
    fn index_mut(&mut self, index: Handle<T>) -> &mut Self::Output {
        self.get_raw_mut(index)
            .as_mut()
            .expect("Handle not available")
    }
}

impl<T> Default for Storage<T> {
    fn default() -> Self {
        Self {
            vec: Vec::default(),
        }
    }
}
