use std::ptr::NonNull;

#[derive(Debug)]
pub struct State {
    root_ptrs: Vec<NonNull<u32>>,
}

impl State {
    pub fn new() -> Self {
        Self {
            root_ptrs: Vec::new(),
        }
    }

    pub fn add_root(&mut self, root: NonNull<u32>) {
        self.root_ptrs.push(root);
    }

    pub fn iter_roots(&self) -> impl Iterator<Item = NonNull<u32>> + ExactSizeIterator + '_ {
        self.root_ptrs.iter().copied()
    }
}
