use std::ptr::NonNull;

use caer_types::id::InstanceTypeId;

#[derive(Debug)]
/// Collects the GC-relevant state of suspended coros.
pub struct State {
    roots: Vec<(NonNull<u8>, InstanceTypeId)>,
    stack_ptrs: Vec<NonNull<u8>>,
}

impl State {
    pub fn new() -> Self {
        Self {
            roots: Vec::new(),
            stack_ptrs: Vec::new(),
        }
    }

    pub fn add_root(&mut self, root: NonNull<u8>, ty_id: InstanceTypeId) {
        self.roots.push((root, ty_id));
    }

    pub fn add_stack_ptr(&mut self, root: NonNull<u8>) {
        self.stack_ptrs.push(root);
    }

    pub fn iter_roots(
        &self,
    ) -> impl Iterator<Item = (NonNull<u8>, InstanceTypeId)> + ExactSizeIterator + '_ {
        self.roots.iter().copied()
    }

    pub fn iter_stack_ptrs(&self) -> impl Iterator<Item = NonNull<u8>> + ExactSizeIterator + '_ {
        self.stack_ptrs.iter().copied()
    }
}
