// uses system allocator, for now. mainly exists to track allocations for GC sweeping
use std::alloc::{self, Allocator};
use std::cell::{Ref, RefCell};
use std::collections::HashMap;
use std::ptr::NonNull;

#[derive(Debug)]
pub struct Alloc {
    // TODO: replace with UnsafeCell for speeeed?
    allocations: RefCell<HashMap<NonNull<u8>, AllocRecord>>,
}

#[derive(Debug)]
struct AllocRecord {
    layout: alloc::Layout,
}

impl Alloc {
    pub fn new() -> Self {
        Self {
            allocations: RefCell::new(HashMap::new()),
        }
    }

    // TODO: tailor more to the types of objects we're allocating, revisit alignment
    pub fn alloc(&mut self, size: usize) -> NonNull<u8> {
        let layout = alloc::Layout::from_size_align(size, 8).unwrap();
        (&*self).allocate_zeroed(layout).unwrap().cast()
    }

    pub unsafe fn dealloc(&mut self, ptr: NonNull<u8>) {
        assert!(self.contains(ptr), "unknown allocation {:?}", ptr);

        let record = &self.allocations.borrow_mut().remove(&ptr).unwrap();
        (&*self).deallocate(ptr, record.layout)
    }

    pub fn for_each<F>(&mut self, f: F)
    where
        F: FnMut(NonNull<u8>),
    {
        self.allocations
            .borrow()
            .iter()
            .map(|(p, _)| *p)
            .for_each(f);
    }

    pub fn contains(&self, ptr: NonNull<u8>) -> bool {
        self.allocations.borrow().contains_key(&ptr)
    }
}

unsafe impl Allocator for &Alloc {
    fn allocate(&self, layout: alloc::Layout) -> Result<NonNull<[u8]>, alloc::AllocError> {
        let ptr = alloc::Global.allocate_zeroed(layout)?;
        let record = AllocRecord { layout };
        self.allocations.borrow_mut().insert(ptr.cast(), record);
        Ok(ptr)
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: alloc::Layout) {
        alloc::Global.deallocate(ptr, layout);
    }
}

/*
struct AllocIterator<'a, I> {
    alloc_ref: Ref<'a, HashMap<NonNull<u8>, AllocRecord>>,
    iter: I,
}

impl<'a, I> Iterator for AllocIterator<'a, I>
where
    I: Iterator<Item = NonNull<u8>>,
{
    type Item = NonNull<u8>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

impl<'a, I> ExactSizeIterator for AllocIterator<'a, I>
where
    I: Iterator<Item = NonNull<u8> + ExactSizeIterator,
{
    fn len(&self) -> usize {
        self.iter.len()
    }
}
*/
