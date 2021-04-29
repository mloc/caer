// uses system allocator, for now. mainly exists to track allocations for GC sweeping
use std::alloc;
use std::collections::HashMap;
use std::ptr::NonNull;

#[derive(Debug)]
pub struct Alloc {
    allocations: HashMap<NonNull<u8>, AllocRecord>,
}

#[derive(Debug)]
struct AllocRecord {
    layout: alloc::Layout,
}

impl Alloc {
    pub fn new() -> Self {
        Self {
            allocations: HashMap::new(),
        }
    }

    // TODO: tailor more to the types of objects we're allocating, revisit alignment
    pub fn alloc(&mut self, size: usize) -> NonNull<u8> {
        let layout = alloc::Layout::from_size_align(size, 8).unwrap();
        let ptr = unsafe { NonNull::new(alloc::alloc_zeroed(layout)).unwrap() };

        let record = AllocRecord { layout };
        self.allocations.insert(ptr, record);
        ptr
    }

    pub unsafe fn dealloc(&mut self, ptr: NonNull<u8>) {
        assert!(
            self.allocations.contains_key(&ptr),
            "unknown allocation {:?}",
            ptr
        );

        let record = &self.allocations.remove(&ptr).unwrap();
        alloc::dealloc(ptr.as_ptr(), record.layout);
    }

    pub fn iter_allocations(&self) -> impl Iterator<Item = NonNull<u8>> + ExactSizeIterator + '_ {
        self.allocations.iter().map(|(p, _)| *p)
    }

    pub fn contains(&self, ptr: NonNull<u8>) -> bool {
        self.allocations.contains_key(&ptr)
    }
}
