// uses system allocator, for now. mainly exists to track allocations for GC sweeping
use alloc::Layout;
use std::alloc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::mem::ManuallyDrop;
use std::ops::Deref;
use std::ptr::{copy_nonoverlapping, NonNull};

#[derive(Debug)]
pub struct Alloc {
    // TODO: replace with UnsafeCell for speeeed?
    allocations: RefCell<HashMap<NonNull<u8>, AllocRecord>>,
}

#[derive(Debug)]
struct AllocRecord {
    layout: Layout,
}

impl Alloc {
    pub fn new() -> Self {
        Self {
            allocations: RefCell::new(HashMap::new()),
        }
    }

    // TODO: tailor more to the types of objects we're allocating, revisit alignment
    pub fn alloc(&mut self, size: usize) -> NonNull<u8> {
        let layout = Layout::from_size_align(size, 8).unwrap();
        self.allocate(layout)
    }

    pub fn alloc_emplace<T>(&self, obj: T) -> NonNull<T> {
        let layout = Layout::for_value(&obj);
        let ptr = self.allocate(layout).cast();
        let obj = ManuallyDrop::new(obj);
        unsafe {
            copy_nonoverlapping(obj.deref(), ptr.as_ptr(), 1);
        }
        ptr
    }

    // alloc api, ish
    fn allocate(&self, layout: Layout) -> NonNull<u8> {
        //let ptr = alloc::Global.allocate_zeroed(layout)?;
        let ptr = unsafe { NonNull::new(alloc::alloc_zeroed(layout)).unwrap() };
        let record = AllocRecord { layout };
        self.allocations.borrow_mut().insert(ptr.cast(), record);
        ptr
    }

    pub unsafe fn dealloc(&mut self, ptr: NonNull<u8>) {
        assert!(self.contains(ptr), "unknown allocation {:?}", ptr);

        let record = &self.allocations.borrow_mut().remove(&ptr).unwrap();
        alloc::dealloc(ptr.as_ptr(), record.layout);
    }

    // Mut here is more about ensuring the closure can't call back into self.
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
