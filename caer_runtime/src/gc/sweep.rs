use std::mem::ManuallyDrop;
use std::ptr::NonNull;

use crate::alloc::Alloc;
use crate::datum::Datum;
use crate::heap_object::{GcMarker, HeapHeader, HeapKind};
use crate::list::List;
use crate::string::RtString;

pub struct Sweep<'rt> {
    alloc: &'rt mut Alloc,
}

impl<'rt> Sweep<'rt> {
    pub fn new(alloc: &'rt mut Alloc) -> Self {
        Self { alloc }
    }

    pub fn sweep(&mut self) {
        let mut to_free = Vec::new();
        //println!("sweeping {} allocations", iter.len());
        self.alloc.for_each(|ptr| {
            println!("sweeping {:?}", ptr);
            let mut object_ptr = ptr.cast();
            let object_ref: &mut HeapHeader = unsafe { object_ptr.as_mut() };
            println!("marker is {:?}", object_ref.gc_marker);
            match object_ref.gc_marker {
                GcMarker::Black => object_ref.gc_marker = GcMarker::White,
                GcMarker::Grey => panic!("should have no grey left during sweep?"),
                GcMarker::White => to_free.push(object_ptr),
            }
        });

        for ptr in to_free {
            self.drop_ptr(ptr);
        }
    }

    // Runs drop glue, then deallocs
    fn drop_ptr(&mut self, ptr: NonNull<HeapHeader>) {
        // Lots of special casing here :/
        let header = unsafe { ptr.as_ref() };
        match header.kind {
            HeapKind::String => {
                let string: &mut ManuallyDrop<RtString> = unsafe { ptr.cast().as_mut() };
                unsafe { ManuallyDrop::drop(string) };
            },
            HeapKind::List => {
                // List Is Not A Datum
                // /Del isn't called, even if hooked. sorry.
                // TODO(generics): handling for generic list dtors
                let list: &mut ManuallyDrop<List> = unsafe { ptr.cast().as_mut() };
                unsafe { ManuallyDrop::drop(list) };
            },
            HeapKind::Datum => {
                // TODO: call finalizers w/ associated logic, revival checking
            },
        }

        println!("freeing {:?} / {:?}", ptr, header.kind);
        unsafe { self.alloc.dealloc(ptr.cast()) };
    }
}
