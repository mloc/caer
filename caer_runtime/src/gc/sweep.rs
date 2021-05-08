use crate::alloc::Alloc;
use crate::datum::Datum;
use crate::heap_object::{GcMarker, HeapKind};

pub struct Sweep<'rt> {
    alloc: &'rt mut Alloc,
}

impl<'rt> Sweep<'rt> {
    pub fn new(alloc: &'rt mut Alloc) -> Self {
        Self { alloc }
    }

    pub fn sweep(&mut self) {
        let mut to_free = Vec::new();
        let iter = self.alloc.iter_allocations();
        println!("sweeping {} allocations", iter.len());
        for ptr in iter {
            println!("sweeping {:?}", ptr);
            let mut datum_ptr = ptr.cast();
            let datum_ref: &mut Datum = unsafe { datum_ptr.as_mut() };
            assert!(matches!(datum_ref.heap_header.kind, HeapKind::Datum));
            println!("marker is {:?}", datum_ref.heap_header.gc_marker);
            match datum_ref.heap_header.gc_marker {
                GcMarker::Black => datum_ref.heap_header.gc_marker = GcMarker::White,
                GcMarker::Grey => panic!("should have no grey left during sweep?"),
                GcMarker::White => to_free.push(ptr),
            }
        }

        unsafe {
            for ptr in to_free {
                println!("freeing {:?}", ptr);
                self.alloc.dealloc(ptr);
            }
        }
    }
}
