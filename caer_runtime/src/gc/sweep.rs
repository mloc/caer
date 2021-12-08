use std::mem::ManuallyDrop;
use std::ptr::NonNull;

use caer_types::id::{InstanceTypeId, TypeId};
use caer_types::ty::Type;
use caer_types::type_tree::Specialization;

use crate::heap_object::{GcMarker, HeapHeader};
use crate::list::List;
use crate::runtime::Runtime;
use crate::string::RtString;

pub struct Sweep<'rt> {
    runtime: &'rt mut Runtime,
}

impl<'rt> Sweep<'rt> {
    pub fn new(runtime: &'rt mut Runtime) -> Self {
        Self { runtime }
    }

    pub fn sweep(&mut self) {
        let mut to_free = Vec::new();
        //println!("sweeping {} allocations", iter.len());
        self.runtime.alloc.for_each(|ptr, ty_id| {
            println!("sweeping {:?}", ptr);
            let mut object_ptr = ptr.cast();
            let object_ref: &mut HeapHeader = unsafe { object_ptr.as_mut() };
            println!("marker is {:?}", object_ref.gc_marker);
            match object_ref.gc_marker {
                GcMarker::Black => object_ref.gc_marker = GcMarker::White,
                GcMarker::Grey => panic!("should have no grey left during sweep?"),
                GcMarker::White => to_free.push((object_ptr, ty_id)),
            }
        });

        for (ptr, ty_id) in to_free {
            self.drop_ptr(ptr, ty_id);
        }
    }

    // Runs drop glue, then deallocs
    fn drop_ptr(&mut self, ptr: NonNull<HeapHeader>, ty_id: InstanceTypeId) {
        match self.runtime.env.get_type_spec(ty_id) {
            Specialization::String => {
                let string: &mut ManuallyDrop<RtString> = unsafe { ptr.cast().as_mut() };
                unsafe { ManuallyDrop::drop(string) };
            },
            Specialization::Datum => {
                // TODO: call finalizers w/ associated logic, revival checking
            },
            Specialization::List => {
                // List Is Not A Datum
                // /Del isn't called, even if hooked. sorry.
                // TODO(generics): handling for generic list dtors
                let list: &mut ManuallyDrop<List> = unsafe { ptr.cast().as_mut() };
                unsafe { ManuallyDrop::drop(list) };
            },
            _ => todo!("{:?}", ty_id),
        }

        println!("freeing {:?} / {:?}", ptr, ty_id);
        unsafe { self.runtime.alloc.dealloc(ptr.cast()) };
    }
}
