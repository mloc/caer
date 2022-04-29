use std::slice::from_raw_parts_mut;

use caer_types::id::InstanceTypeId;
use pinion::PinionData;

use crate::heap_object::HeapHeader;
use crate::runtime::Runtime;
use crate::val::Val;

// TODO: rename. object? called gdatum in some places.

/// Represents the statically-sized part of a datum struct.
/// Datums in the runtime are DSTs due to their var array
#[derive(Debug, Clone, PinionData)]
#[repr(C)]
pub struct Datum {
    pub heap_header: HeapHeader,
}

impl Datum {
    pub fn new(ty: InstanceTypeId) -> Self {
        Self {
            heap_header: HeapHeader::new(),
        }
    }

    // TODO: hacky, not great... should maybe use vtable functions?
    // should probably also not be doing runtime diving here
    pub unsafe fn get_vars<'a>(
        datum: &'a mut Datum, ty: InstanceTypeId, runtime: &Runtime,
    ) -> &'a mut [Val] {
        let n_vars = runtime
            .env
            .instances
            .lookup_instance(ty)
            .unwrap()
            .pty
            .vars
            .len();
        unsafe {
            let vars_start = (datum as *mut Datum as *mut u8).add(8) as *mut Val;
            from_raw_parts_mut(vars_start, n_vars)
        }
    }
}
