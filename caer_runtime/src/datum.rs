use std::slice::from_raw_parts_mut;

use caer_types::id::TypeId;

use crate::heap_object::HeapHeader;
use crate::runtime::Runtime;
use crate::val::Val;

// TODO: rename. object? called gdatum in some places.

/// Represents the statically-sized part of a datum struct.
/// Datums in the runtime are DSTs due to their var array
#[derive(Debug, Clone)]
#[repr(C)]
pub struct Datum {
    pub heap_header: HeapHeader,
    pub ty: TypeId,
}

impl Datum {
    pub fn new(ty: TypeId) -> Self {
        Self {
            heap_header: HeapHeader::datum(),
            ty,
        }
    }

    // TODO: hacky, not great... should maybe use vtable functions?
    pub fn get_vars(&mut self, runtime: &Runtime) -> &mut [Val] {
        let n_vars = runtime.env.type_tree.types[self.ty].vars.len();
        unsafe {
            let vars_start = (self as *mut Datum as *mut u8).add(8) as *mut Val;
            from_raw_parts_mut(vars_start, n_vars)
        }
    }
}
