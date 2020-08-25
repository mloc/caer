use caer_types::id::TypeId;
use std::slice::from_raw_parts_mut;
use crate::val::Val;
use crate::runtime::Runtime;

// TODO: rename. object? called gdatum in some places.

/// Represents the statically-sized part of a datum struct.
/// Datums in the runtime are DSTs due to their var array
#[derive(Debug, Clone)]
pub struct Datum {
    pub ty: TypeId,
    pub gc_marker: GcMarker,
}

impl Datum {
    pub fn new(ty: TypeId) -> Self {
        Self {
            ty,
            gc_marker: GcMarker::White,
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

#[derive(Debug, Clone)]
#[repr(u32)]
pub enum GcMarker {
    White = 0,
    Grey,
    Black,
}
