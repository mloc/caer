use crate::val::Val;
use crate::datum::Datum;
use crate::type_tree::TypeId;
use crate::arg_pack::ArgPack;

use index_vec::Idx;

use std::slice::from_raw_parts;
use std::ops::Index;

/// Convenience wrapper around the global static vtable
#[derive(Debug)]
pub struct Vtable {
    table: &'static [Entry],
}

impl Vtable {
    pub(crate) fn from_static(vtable_ptr: *const Entry, n: usize) -> Self {
        let table = unsafe { from_raw_parts(vtable_ptr, n) };
        Vtable {
            table: table,
        }
    }
}

impl Index<TypeId> for Vtable {
    type Output = Entry;

    fn index(&self, id: TypeId) -> &'static Self::Output {
        &self.table[id.index()]
    }
}

pub const VTABLE_SIZE_FIELD_OFFSET: u64 = 0;
pub const VTABLE_VAR_INDEX_FIELD_OFFSET: u64 = 1;
pub const VTABLE_VAR_GET_FIELD_OFFSET: u64 = 2;
pub const VTABLE_VAR_SET_FIELD_OFFSET: u64 = 3;
pub const VTABLE_PROC_LOOKUP_FIELD_OFFSET: u64 = 4;

/// A single vtable entry for a type
/// **MUST BE KEPT IN SYNC WITH LLVM TYPE**; if not, Bad Things will happen
#[derive(Debug)]
pub struct Entry {
    pub size: i64,
    pub var_index: extern fn(var: u64) -> i32,
    pub var_get: extern fn(datum: *const Datum, var: u64) -> Val,
    pub var_set: extern fn(datum: *const Datum, var: u64, val: Val),
    pub proc_lookup: extern fn(var: u32) -> extern fn(arg_pack: *const ArgPack) -> Val,
}
