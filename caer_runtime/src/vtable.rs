use crate::arg_pack::ArgPack;
use crate::datum::Datum;
use crate::runtime::Runtime;
use crate::string_table::StringId;
use crate::type_tree::TypeId;
use crate::val::Val;

use std::ops::Index;
use std::ptr::NonNull;
use std::slice::from_raw_parts;

/// Convenience wrapper around the global static vtable
#[derive(Debug)]
pub struct Vtable {
    table: &'static [Entry],
}

impl Vtable {
    pub(crate) fn from_static(vtable_ptr: *const Entry, n: usize) -> Self {
        let table = unsafe { from_raw_parts(vtable_ptr, n) };
        Vtable { table }
    }
}

impl Index<TypeId> for Vtable {
    type Output = Entry;

    fn index(&self, id: TypeId) -> &'static Self::Output {
        &self.table[id.index()]
    }
}

pub const VTABLE_SIZE_FIELD_OFFSET: u64 = 0;
pub const VTABLE_VAR_GET_FIELD_OFFSET: u64 = 1;
pub const VTABLE_VAR_SET_FIELD_OFFSET: u64 = 2;
pub const VTABLE_PROC_LOOKUP_FIELD_OFFSET: u64 = 3;

pub type ProcPtr = extern "C" fn(arg_pack: *const ArgPack, rt: NonNull<Runtime>) -> Val;

/// A single vtable entry for a type
/// **MUST BE KEPT IN SYNC WITH LLVM TYPE**; if not, Bad Things will happen
#[derive(Debug)]
pub struct Entry {
    pub size: i64,
    pub var_get: extern "C" fn(datum: *mut Datum, var: u64) -> Val,
    pub var_set: extern "C" fn(datum: *mut Datum, var: u64, val: Val),
    pub proc_lookup: extern "C" fn(var: StringId, rt: NonNull<Runtime>) -> ProcPtr,
}
