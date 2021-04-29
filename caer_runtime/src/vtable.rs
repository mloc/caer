use std::ops::Index;
use std::ptr::NonNull;
use std::slice::from_raw_parts;

use caer_types::id::{FuncId, StringId, TypeId};

use crate::arg_pack::ProcPack;
use crate::datum::Datum;
use crate::runtime::Runtime;
use crate::val::Val;

/// Convenience wrapper around the global static vtable
#[derive(Debug)]
pub struct Vtable {
    table: &'static [Entry],
    funcs: &'static [FuncPtr],
}

impl Vtable {
    pub(crate) fn from_static(
        vtable_ptr: *const Entry, vtable_n: usize, funcs_ptr: *const FuncPtr, funcs_n: usize,
    ) -> Self {
        let table = unsafe { from_raw_parts(vtable_ptr, vtable_n) };
        let funcs = unsafe { from_raw_parts(funcs_ptr, funcs_n) };
        Vtable { table, funcs }
    }

    pub fn lookup_func(&self, func: FuncId) -> Option<FuncPtr> {
        self.funcs.get(func.index()).copied()
    }
}

impl Index<TypeId> for Vtable {
    type Output = Entry;

    fn index(&self, id: TypeId) -> &'static Self::Output {
        &self.table[id.index()]
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct FuncPtr {
    ptr: *const std::ffi::c_void,
}
pub type ProcPtr = extern "C" fn(arg_pack: *const ProcPack, rt: NonNull<Runtime>, out: *mut Val);
pub type ClosurePtr = extern "C" fn(arg_pack: *const Val, rt: NonNull<Runtime>, out: *mut Val);

impl FuncPtr {
    pub unsafe fn as_proc(self) -> ProcPtr {
        std::mem::transmute(self.ptr)
    }

    pub unsafe fn as_closure(self) -> ClosurePtr {
        std::mem::transmute(self.ptr)
    }
}

/// A single vtable entry for a type
/// **MUST BE KEPT IN SYNC WITH LLVM TYPE**; if not, Bad Things will happen
#[derive(Debug)]
pub struct Entry {
    pub size: i64,
    pub var_get: extern "C" fn(datum: *mut Datum, var: u64, out: *mut Val),
    pub var_set: extern "C" fn(datum: *mut Datum, var: u64, val: *const Val),
    pub proc_lookup: extern "C" fn(var: StringId, rt: NonNull<Runtime>) -> ProcPtr,
}
