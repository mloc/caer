use std::fmt::Debug;
use std::ops::Index;
use std::slice::from_raw_parts;

use caer_types::id::{FuncId, InstanceTypeId};
use pinion::PinionData;

use crate::arg_pack::ProcPack;
use crate::datum::Datum;
use crate::runtime::Runtime;
use crate::string::RtString;
use crate::val::Val;

/// Convenience wrapper around the global static vtable
#[derive(Debug)]
pub struct Vtable {
    // Indexed by InstanceTypeId
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

    pub fn lookup(&self, id: InstanceTypeId) -> &'static Entry {
        &self.table[id.index()]
    }
}

impl Index<InstanceTypeId> for Vtable {
    type Output = Entry;

    fn index(&self, id: InstanceTypeId) -> &'static Self::Output {
        &self.table[id.index()]
    }
}

#[derive(Debug, Clone, Copy, PinionData)]
#[repr(transparent)]
pub struct FuncPtr(pub *const u8);

#[derive(Clone, Copy, PinionData)]
#[repr(transparent)]
pub struct ProcPtr(pub extern "C" fn(arg_pack: *const ProcPack, rt: &mut Runtime, out: *mut Val));

pub type ClosurePtr = extern "C" fn(arg_pack: *const Val, rt: &mut Runtime, out: *mut Val);

impl FuncPtr {
    pub unsafe fn as_proc(self) -> ProcPtr {
        std::mem::transmute(self.0)
    }

    pub unsafe fn as_closure(self) -> ClosurePtr {
        std::mem::transmute(self.0)
    }
}

/// A single vtable entry for a type
/// **MUST BE KEPT IN SYNC WITH LLVM TYPE**; if not, Bad Things will happen
#[derive(Clone, Copy, PinionData)]
#[repr(C)]
pub struct Entry {
    pub id: InstanceTypeId, // i32
    pub size: i64,
    pub var_get: extern "C" fn(datum: *mut Datum, var: &RtString, out: *mut Val),
    pub var_set: extern "C" fn(datum: *mut Datum, var: &RtString, val: *const Val),
    pub proc_lookup: extern "C" fn(proc: &RtString, rt: &mut Runtime) -> ProcPtr,
}

impl Debug for Entry {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO: proper formatter
        Ok(())
    }
}
