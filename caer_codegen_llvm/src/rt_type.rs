use std::ptr::NonNull;

use caer_types::id::InstanceTypeId;
use pinion::{PinionOpaqueStruct, PinionStruct};

#[derive(PinionStruct)]
#[repr(C)]
#[pinion(name = "val")]
pub struct Val {
    disc: i8,
    data: ValUnion,
}

#[derive(PinionStruct)]
#[repr(C)]
pub struct ValUnion {
    // Value for most, vtable ptr for ref
    low: i64,
    // Ptr for ref, unused for rest
    high: i64,
}

#[derive(PinionStruct)]
#[repr(C)]
#[pinion(name = "runtime")]
pub struct Runtime {
    inner: PinionOpaqueStruct,
}

#[derive(PinionStruct)]
#[repr(C)]
#[pinion(name = "arg_pack_tuple")]
pub struct ArgPackTuple {
    name_id: i64,
    val: Val,
}

#[derive(PinionStruct)]
#[repr(C)]
#[pinion(name = "arg_pack")]
pub struct ArgPack {
    unnamed_n: i64,
    unnamed_arr: Option<NonNull<Val>>,
    named_n: i64,
    named_arr: Option<NonNull<ArgPackTuple>>,
    src_val: Val,
}

#[derive(PinionStruct)]
#[repr(C)]
#[pinion(name = "arg_pack")]
pub struct HeapHeader {
    kind: i8,
    gc_marker: i8,
}

#[derive(PinionStruct)]
#[repr(C)]
#[pinion(name = "datum_header")]
pub struct DatumCommon {
    header: HeapHeader,
}

#[derive(PinionStruct)]
#[repr(C)]
#[pinion(name = "rt_string")]
pub struct RtString {
    header: HeapHeader,
    size: i64,
    ptr: NonNull<i8>,
}

#[derive(PinionStruct, Clone, Copy)]
#[repr(C)]
#[pinion(name = "vt_entry")]
pub struct Entry {
    pub id: InstanceTypeId, // i32
    pub size: i64,
    pub var_get: extern "C" fn(datum: *mut DatumCommon, var: &RtString, out: *mut Val),
    pub var_set: extern "C" fn(datum: *mut DatumCommon, var: &RtString, val: *const Val),
    pub proc_lookup: extern "C" fn(proc: &RtString, rt: &mut Runtime) -> ProcPtr,
}

#[derive(PinionStruct, Clone, Copy)]
#[repr(C)]
pub struct ProcPtr(extern "C" fn(arg_pack: *const ArgPack, rt: &mut Runtime, out: *mut Val));

/*
use pinion::ptr::FuncPtr;
use pinion::types::*;
use pinion::{func_type, struct_type};

type Ptr<T> = pinion::ptr::Ptr<T, 0>;
type GcPtr<T> = pinion::ptr::Ptr<T, 1>;

func_type!(
    pub Proc(Ptr<ArgPack>, Ptr<Rt>, GcPtr<Val>)
);

func_type!(
    pub Closure(GcPtr<Val>, Ptr<Rt>, GcPtr<Val>)
);

func_type!(
    pub VarGetFunc(GcPtr<DatumCommon>, GcPtr<String>, GcPtr<Val>)
);

func_type!(
    pub VarSetFunc(GcPtr<DatumCommon>, GcPtr<String>, GcPtr<Val>)
);

func_type!(
    pub ProcLookup(GcPtr<String>, Ptr<Rt>)
);

struct_type!(
    pub VtEntry {
        id: Int32,
        size: Int64,
        var_get_fn: FuncPtr<VarGetFunc>,
        var_set_fn: FuncPtr<VarSetFunc>,
        proc_lookup_fn: FuncPtr<ProcLookup>,
    },
    false,
    "vt_entry",
);

struct_type!(
    pub Ref {
        vptr: Ptr<VtEntry>,
        ptr: GcPtr<OpaqueStruct>,
    },
    false,
    "ref",
);

struct_type!(
    pub Landingpad {
        a: Ptr<OpaqueStruct>,
        b: Int32,
    },
    false,
    "landingpad",
);*/
