/*
use pinion::ptr::FuncPtr;
use pinion::types::*;
use pinion::{func_type, struct_type};

type Ptr<T> = pinion::ptr::Ptr<T, 0>;
type GcPtr<T> = pinion::ptr::Ptr<T, 1>;

struct_type!(
    pub Val {
        disc: Int8, // Discrim
        data: ValUnion, // 16 bytes of data
    },
    false,
    "val",
);

struct_type!(
    pub ValUnion {
        // Value for most, vtable ptr for ref
        low: Int64,
        // Ptr for ref, unused for rest
        high: Int64,
    },
    false,
);

type Rt = OpaqueStruct;

// Argpack bits

struct_type!(
    pub ArgPackTuple {
        name_id: Int64,
        val: Val,
    },
    false,
    "arg_pack_tuple",
);

struct_type!(
    pub ArgPack {
        unnamed_n: Int64,
        unnamed_ptr: Ptr<Val>,
        named_n: Int64,
        named_arr: Ptr<ArgPackTuple>,
        src_val: Val,
    },
    false,
    "arg_pack",
);

func_type!(
    pub Proc(Ptr<ArgPack>, Ptr<Rt>, GcPtr<Val>)
);

func_type!(
    pub Closure(GcPtr<Val>, Ptr<Rt>, GcPtr<Val>)
);

struct_type!(
    pub HeapHeader {
        kind: Int8,
        gc_marker: Int8,
    },
    false,
    "heap_header",
);

struct_type!(
    pub DatumCommon {
        header: HeapHeader,
    },
    false,
    "datum_header",
);

struct_type!(
    pub String {
        header: HeapHeader,
        size: Int64,
        ptr: Ptr<Int8>,
    },
    false,
    "rt_string",
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
