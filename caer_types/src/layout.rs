//! SoT constants for structure layout/offsets

// should probably be in backend!

// TODO: replace this with better addressing in LLVM
// ty indexes into vtable
pub const DATUM_TY_FIELD_OFFSET: u64 = 1;
// only for datum-spec datums, offset to start of vars blob in VST
pub const DATUM_VARS_FIELD_OFFSET: u64 = 1;

pub const VTABLE_SIZE_FIELD_OFFSET: u64 = 0;
pub const VTABLE_VAR_GET_FIELD_OFFSET: u64 = 1;
pub const VTABLE_VAR_SET_FIELD_OFFSET: u64 = 2;
pub const VTABLE_PROC_LOOKUP_FIELD_OFFSET: u64 = 3;

// "DMGC"
pub const GC_STACKMAP_ID: u64 = 0x444d4743;

// discriminants for runtime Val enums
// VAL_DISCRIM_NULL acts as an upper bound for type ids
// TODO: document standard struct layout
pub const VAL_DISCRIM_NULL: u32 = 0x8000_0000;
pub const VAL_DISCRIM_FLOAT: u32 = 0x8000_0001;
pub const VAL_DISCRIM_STRING: u32 = 0x8000_0002;
pub const VAL_DISCRIM_REF: u32 = 0x8000_0003;
