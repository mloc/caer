//! SoT constants for structure layout/offsets

// should probably be in backend!

// TODO: replace this with better addressing in LLVM
// ty indexes into vtable
//pub const DATUM_TY_FIELD_OFFSET: u64 = 1;
// only for datum-spec datums, offset to start of vars blob in VST
pub const DATUM_VARS_FIELD_OFFSET: u64 = 1;

pub const VTABLE_SIZE_FIELD_OFFSET: u64 = 0;
pub const VTABLE_VAR_GET_FIELD_OFFSET: u64 = 1;
pub const VTABLE_VAR_SET_FIELD_OFFSET: u64 = 2;
pub const VTABLE_PROC_LOOKUP_FIELD_OFFSET: u64 = 3;

// "DMGC"
pub const GC_STACKMAP_ID: u64 = 0x444d4743;

pub const HEAP_KIND_DATUM: u8 = 0x00;
pub const HEAP_KIND_LIST: u8 = 0x01;
pub const HEAP_KIND_STRING: u8 = 0x02;

// discriminants for runtime Val enums
// Shares space with HeapKinds- MSB is set for val discs.
// TODO: document standard struct layout
pub const VAL_DISCRIM_NULL: u8 = 0x80;
pub const VAL_DISCRIM_FLOAT: u8 = 0x81;
pub const VAL_DISCRIM_STRING: u8 = 0x82;
pub const VAL_DISCRIM_REF: u8 = 0x83;
