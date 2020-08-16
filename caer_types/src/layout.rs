//! SoT constants for structure layout/offsets

// should probably be in backend!

// TODO: replace this with better addressing in LLVM
// ty indexes into vtable
pub const DATUM_TY_FIELD_OFFSET: u64 = 0;
// only for datum-spec datums, offset to start of vars blob in VST
pub const DATUM_VARS_FIELD_OFFSET: u64 = 1;

pub const VTABLE_SIZE_FIELD_OFFSET: u64 = 0;
pub const VTABLE_VAR_GET_FIELD_OFFSET: u64 = 1;
pub const VTABLE_VAR_SET_FIELD_OFFSET: u64 = 2;
pub const VTABLE_PROC_LOOKUP_FIELD_OFFSET: u64 = 3;
