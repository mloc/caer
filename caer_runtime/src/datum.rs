use crate::type_tree::TypeId;

// TODO: replace this with better addressing in LLVM
/// Used for LLVM codegen
pub const DATUM_VARS_FIELD_OFFSET: u64 = 1;

/// Represents the statically-sized part of a datum struct.
/// Datums in the runtime are DSTs due to their var array
pub struct Datum {
    pub ty: TypeId,
}
