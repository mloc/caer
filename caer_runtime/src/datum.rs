use caer_types::id::TypeId;

/// Represents the statically-sized part of a datum struct.
/// Datums in the runtime are DSTs due to their var array
#[derive(Debug, Clone)]
pub struct Datum {
    pub ty: TypeId,
}
