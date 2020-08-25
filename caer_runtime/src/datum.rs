use caer_types::id::TypeId;

// TODO: rename. object? called gdatum in some places.

/// Represents the statically-sized part of a datum struct.
/// Datums in the runtime are DSTs due to their var array
#[derive(Debug, Clone)]
pub struct Datum {
    pub ty: TypeId,
    pub gc_marker: GcMarker,
}

impl Datum {
    pub fn new(ty: TypeId) -> Self {
        Self {
            ty,
            gc_marker: GcMarker::White,
        }
    }
}

#[derive(Debug, Clone)]
#[repr(u32)]
pub enum GcMarker {
    White = 0,
    Grey,
    Black,
}
