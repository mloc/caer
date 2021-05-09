/// Header for heap objects
#[derive(Debug, Clone)]
#[repr(C)]
pub struct HeapObject {
    pub kind: HeapKind,
    pub gc_marker: GcMarker,
}

impl HeapObject {
    fn with_kind(kind: HeapKind) -> Self {
        Self {
            kind,
            gc_marker: GcMarker::White,
        }
    }

    pub fn datum() -> Self {
        Self::with_kind(HeapKind::Datum)
    }

    pub fn string() -> Self {
        Self::with_kind(HeapKind::String)
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum HeapKind {
    Datum = 0,
    String = 1,
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum GcMarker {
    White = 0,
    Grey,
    Black,
}
