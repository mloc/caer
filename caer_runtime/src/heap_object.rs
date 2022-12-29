use caer_types::layout;
use pinion::PinionData;

/// Header for heap objects
#[derive(Debug, Clone, PinionData)]
#[repr(C)]
pub struct HeapHeader {
    pub heap_kind: HeapKind,
    pub gc_marker: GcMarker,
}

impl HeapHeader {
    pub fn new(kind: HeapKind) -> Self {
        Self {
            heap_kind: kind,
            gc_marker: GcMarker::White,
        }
    }

    pub fn datum() -> Self {
        Self::new(HeapKind::Datum)
    }

    pub fn list() -> Self {
        Self::new(HeapKind::List)
    }

    pub fn string() -> Self {
        Self::new(HeapKind::String)
    }
}

#[derive(Debug, Clone, Copy, PinionData)]
#[repr(u8)]
pub enum HeapKind {
    Datum = layout::HEAP_KIND_DATUM,
    List = layout::HEAP_KIND_LIST,
    String = layout::HEAP_KIND_STRING,
}

#[derive(Debug, Clone, Copy, PinionData)]
#[repr(u8)]
pub enum GcMarker {
    White = 0,
    Grey,
    Black,
}
