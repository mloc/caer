use caer_types::layout;
use pinion::PinionData;

/// Header for heap objects
#[derive(Debug, Clone, PinionData)]
#[repr(C)]
pub struct HeapHeader {
    pub gc_marker: GcMarker,
}

impl HeapHeader {
    pub fn new() -> Self {
        Self {
            gc_marker: GcMarker::White,
        }
    }
}

impl Default for HeapHeader {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy)]
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
