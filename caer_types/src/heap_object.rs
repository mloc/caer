use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use pinion::PinionStruct;

use crate::layout;

/// Header for heap objects
#[derive(PinionStruct, Debug, Clone)]
#[repr(C)]
pub struct HeapHeader {
    pub gc_marker: u8,
}

impl HeapHeader {
    pub fn new() -> Self {
        Self {
            gc_marker: GcMarker::White as u8,
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

#[derive(FromPrimitive, Debug, Clone, Copy)]
#[repr(u8)]
pub enum GcMarker {
    White = 0,
    Grey,
    Black,
}
