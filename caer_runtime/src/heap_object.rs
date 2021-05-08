/// Header for heap objects
#[derive(Debug, Clone)]
#[repr(C)]
pub struct HeapObject {
    pub kind: HeapKind,
    pub gc_marker: GcMarker,
}

impl HeapObject {
    pub fn datum() -> Self {
        Self {
            kind: HeapKind::Datum,
            gc_marker: GcMarker::White,
        }
    }
}

#[derive(Debug, Clone)]
#[repr(u8)]
pub enum HeapKind {
    Datum = 0,
    String = 1,
}

#[derive(Debug, Clone)]
#[repr(u8)]
pub enum GcMarker {
    White = 0,
    Grey,
    Black,
}
