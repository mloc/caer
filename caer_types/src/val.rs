use pinion::PinionData;

#[derive(PinionData)]
#[repr(C)]
#[pinion(name = "val")]
pub struct ValFlat {
    disc: i8,
    data: ValUnion,
}

#[derive(PinionData)]
#[repr(C)]
pub struct ValUnion {
    // Value for most, vtable ptr for ref
    low: i64,
    // Ptr for ref, unused for rest
    high: i64,
}
