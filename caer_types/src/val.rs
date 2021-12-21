use pinion::{pinion_export_funcs, PinionOpaqueStruct, PinionStruct};

#[derive(PinionStruct)]
#[repr(C)]
#[pinion(name = "val")]
pub struct ValFlat {
    disc: i8,
    data: ValUnion,
}

#[derive(PinionStruct)]
#[repr(C)]
pub struct ValUnion {
    // Value for most, vtable ptr for ref
    low: i64,
    // Ptr for ref, unused for rest
    high: i64,
}

/*#[pinion_export_funcs(ValFuncs, export_val_funcs)]
pub trait ValExport {
    fn rt_val_binary_op(
        rt: &mut PinionOpaqueStruct, op: op::BinaryOp, lhs: &ValFlat, rhs: &ValFlat,
        out: &mut ValFlat,
    );
}*/
