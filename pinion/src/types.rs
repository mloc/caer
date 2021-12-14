use crate::traits::{PinionBasicType, PinionIndexableType, PinionType};
use crate::{float_type, int_type, scalar_type};

scalar_type!(pub Void, void_type, VoidType);

int_type!(pub Bool, bool_type);
int_type!(pub Int8, i8_type);
int_type!(pub Int16, i16_type);
int_type!(pub Int32, i32_type);
int_type!(pub Int64, i64_type);

float_type!(pub Float16, f16_type);
float_type!(pub Float32, f32_type);
float_type!(pub Float64, f64_type);

// TODO: move maybe idk
pub struct OpaqueStruct {
    _dummy: (),
}

impl<'ctx> PinionType<'ctx> for OpaqueStruct {
    type Out = inkwell::types::StructType<'ctx>;

    fn instantiate(ctx: &'ctx inkwell::context::Context) -> Self::Out {
        ctx.opaque_struct_type("opaque")
    }

    fn debug_stringify() -> String {
        "OpaqueStruct".into()
    }
}

impl<'ctx> PinionBasicType<'ctx> for OpaqueStruct {
    type BasicOut = Self::Out;

    fn create_empty() -> Box<dyn PinionIndexableType<'ctx>> {
        Box::new(Self { _dummy: () })
    }
}

impl<'ctx> PinionIndexableType<'ctx> for OpaqueStruct {}
