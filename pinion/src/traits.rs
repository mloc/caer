use crate::interface::Context;
use crate::types::{layout, Primitive};

pub trait PinionBasicType {
    fn create_in_context<C: Context>(ctx: &mut C) -> C::BasicType;

    fn get_layout() -> layout::BasicType;
}

macro_rules! prim_types {
    ($($($prim:ty)|+ => $var:ident),+ $(,)?) => {
        $($(
            impl PinionBasicType for $prim {
                fn create_in_context<C: Context>(ctx: &mut C) -> C::BasicType {
                    ctx.make_primitive_type(Primitive::$var)
                }

                fn get_layout() -> layout::BasicType {
                    layout::BasicType::Primitive(Primitive::$var)
                }
            }
        )+)+
    };
}

prim_types! {
    bool => Bool,
    i8 | u8 => Int8,
    i16 | u16 => Int16,
    i32 | u32 => Int32,
    i64 | u64 => Int64,
    f32 => Float32,
    f64 => Float64,
}
