#[macro_export]
macro_rules! struct_type {
    ($v:vis $i:ident, {$($field_ty:ty) , +}, $packed:expr, $name:expr) => {
        $v struct $i {
            fields: std::marker::PhantomData<(
                $($field_ty,)+
            )>
        }

        impl<'ctx> $crate::traits::PinionType<'ctx> for $i {
            type Out = inkwell::types::StructType<'ctx>;

            fn instantiate(ctx: &'ctx inkwell::context::Context) -> Self::Out {
                ctx.struct_type(
                    &[
                        $(<$field_ty>::instantiate(ctx).into()),+
                    ],
                    $packed)
            }

            fn debug_stringify() -> String {
                concat!(stringify!($i), " {", stringify!($($field_ty),+), "}").into()
            }
        }

        impl<'ctx> $crate::traits::PinionBasicType<'ctx> for $i {
            type BasicOut = Self::Out;
        }
    };
    ($v:vis $i:ident, $field_tys:tt, $packed:expr) => {
        $crate::struct_type!($v $i, $field_tys, $packed, "");
    };
}
