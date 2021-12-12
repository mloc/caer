#[macro_export]
macro_rules! struct_type {
    (@int $i:ident, {$($field_ty:ty) , +}, $packed:expr) => {
        struct $i {
            fields: std::marker::PhantomData<(
                $($field_ty,)+
            )>
        }

        impl<'ctx> $crate::traits::Type<'ctx> for $i {
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

        impl<'ctx> $crate::traits::BasicType<'ctx> for $i {
            type BasicOut = Self::Out;
        }
    };
    ($i:ident, $field_tys:tt ) => {
        struct_type!(@int $i, $field_tys, false);
    };
    ($i:ident, packed $field_tys:tt) => {
        struct_type!(@int $i, $field_tys, true);
    };
}
