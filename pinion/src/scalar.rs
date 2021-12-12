pub enum ScalarType {}

#[macro_export]
macro_rules! scalar_type {
    ($i:ident , $ctx_fn:ident , $out_ty:ident) => {
        pub struct $i {
            _dummy: (),
        }

        impl<'ctx> $crate::traits::Type<'ctx> for $i {
            type Out = inkwell::types::$out_ty<'ctx>;

            fn instantiate(ctx: &'ctx inkwell::context::Context) -> Self::Out {
                ctx.$ctx_fn()
            }

            fn debug_stringify() -> String {
                stringify!($i).into()
            }
        }

        impl<'ctx> $crate::traits::BasicType<'ctx> for $i {
            type BasicOut = inkwell::types::$out_ty<'ctx>;
        }
    };
}

#[macro_export]
macro_rules! int_type {
    ($i:ident , $ctx_fn:ident) => {
        $crate::scalar_type!($i, $ctx_fn, IntType);

        impl<'ctx> $crate::traits::IntType<'ctx> for $i {}
    };
}

#[macro_export]
macro_rules! float_type {
    ($i:ident , $ctx_fn:ident) => {
        $crate::scalar_type!($i, $ctx_fn, FloatType);

        impl<'ctx> $crate::traits::FloatType<'ctx> for $i {}
    };
}
