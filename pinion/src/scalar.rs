pub enum ScalarType {}

#[macro_export]
macro_rules! scalar_type {
    ($v:vis $i:ident , $ctx_fn:ident , $out_ty:ident) => {
        $v struct $i {
            _dummy: (),
        }

        impl<'ctx> $crate::traits::PinionType<'ctx> for $i {
            type Out = inkwell::types::$out_ty<'ctx>;

            fn instantiate(ctx: &'ctx inkwell::context::Context) -> Self::Out {
                ctx.$ctx_fn()
            }

            fn debug_stringify() -> String {
                stringify!($i).into()
            }
        }
    };
}

#[macro_export]
macro_rules! basic_scalar_type {
    ($v:vis $i:ident , $ctx_fn:ident , $out_ty:ident) => {
        $crate::scalar_type!($v $i, $ctx_fn, $out_ty);

        impl<'ctx> $crate::traits::PinionBasicType<'ctx> for $i {
            type BasicOut = inkwell::types::$out_ty<'ctx>;

            fn create_empty() -> Box<dyn $crate::traits::PinionIndexableType<'ctx>> {
                Box::new(Self { _dummy: () })
            }
        }
        impl<'ctx> $crate::traits::PinionIndexableType<'ctx> for $i {}
    };
}

#[macro_export]
macro_rules! int_type {
    ($v:vis $i:ident , $ctx_fn:ident) => {
        $crate::basic_scalar_type!($v $i, $ctx_fn, IntType);

        impl<'ctx> $crate::traits::PinionIntType<'ctx> for $i {}
    };
}

#[macro_export]
macro_rules! float_type {
    ($v:vis $i:ident , $ctx_fn:ident) => {
        $crate::basic_scalar_type!($v $i, $ctx_fn, FloatType);

        impl<'ctx> $crate::traits::PinionFloatType<'ctx> for $i {}
    };
}
