#[macro_export]
macro_rules! func_type {
    (%impl% $v:vis $name:ident, ( $($param_ty:ty),* ), $ret_ty:ty) => {
        $v struct $name {
            params: std::marker::PhantomData<(
                $($param_ty),*
            )>,
            ret: std::marker::PhantomData<$ret_ty>,
        }

        impl<'ctx> $crate::traits::PinionType<'ctx> for $name {
            type Out = inkwell::types::FunctionType<'ctx>;

            fn instantiate(ctx: &'ctx inkwell::context::Context) -> Self::Out {
                <$ret_ty>::instantiate(ctx).fn_type(
                    &[
                        $(<$param_ty>::instantiate(ctx).into()),*
                    ],
                    false)
            }

            fn debug_stringify() -> std::string::String {
                let params = [$(<$param_ty>::debug_stringify()),*];
                format!("fn {} ({}) -> {}", stringify!($name), params.join(", "), <$ret_ty>::debug_stringify())
            }
        }

        impl<'ctx> $crate::traits::PinionFuncType<'ctx> for $name {}
    };

    ($v:vis $name:ident $params:tt -> $ret_ty:ty) => {
        $crate::func_type!(%impl% $v $name, $params, $ret_ty);
    };

    ($v:vis $name:ident $params:tt) => {
        $crate::func_type!(%impl% $v $name, $params, $crate::types::Void);
    };
}
