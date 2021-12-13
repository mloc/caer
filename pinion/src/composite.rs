#[macro_export]
macro_rules! struct_type {
    (@fmt_debug {$($field_name:ident : $field_ty:ty) , * $(,)?}) => {
        [
            $(
                format!("{}: {}", stringify!($field_name), <$field_ty>::debug_stringify())
            ),*
        ]
    };

    (@struct_ty_parts $ctx:expr, {$($_:ident : $field_ty:ty) , * $(,)?}) => {
        [
            $(<$field_ty>::instantiate($ctx).into()),*
        ]
    };

    (@gen_count_enum $name:ident, {$($field_name:ident : $_:ty) , * $(,)?}) => {
        #[allow(non_camel_case_types)]
        #[allow(unused)]
        enum $name {
            $($field_name),*
        }
    };

    (@resolve_matcher $in:ident, $name:ident, {$($field_name:ident : $field_ty:ty) , * $(,)?}) => {
        match $in {
            $(
                stringify!($field_name) => {
                    let index: u64 = $name::$field_name as u64;
                    Some((index, <$field_ty as $crate::traits::PinionBasicType>::create_empty()))
                },
            )*
            _ => None,
        }
    };

    (@resolve_fn $fields:tt) => {
        fn resolve_index(
            &self, field: &'static str,
        ) -> Option<(u64, Box<dyn $crate::traits::PinionIndexableType<'ctx>>)> {
            $crate::struct_type!(@gen_count_enum Counter, $fields);
            $crate::struct_type!(@resolve_matcher field, Counter, $fields)
        }
    };

    {@entry $v:vis $i:ident $fields:tt, $packed:expr, $name:expr $(,)?} => {
        $v struct $i {
            _dummy: ()
        }

        impl<'ctx> $crate::traits::PinionType<'ctx> for $i {
            type Out = inkwell::types::StructType<'ctx>;

            fn instantiate(ctx: &'ctx inkwell::context::Context) -> Self::Out {
                ctx.struct_type(
                    &$crate::struct_type!(@struct_ty_parts ctx, $fields),
                    $packed,
                )
            }

            fn debug_stringify() -> String {
                let fields = $crate::struct_type!(@fmt_debug $fields);
                format!("{} {{{}}}", stringify!($i), fields.join(", "))
            }
        }

        impl<'ctx> $crate::traits::PinionBasicType<'ctx> for $i {
            type BasicOut = Self::Out;

            fn create_empty() -> Box<dyn $crate::traits::PinionIndexableType<'ctx>> {
                todo!()
            }
        }
        impl<'ctx> $crate::traits::PinionIndexableType<'ctx> for $i {
            $crate::struct_type!(@resolve_fn $fields);
        }
    };

    {@entry $v:vis $i:ident $field_tys:tt, $packed:expr $(,)?} => {
        $crate::struct_type!{$v $i $field_tys, $packed, ""}
    };

    {$($part:tt)+} => {
        $crate::struct_type!{@entry $($part)+}
    };
}
