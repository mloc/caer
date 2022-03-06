use std::marker::Sized;
use std::ops::Deref;
use std::ptr::NonNull;

use ordered_float::OrderedFloat;

use crate::interface::Context;
use crate::layout::{self, BasicType};
use crate::layout_ctx::LayoutCtx;
use crate::types::Primitive;

pub trait PinionData: Sized + 'static {
    unsafe fn validate(ptr: *const u8);

    fn create_in_context<C: Context>(ctx: &mut C) -> C::BasicType;

    fn get_layout(lctx: &mut LayoutCtx) -> BasicType;
}

pub trait PinionStruct: PinionData {}

// marker trait. must be a nicheable type
pub trait PinionPointerType: PinionData {}

pub trait PinionFuncCarrier {
    fn get_all_funcs<C: Context>(ctx: &mut C) -> Vec<(&'static str, C::FunctionType)>;
}

// Everything below here should probably be moved...

#[macro_export]
macro_rules! gep_path {
    ($($part:ident).+) => {
        &[$(stringify!($part)),+]
    };
}

macro_rules! prim_types {
    ($($($prim:ty)|+ => $var:ident),+ $(,)?) => {
        $($(
            impl PinionData for $prim {
                unsafe fn validate(_ptr: *const u8) {}

                fn create_in_context<C: Context>(ctx: &mut C) -> C::BasicType {
                    ctx.make_primitive_type(Primitive::$var)
                }

                fn get_layout(_lctx: &mut LayoutCtx) -> BasicType {
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
    f32 | OrderedFloat<f32> => Float32,
    f64 | OrderedFloat<f64> => Float64,
}

macro_rules! ptr_types {
    (@inner $t:tt, $res:tt, $ty:ty) => {
        impl <$t: $res> PinionData for $ty {
            unsafe fn validate(ptr: *const u8) {
                assert_eq!(std::mem::size_of::<$ty>(), 8);
                let pptr: *const u64 = ptr as _;
                assert_ne!(*pptr, 0);
            }

            fn create_in_context<C: Context>(ctx: &mut C) -> C::BasicType {
                let elem_type = $t::create_in_context(ctx);
                ctx.make_pointer_type(elem_type, false)
            }

            fn get_layout(lctx: &mut LayoutCtx) -> BasicType {
                layout::BasicType::Pointer(layout::Pointer::new(lctx.populate::<$t>(), false))
            }
        }
        impl <$t: $res> PinionPointerType for $ty {}
    };

    ($t:ident : $res:tt, [$($ty:ty),+$(,)?]) => {
        $(ptr_types!(@inner $t, $res, $ty);)+
    }
}

ptr_types!(T: PinionData, [&'static T, &'static mut T, *const T, *mut T, NonNull<T>]);

impl<T: PinionPointerType> PinionData for Option<T> {
    unsafe fn validate(ptr: *const u8) {
        assert_eq!(std::mem::size_of::<Self>(), std::mem::size_of::<T>());
        assert_eq!(std::mem::size_of::<Self>(), 8);
        if (ptr as *const Self).as_ref().is_some() {
            T::validate(ptr)
        }
    }

    fn create_in_context<C: Context>(ctx: &mut C) -> C::BasicType {
        T::create_in_context(ctx)
    }

    fn get_layout(lctx: &mut LayoutCtx) -> BasicType {
        T::get_layout(lctx)
    }
}

pub struct PinionFuncPtr {
    _dummy: (),
}

impl PinionData for PinionFuncPtr {
    unsafe fn validate(ptr: *const u8) {
        assert!(!ptr.is_null());
    }

    fn create_in_context<C: Context>(_ctx: &mut C) -> C::BasicType {
        panic!("can't call create_in_context directly for PinionFuncPtr")
    }

    fn get_layout(_lctx: &mut LayoutCtx) -> BasicType {
        layout::BasicType::FuncPtr
    }
}
impl PinionPointerType for PinionFuncPtr {}

pub struct PinionOpaqueStruct {
    _dummy: (),
}

impl PinionData for PinionOpaqueStruct {
    unsafe fn validate(_ptr: *const u8) {}

    fn create_in_context<C: Context>(_ctx: &mut C) -> C::BasicType {
        panic!("can't call create_in_context directly for PinionOpaqueStruct")
    }

    fn get_layout(_lctx: &mut LayoutCtx) -> BasicType {
        layout::BasicType::OpaqueStruct
    }
}
