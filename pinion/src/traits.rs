use std::fmt::Debug;
use std::marker::{PhantomData, Sized};
use std::ptr::NonNull;

use ordered_float::OrderedFloat;

use crate::interface::Context;
use crate::layout::{self, Func, Layout, StructLayout};
use crate::layout_ctx::LayoutCtx;
use crate::types::Primitive;

pub trait PinionData: Sized {
    type Static: 'static;

    unsafe fn validate(ptr: *const u8);

    fn get_layout(lctx: &mut LayoutCtx) -> Layout;
}

pub trait PinionStruct: PinionData {}

// marker trait. must be a nicheable type
pub trait PinionPointerType: PinionData {}

pub trait PinionFuncCarrier {
    fn get_all_funcs<C: Context>(ctx: &mut C) -> Vec<(&'static str, C::FunctionType)>;
}

pub trait PinionFunc {
    fn get_func_layout(lctx: &mut LayoutCtx) -> Func;
}

pub trait PinionModule {
    type Funcs: PinionModuleFuncsEnum;
    fn get_funcs(lctx: &mut LayoutCtx) -> Vec<(Self::Funcs, Func)>;
}

pub trait PinionModuleFuncsEnum: Clone + Copy + Debug {
    fn get_name(self) -> &'static str;
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
                type Static = Self;

                unsafe fn validate(_ptr: *const u8) {}

                fn get_layout(_lctx: &mut LayoutCtx) -> Layout {
                    layout::Layout::Primitive(Primitive::$var)
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

// TODO: macro for tuples
impl<A, B> PinionData for (A, B)
where
    A: PinionData,
    B: PinionData,
{
    type Static = (A::Static, B::Static);

    unsafe fn validate(ptr: *const u8) {
        let tptr = ptr as *const Self;
        <A as PinionData>::validate(&(*tptr).0 as *const _ as _);
        <B as PinionData>::validate(&(*tptr).1 as *const _ as _);
    }

    fn get_layout(lctx: &mut LayoutCtx) -> Layout {
        Layout::Struct(StructLayout::new(&[
            ("t0", lctx.populate::<A>()),
            ("t1", lctx.populate::<B>()),
        ]))
    }
}

/*macro_rules! ptr_types {
    (@inner $t:tt, $res:tt, $ty:ty) => {
        impl <$t: $res> PinionData for $ty {
            type Static = $ty;

            unsafe fn validate(ptr: *const u8) {
                assert_eq!(std::mem::size_of::<$ty>(), 8);
                let pptr: *const u64 = ptr as _;
                assert_ne!(*pptr, 0);
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
}*/

impl<'a, T: PinionData + 'a> PinionData for *const T {
    type Static = *const T::Static;

    unsafe fn validate(ptr: *const u8) {
        let pptr: *const u64 = ptr as _;
        assert_ne!(*pptr, 0);
    }

    fn get_layout(lctx: &mut LayoutCtx) -> Layout {
        layout::Layout::Pointer(layout::Pointer::new(lctx.populate::<T>(), false))
    }
}
impl<'a, T: PinionData> PinionPointerType for *const T {}

impl<'a, T: PinionData + 'a> PinionData for *mut T {
    type Static = *mut T::Static;

    unsafe fn validate(ptr: *const u8) {
        let pptr: *const u64 = ptr as _;
        assert_ne!(*pptr, 0);
    }

    fn get_layout(lctx: &mut LayoutCtx) -> Layout {
        layout::Layout::Pointer(layout::Pointer::new(lctx.populate::<T>(), false))
    }
}
impl<'a, T: PinionData> PinionPointerType for *mut T {}

impl<'a, T: PinionData + 'a> PinionData for NonNull<T> {
    type Static = NonNull<T::Static>;

    unsafe fn validate(ptr: *const u8) {
        let pptr: *const u64 = ptr as _;
        assert_ne!(*pptr, 0);
    }

    fn get_layout(lctx: &mut LayoutCtx) -> Layout {
        layout::Layout::Pointer(layout::Pointer::new(lctx.populate::<T>(), false))
    }
}
impl<'a, T: PinionData> PinionPointerType for NonNull<T> {}

impl<'a, T: PinionData + 'a> PinionData for &'a T {
    type Static = &'static T::Static;

    unsafe fn validate(ptr: *const u8) {
        let pptr: *const u64 = ptr as _;
        assert_ne!(*pptr, 0);
    }

    fn get_layout(lctx: &mut LayoutCtx) -> Layout {
        layout::Layout::Pointer(layout::Pointer::new(lctx.populate::<T>(), false))
    }
}
impl<'a, T: PinionData> PinionPointerType for &'a T {}

impl<'a, T: PinionData + 'a> PinionData for &'a mut T {
    type Static = &'static mut T::Static;

    unsafe fn validate(ptr: *const u8) {
        let pptr: *const u64 = ptr as _;
        assert_ne!(*pptr, 0);
    }

    fn get_layout(lctx: &mut LayoutCtx) -> Layout {
        layout::Layout::Pointer(layout::Pointer::new(lctx.populate::<T>(), false))
    }
}
impl<'a, T: PinionData> PinionPointerType for &'a mut T {}

impl<T: PinionPointerType> PinionData for Option<T> {
    type Static = Option<T::Static>;

    unsafe fn validate(ptr: *const u8) {
        assert_eq!(std::mem::size_of::<Self>(), std::mem::size_of::<T>());
        assert_eq!(std::mem::size_of::<Self>(), 8);
        if (ptr as *const Self).as_ref().is_some() {
            T::validate(ptr)
        }
    }

    fn get_layout(lctx: &mut LayoutCtx) -> Layout {
        T::get_layout(lctx)
    }
}

pub struct PinionFuncPtr {
    _dummy: (),
}

impl PinionData for PinionFuncPtr {
    type Static = Self;

    unsafe fn validate(ptr: *const u8) {
        assert!(!ptr.is_null());
    }

    fn get_layout(_lctx: &mut LayoutCtx) -> Layout {
        layout::Layout::FuncPtr
    }
}
impl PinionPointerType for PinionFuncPtr {}

pub struct PinionOpaqueStruct {
    _dummy: (),
}

impl PinionData for PinionOpaqueStruct {
    type Static = Self;

    unsafe fn validate(_ptr: *const u8) {}

    fn get_layout(_lctx: &mut LayoutCtx) -> Layout {
        layout::Layout::OpaqueStruct(None)
    }
}

impl<T> PinionData for PhantomData<T> {
    type Static = PhantomData<()>;

    unsafe fn validate(ptr: *const u8) {
        panic!("should never interact with unsized type");
    }

    fn get_layout(lctx: &mut LayoutCtx) -> Layout {
        Layout::Unsized
    }
}
