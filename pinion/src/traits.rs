use std::ops::Deref;
use std::ptr::NonNull;

use crate::interface::Context;
use crate::types::{layout, Primitive};

pub trait PinionData {
    fn validate(&self);

    fn create_in_context<C: Context>(ctx: &mut C) -> C::BasicType;

    fn get_layout() -> &'static layout::CycleCell;
}

pub trait PinionStruct: PinionData {
    fn get_gep_indices(path: &[&'static str]) -> Vec<u64> {
        let mut indices = vec![0];

        let mut cur_layout = Self::get_layout();
        for part in path {
            let (index, new_layout) = match cur_layout.get().deref() {
                layout::BasicType::Struct(sl) => sl.lookup_field(part).unwrap(),
                _ => panic!(),
            };

            cur_layout = new_layout;
            indices.push(index as u64)
        }

        indices
    }
}

// marker trait
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
                fn validate(&self) {}

                fn create_in_context<C: Context>(ctx: &mut C) -> C::BasicType {
                    ctx.make_primitive_type(Primitive::$var)
                }

                fn get_layout() -> &'static layout::CycleCell {
                    static LAYOUT: once_cell::sync::OnceCell<layout::CycleCell> = once_cell::sync::OnceCell::new();
                    LAYOUT.get_or_init(|| {
                        layout::CycleCell::new_init(layout::BasicType::Primitive(Primitive::$var))
                    })
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

macro_rules! ptr_types {
    (@inner $t:tt, $res:tt, $ty:ty) => {
        impl <$t: $res> PinionData for $ty {
            fn validate(&self) {
                assert_eq!(std::mem::size_of::<$ty>(), 8);
                unsafe {
                    let pptr: *const u64 = std::mem::transmute(self);
                    assert_ne!(*pptr, 0);
                }
            }

            fn create_in_context<C: Context>(ctx: &mut C) -> C::BasicType {
                let elem_type = $t::create_in_context(ctx);
                ctx.make_pointer_type(elem_type, false)
            }

            fn get_layout() -> &'static layout::CycleCell {
                static LAYOUT: once_cell::sync::OnceCell<layout::CycleCell> =
                    once_cell::sync::OnceCell::new();

                if let Some(v) = LAYOUT.get() {
                    return v;
                }

                match LAYOUT.set(layout::CycleCell::new_empty()) {
                    Ok(()) => {},
                    Err(_) => panic!(),
                }

                let ty = layout::BasicType::Pointer(layout::Pointer::new($t::get_layout(), false));

                let layout = LAYOUT.get().unwrap();
                layout.update(ty);
                layout
            }
        }
        impl <$t: $res> PinionPointerType for $ty {}
    };

    ($t:ident : $res:tt, [$($ty:ty),+$(,)?]) => {
        $(ptr_types!(@inner $t, $res, $ty);)+
    }
}

ptr_types!(T: PinionData, [&T, &mut T, *const T, *mut T, NonNull<T>]);

impl<T: PinionPointerType> PinionData for Option<T> {
    fn validate(&self) {
        assert_eq!(std::mem::size_of::<Self>(), std::mem::size_of::<T>());
        if let Some(x) = self {
            x.validate()
        }
    }

    fn create_in_context<C: Context>(ctx: &mut C) -> C::BasicType {
        T::create_in_context(ctx)
    }

    fn get_layout() -> &'static layout::CycleCell {
        T::get_layout()
    }
}

pub struct PinionFuncPtr {
    _dummy: (),
}

impl PinionData for PinionFuncPtr {
    fn validate(&self) {}

    fn create_in_context<C: Context>(_ctx: &mut C) -> C::BasicType {
        panic!("can't call create_in_context directly for PinionFuncPtr")
    }

    fn get_layout() -> &'static layout::CycleCell {
        static LAYOUT: once_cell::sync::OnceCell<layout::CycleCell> =
            once_cell::sync::OnceCell::new();
        LAYOUT.get_or_init(|| layout::CycleCell::new_init(layout::BasicType::FuncPtr))
    }
}
impl PinionPointerType for PinionFuncPtr {}

pub struct PinionOpaqueStruct {
    _dummy: (),
}

impl PinionData for PinionOpaqueStruct {
    fn validate(&self) {}

    fn create_in_context<C: Context>(_ctx: &mut C) -> C::BasicType {
        panic!("can't call create_in_context directly for PinionOpaqueStruct")
    }

    fn get_layout() -> &'static layout::CycleCell {
        static LAYOUT: once_cell::sync::OnceCell<layout::CycleCell> =
            once_cell::sync::OnceCell::new();
        LAYOUT.get_or_init(|| layout::CycleCell::new_init(layout::BasicType::OpaqueStruct))
    }
}
