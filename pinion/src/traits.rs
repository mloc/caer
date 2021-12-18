use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;

use crate::interface::Context;
use crate::types::{layout, Primitive};

pub trait PinionBasicType {
    fn create_in_context<C: Context>(ctx: &mut C) -> C::BasicType;

    fn get_layout() -> &'static layout::CycleCell;
}

pub trait PinionStruct: PinionBasicType {
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
pub trait PinionPointerType: PinionBasicType {}

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
            impl PinionBasicType for $prim {
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
        impl <$t: $res> PinionBasicType for $ty {
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

ptr_types!(T: PinionBasicType, [&T, &mut T, *const T, *mut T, NonNull<T>]);

impl<T: PinionPointerType> PinionBasicType for Option<T> {
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

impl PinionBasicType for PinionFuncPtr {
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

impl PinionBasicType for PinionOpaqueStruct {
    fn create_in_context<C: Context>(_ctx: &mut C) -> C::BasicType {
        panic!("can't call create_in_context directly for PinionOpaqueStruct")
    }

    fn get_layout() -> &'static layout::CycleCell {
        static LAYOUT: once_cell::sync::OnceCell<layout::CycleCell> =
            once_cell::sync::OnceCell::new();
        LAYOUT.get_or_init(|| layout::CycleCell::new_init(layout::BasicType::OpaqueStruct))
    }
}
