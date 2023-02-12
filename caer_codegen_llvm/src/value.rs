use std::borrow::Borrow;
use std::ffi::c_void;
use std::marker::PhantomData;
use std::ptr::NonNull;

use caer_runtime::rtti::RttiRef;
use caer_runtime::string::RtString;
use caer_runtime::val::Val;
use caer_types::id::TypeId;
use inkwell::types::{AnyTypeEnum, BasicTypeEnum, FunctionType};
use inkwell::values::{
    BasicValue, BasicValueEnum, GlobalValue, InstructionOpcode, IntValue, PointerValue,
};
use pinion::layout_ctx::LayoutId;
use pinion::{
    PinionData, PinionEnum, PinionField, PinionPointerType, PinionPrim, PinionStruct,
    PinionValueHolder,
};

use crate::context::Context;

fn any_to_basic(any: AnyTypeEnum) -> BasicTypeEnum {
    match any {
        AnyTypeEnum::ArrayType(ty) => ty.into(),
        AnyTypeEnum::FloatType(ty) => ty.into(),
        AnyTypeEnum::IntType(ty) => ty.into(),
        AnyTypeEnum::PointerType(ty) => ty.into(),
        AnyTypeEnum::StructType(ty) => ty.into(),
        AnyTypeEnum::VectorType(ty) => ty.into(),
        AnyTypeEnum::VoidType(_) | AnyTypeEnum::FunctionType(_) | AnyTypeEnum::TokenType(_) => {
            panic!("{:?} is not basic", any)
        },
    }
}

#[derive(Debug)]
pub struct ReifiedValue<'ctx> {
    pub inkwell_value: BasicValueEnum<'ctx>,
    pub layout_id: LayoutId,
}

#[derive(Debug)]
pub struct BrandedValue<'ctx, T> {
    pub val: BasicValueEnum<'ctx>,
    pub layout_id: LayoutId,
    //ty: TypeId,
    phantom: PhantomData<T>,
}

impl<'ctx, T: PinionData> PinionValueHolder<T> for BrandedValue<'ctx, T> {
    type Reified = BasicValueEnum<'ctx>;

    fn reify(&self) -> Self::Reified {
        self.val
    }
}

// We manually impl Clone and Copy because we don't want to require T:Clone+Copy
impl<'ctx, T> Clone for BrandedValue<'ctx, T> {
    fn clone(&self) -> Self {
        Self {
            val: self.val,
            layout_id: self.layout_id,
            phantom: PhantomData,
        }
    }
}
impl<'ctx, T> Copy for BrandedValue<'ctx, T> {}

impl<'ctx, T: PinionData> BrandedValue<'ctx, T> {
    // TODO: checks
    unsafe fn new(val: BasicValueEnum<'ctx>, layout_id: LayoutId /*, ty: TypeId*/) -> Self {
        Self {
            val,
            layout_id,
            //ty,
            phantom: PhantomData,
        }
    }

    // TODO: checks!!!
    pub unsafe fn materialize(ctx: &Context<'_, 'ctx>, val: BasicValueEnum<'ctx>) -> Self {
        let ty = ctx.get_type::<T>();
        assert_eq!(Some(val.get_type()), ty.get_ty());
        Self::new(val, ty.get_id())
    }

    // TODO: copy/prim bound maybe
    pub fn alloca_emplace(self, ctx: &Context<'_, 'ctx>) -> BrandedValue<'ctx, *mut T> {
        let alloca = BrandedValue::<*mut T>::build_as_alloca(ctx);
        alloca.build_store(ctx, self);
        alloca
    }
}

impl<'ctx, T: PrimLiteral> BrandedValue<'ctx, T> {
    pub fn literal(lit: T, ctx: &Context<'_, 'ctx>) -> BrandedValue<'ctx, T> {
        let val = lit.make_llval(ctx);
        unsafe { Self::materialize(ctx, val) }
    }

    // very nasty
    pub unsafe fn bitcast_self<O: PinionData>(
        self, ctx: &Context<'_, 'ctx>,
    ) -> BrandedValue<'ctx, O> {
        let cast = ctx
            .builder
            .build_bitcast(self.val, ctx.get_llvm_type::<O>(), "");
        unsafe { BrandedValue::<O>::materialize(ctx, cast) }
    }
}

impl<'ctx, T: PinionPointerType> BrandedValue<'ctx, T> {
    pub fn build_as_alloca(ctx: &Context<'_, 'ctx>) -> Self {
        let ty = ctx.get_llvm_type::<T>().into_pointer_type();
        unsafe {
            Self::materialize(
                ctx,
                ctx.builder
                    .build_alloca(any_to_basic(ty.get_element_type()), "")
                    .into(),
            )
        }
    }

    pub fn build_as_alloca_array(ctx: &Context<'_, 'ctx>, n: u64) -> Self {
        let ty = ctx.get_llvm_type::<T>().into_pointer_type();
        unsafe {
            Self::materialize(
                ctx,
                ctx.builder
                    .build_array_alloca(
                        any_to_basic(ty.get_element_type()),
                        ctx.llvm_ctx.i64_type().const_int(n, false),
                        "",
                    )
                    .into(),
            )
        }
    }

    pub fn copy(ctx: &Context<'_, 'ctx>, src: Self, dest: Self) {
        assert_eq!(src.val.get_type(), dest.val.get_type());

        let size = ctx.get_store_size::<T>();

        let memcpy_intrinsic = unsafe {
            ctx.get_intrinsic(
                "llvm.memcpy",
                &[
                    src.val.get_type(),
                    dest.val.get_type(),
                    ctx.llvm_ctx.i64_type().into(),
                ],
            )
            .unwrap()
        };

        ctx.builder.build_call(
            memcpy_intrinsic,
            &[
                dest.val.into(),
                src.val.into(),
                ctx.llvm_ctx.i64_type().const_int(size, false).into(),
                ctx.llvm_ctx.bool_type().const_zero().into(),
            ],
            "",
        );
    }

    pub fn copy_to_alloca(self, ctx: &Context<'_, 'ctx>) -> Self {
        let dest = Self::build_as_alloca(ctx);
        Self::copy(ctx, self, dest);
        dest
    }

    pub unsafe fn array_gep(self, ctx: &Context<'_, 'ctx>, n: u64) -> BrandedValue<'ctx, T> {
        let ptr = unsafe { ctx.const_gep(self.ptr_val(), &[n]) };
        unsafe { BrandedValue::materialize(ctx, ptr.into()) }
    }

    pub fn build_load(self, ctx: &Context<'_, 'ctx>) -> BrandedValue<'ctx, T::Element> {
        let val = ctx.builder.build_load(self.ptr_val(), "");
        unsafe { BrandedValue::materialize(ctx, val) }
    }

    pub fn build_store(self, ctx: &Context<'_, 'ctx>, val: BrandedValue<'ctx, T::Element>) {
        let val = ctx.builder.build_store(self.ptr_val(), val.val);
    }

    pub fn ptr_val(self) -> PointerValue<'ctx> {
        self.val.into_pointer_value()
    }

    // Only changes the kind of pointer, not the pointed to value.
    // e.g. *mut u8 -> &mut u8
    // TODO: disallow const->mut? idk, mutability is kinda moot anyway
    // TODO: worry about gc-marking
    pub fn cast_ptr<O: PinionPointerType<Element = T::Element>>(
        self, ctx: &Context<'_, 'ctx>,
    ) -> BrandedValue<'ctx, O> {
        unsafe { BrandedValue::materialize(ctx, self.val) }
    }

    // TODO: with opaque pointers, remove ctx
    pub unsafe fn cast_value<O: PinionPointerType>(
        self, ctx: &Context<'_, 'ctx>,
    ) -> BrandedValue<'ctx, O> {
        let cast = ctx
            .builder
            .build_bitcast(self.val, ctx.get_llvm_type::<O>(), "");
        BrandedValue::materialize(ctx, cast)
    }

    pub unsafe fn cast_void(self, ctx: &Context<'_, 'ctx>) -> BrandedValue<'ctx, *mut c_void> {
        self.cast_value(ctx)
    }
}

impl<'ctx, T> BrandedValue<'ctx, T>
where
    T: PinionPointerType,
    T::Element: PinionEnum,
{
    pub fn gep_disc(
        self, ctx: &Context<'_, 'ctx>,
    ) -> BrandedValue<'ctx, *mut <T::Element as PinionEnum>::Disc> {
        let ptr = unsafe { ctx.const_gep(self.ptr_val(), &[0, 0]) };
        unsafe { BrandedValue::materialize(ctx, ptr.into()) }
    }

    pub fn gep_value(self, ctx: &Context<'_, 'ctx>) -> BrandedValue<'ctx, *mut c_void> {
        let aptr = unsafe { ctx.const_gep(self.ptr_val(), &[0, 2]) };
        let optr = ctx
            .builder
            .build_bitcast(aptr, ctx.get_llvm_type_ptr::<c_void>(), "");
        unsafe { BrandedValue::materialize(ctx, optr) }
    }
}

impl<'ctx, T, S> BrandedValue<'ctx, T>
where
    S: PinionStruct,
    T: PinionPointerType<Element = S>,
{
    pub fn gep_field<const N: u32, F: PinionData>(
        self, ctx: &Context<'_, 'ctx>, field: PinionField<N, S, F>,
    ) -> BrandedValue<'ctx, *mut F> {
        let ptr = unsafe { ctx.const_gep(self.ptr_val(), &[0, N as _]) };
        unsafe { BrandedValue::materialize(ctx, ptr.into()) }
    }
}

impl<'ctx> BrandedValue<'ctx, f32> {
    pub fn cast_f32_to_i32(self, ctx: &Context<'_, 'ctx>) -> BrandedValue<'ctx, i32> {
        let cast_maybe = ctx.builder.build_cast(
            InstructionOpcode::FPToSI,
            self.val,
            ctx.llvm_ctx.i32_type(),
            "",
        );
        // TODO: freeze, or handle poison values in some other way
        unsafe { BrandedValue::materialize(ctx, cast_maybe) }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BrandedStackValue<'ctx> {
    Val(BrandedValue<'ctx, *mut Val>),
    Null,
    Float(BrandedValue<'ctx, *mut f32>),
    String(BrandedValue<'ctx, *mut Option<NonNull<RtString>>>),
    Ref(BrandedValue<'ctx, *mut RttiRef>),
}

impl<'ctx> BrandedStackValue<'ctx> {
    pub fn set_name(&self, name: &str) {
        if let Some(bve) = self.as_bve() {
            bve.set_name(name)
        }
    }

    // temporary glue
    pub fn as_bve(&self) -> Option<BasicValueEnum<'ctx>> {
        match self {
            BrandedStackValue::Val(v) => Some(v.val),
            BrandedStackValue::Null => None,
            BrandedStackValue::Float(v) => Some(v.val),
            BrandedStackValue::String(v) => Some(v.val),
            BrandedStackValue::Ref(v) => Some(v.val),
        }
    }

    pub fn as_val(&self) -> Option<BrandedValue<'ctx, *mut Val>> {
        if let Self::Val(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    pub fn as_float(&self) -> Option<BrandedValue<'ctx, *mut f32>> {
        if let Self::Float(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    pub fn as_string(&self) -> Option<BrandedValue<'ctx, *mut Option<NonNull<RtString>>>> {
        if let Self::String(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    pub fn as_ref(&self) -> Option<BrandedValue<'ctx, *mut RttiRef>> {
        if let Self::Ref(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    pub fn is_heapptr(self) -> bool {
        match self {
            BrandedStackValue::Val(_) => true,
            BrandedStackValue::Null => false,
            BrandedStackValue::Float(_) => false,
            BrandedStackValue::String(_) => true,
            BrandedStackValue::Ref(_) => true,
        }
    }

    pub fn copy_to_alloca(self, ctx: &Context<'_, 'ctx>) -> Self {
        match self {
            BrandedStackValue::Val(val) => val.copy_to_alloca(ctx).into(),
            BrandedStackValue::Null => self,
            BrandedStackValue::Float(val) => val.copy_to_alloca(ctx).into(),
            BrandedStackValue::String(val) => val.copy_to_alloca(ctx).into(),
            BrandedStackValue::Ref(val) => val.copy_to_alloca(ctx).into(),
        }
    }
}

impl<'ctx> From<BrandedValue<'ctx, *mut Val>> for BrandedStackValue<'ctx> {
    fn from(from: BrandedValue<'ctx, *mut Val>) -> Self {
        Self::Val(from)
    }
}

impl<'ctx> From<BrandedValue<'ctx, *mut f32>> for BrandedStackValue<'ctx> {
    fn from(from: BrandedValue<'ctx, *mut f32>) -> Self {
        Self::Float(from)
    }
}

impl<'ctx> From<BrandedValue<'ctx, *mut Option<NonNull<RtString>>>> for BrandedStackValue<'ctx> {
    fn from(from: BrandedValue<'ctx, *mut Option<NonNull<RtString>>>) -> Self {
        Self::String(from)
    }
}

impl<'ctx> From<BrandedValue<'ctx, *mut RttiRef>> for BrandedStackValue<'ctx> {
    fn from(from: BrandedValue<'ctx, *mut RttiRef>) -> Self {
        Self::Ref(from)
    }
}

pub trait PrimLiteral: PinionPrim {
    fn make_llval<'ctx>(self, ctx: &Context<'_, 'ctx>) -> BasicValueEnum<'ctx>;
}

macro_rules! primlit_ints {
    ($($($prim:ty)|+ => $cons:ident),+ $(,)?) => {
        $($(
            impl PrimLiteral for $prim {
                fn make_llval<'ctx>(self, ctx: &Context<'_, 'ctx>) -> BasicValueEnum<'ctx> {
                    ctx.llvm_ctx.$cons().const_int(self as _, false).into()
                }
            }
        )+)+
    }
}

macro_rules! primlit_floats {
    ($($($prim:ty)|+ => $cons:ident),+ $(,)?) => {
        $($(
            impl PrimLiteral for $prim {
                fn make_llval<'ctx>(self, ctx: &Context<'_, 'ctx>) -> BasicValueEnum<'ctx> {
                    ctx.llvm_ctx.$cons().const_float(self as _).into()
                }
            }
        )+)+
    }
}

primlit_ints! {
    u8 | i8 => i8_type,
    u16 | i16 => i16_type,
    u32 | i32 => i32_type,
    u64 | i64 => i64_type,
}

// TODO: orderedfloats
primlit_floats! {
    f32 => f32_type,
    f64 => f64_type,
}

pub struct MaybeRet<'ctx, V> {
    maybe_val: Option<BasicValueEnum<'ctx>>,
    phantom: PhantomData<V>,
}

impl<'ctx, V> MaybeRet<'ctx, V> {
    pub fn create(maybe_val: Option<BasicValueEnum<'ctx>>) -> Self {
        Self {
            maybe_val,
            phantom: PhantomData,
        }
    }
}

impl<'ctx, V: PinionData> MaybeRet<'ctx, V> {
    pub fn result(self, ctx: &Context<'_, 'ctx>) -> BrandedValue<'ctx, V> {
        unsafe { BrandedValue::materialize(ctx, self.maybe_val.unwrap()) }
    }
}
