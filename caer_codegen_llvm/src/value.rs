// TODO: break all of this out into something more generic like pinion_inkwell

use std::borrow::Borrow;
use std::ffi::c_void;
use std::marker::PhantomData;
use std::ptr::NonNull;

use caer_runtime::rtti::RttiRef;
use caer_runtime::string::RtString;
use caer_runtime::val::Val;
use caer_types::id::TypeId;
use inkwell::types::{AnyTypeEnum, BasicTypeEnum, FunctionType};
use inkwell::values::{BasicValue, BasicValueEnum, InstructionOpcode, PointerValue};
use pinion::layout_ctx::LayoutId;
use pinion::{
    PinionData, PinionEnum, PinionField, PinionPointerType, PinionPrim, PinionStruct,
    PinionTaggedUnion, PinionValueHolder,
};

use crate::context::Context;

// TODO: ERRH
fn any_to_basic(any: AnyTypeEnum) -> BasicTypeEnum {
    match any {
        AnyTypeEnum::ArrayType(ty) => ty.into(),
        AnyTypeEnum::FloatType(ty) => ty.into(),
        AnyTypeEnum::IntType(ty) => ty.into(),
        AnyTypeEnum::PointerType(ty) => ty.into(),
        AnyTypeEnum::StructType(ty) => ty.into(),
        AnyTypeEnum::VectorType(ty) => ty.into(),
        AnyTypeEnum::VoidType(_)
        | AnyTypeEnum::FunctionType(_)
        | AnyTypeEnum::TokenType(_)
        | AnyTypeEnum::ScalableVectorType(_) => {
            panic!("{:?} is not basic", any)
        },
    }
}

#[derive(Debug)]
pub struct ReifiedValue<'ctx> {
    pub inkwell_value: BasicValueEnum<'ctx>,
    pub layout_id: LayoutId,
}

impl<'ctx> ReifiedValue<'ctx> {
    pub unsafe fn lift<T: PinionData>(&self) -> BrandedValue<'ctx, T> {
        assert!(self.layout_id.is::<T>());
        BrandedValue::new(self.inkwell_value, self.layout_id)
    }
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
        assert!(layout_id.is::<T>());
        Self {
            val,
            layout_id,
            //ty,
            phantom: PhantomData,
        }
    }

    // TODO: checks!!!
    pub unsafe fn materialize(ctx: &Context<'ctx>, val: BasicValueEnum<'ctx>) -> Self {
        let ty = ctx.r.get_type::<T>();
        assert_eq!(Some(val.get_type()), ty.get_ty());
        Self::new(val, ty.get_id())
    }

    // TODO: copy/prim bound maybe
    pub fn alloca_emplace(self, ctx: &Context<'ctx>) -> BrandedValue<'ctx, *mut T> {
        let alloca = BrandedValue::<*mut T>::build_as_alloca(ctx);
        alloca.build_store(ctx, self);
        alloca
    }
}

impl<'ctx, T> BrandedValue<'ctx, T>
where
    T: PinionEnum,
    T::Disc: PrimLiteral,
{
    pub fn literal_enum(lit: T, ctx: &Context<'ctx>) -> BrandedValue<'ctx, T> {
        unsafe { BrandedValue::<T::Disc>::literal(lit.to_disc(), ctx).bitcast_self(ctx) }
    }
}

impl<'ctx, T: PrimLiteral> BrandedValue<'ctx, T> {
    pub fn literal(lit: T, ctx: &Context<'ctx>) -> BrandedValue<'ctx, T> {
        let val = lit.make_llval(ctx);
        // Safety: val from make_llval should match T
        unsafe { Self::materialize(ctx, val) }
    }

    // very nasty
    pub unsafe fn bitcast_self<O: PinionData>(self, ctx: &Context<'ctx>) -> BrandedValue<'ctx, O> {
        let cast = ctx
            .builder
            .build_bit_cast(self.val, ctx.r.get_llvm_type::<O>(), "")
            .unwrap();
        // Safety: inherently unsafe, so this fn is unsafe
        BrandedValue::<O>::materialize(ctx, cast)
    }
}

impl<'ctx, T: PinionPointerType> BrandedValue<'ctx, T> {
    // Create an instance of a pointer type by stack-allocating
    // Data is uninitialized
    pub fn build_as_alloca(ctx: &Context<'ctx>) -> Self {
        let elem_ty = ctx.r.get_llvm_type::<T::Element>();
        let alloca = ctx.builder.build_alloca(elem_ty, "");
        // Safety: alloca creates a raw pointer to appropriate memory
        unsafe { Self::materialize(ctx, alloca.into()) }
    }

    pub fn build_as_alloca_array(ctx: &Context<'ctx>, n: u64) -> Self {
        let elem_ty = ctx.r.get_llvm_type::<T::Element>();
        let n_val = ctx.llvm_ctx.i64_type().const_int(n, false);
        let alloca = ctx.builder.build_array_alloca(elem_ty, n_val, "");
        // Safety: array alloca creates a raw pointer to appropriate memory
        unsafe { Self::materialize(ctx, alloca.into()) }
    }

    pub fn copy(ctx: &Context<'ctx>, src: Self, dest: Self) {
        assert_eq!(src.val.get_type(), dest.val.get_type());

        let size = ctx.r.get_store_size::<T>();

        let memcpy_intrinsic = ctx
            .get_intrinsic_raw(
                "llvm.memcpy",
                &[
                    src.val.get_type(),
                    dest.val.get_type(),
                    ctx.llvm_ctx.i64_type().into(),
                ],
            )
            .unwrap();

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

    pub fn copy_to_alloca(self, ctx: &Context<'ctx>) -> Self {
        let dest = Self::build_as_alloca(ctx);
        Self::copy(ctx, self, dest);
        dest
    }

    pub unsafe fn array_gep(self, ctx: &Context<'ctx>, n: u64) -> BrandedValue<'ctx, T> {
        let ptr = unsafe { ctx.const_gep(self.ptr_val(), &[n]) };
        // Safety: inherently unsafe, we don't know that n is in bounds
        // TODO: a future array wrapper type could help solve this
        BrandedValue::materialize(ctx, ptr.into())
    }

    pub fn build_load(self, ctx: &Context<'ctx>) -> BrandedValue<'ctx, T::Element> {
        // TODO: maybe a check to find uses of FCAs?
        let val = ctx.builder.build_load(self.ptr_val(), "");
        // Safety: value loaded from a pointer to type E should be of type E
        unsafe { BrandedValue::materialize(ctx, val) }
    }

    pub fn build_store(self, ctx: &Context<'ctx>, val: BrandedValue<'ctx, T::Element>) {
        // TODO: maybe a check to find uses of FCAs?
        ctx.builder.build_store(self.ptr_val(), val.val);
    }

    pub fn ptr_val(self) -> PointerValue<'ctx> {
        self.val.into_pointer_value()
    }

    // Only changes the kind of pointer, not the pointed to value.
    // e.g. *mut u8 -> &mut u8
    // TODO: disallow const->mut? idk, mutability is kinda moot anyway
    // TODO: worry about gc-marking
    pub fn cast_ptr<O: PinionPointerType<Element = T::Element>>(
        self, ctx: &Context<'ctx>,
    ) -> BrandedValue<'ctx, O> {
        // Safety: all PinionPointerType instances have the same data layout
        unsafe { BrandedValue::materialize(ctx, self.val) }
    }

    // TODO: with opaque pointers, remove ctx
    pub unsafe fn cast_value<O: PinionPointerType>(
        self, ctx: &Context<'ctx>,
    ) -> BrandedValue<'ctx, O> {
        let cast = ctx
            .builder
            .build_bitcast(self.val, ctx.r.get_llvm_type::<O>(), "");
        // Safety: inherently unsafe, so fn is unsafe
        BrandedValue::materialize(ctx, cast)
    }

    pub unsafe fn cast_void(self, ctx: &Context<'ctx>) -> BrandedValue<'ctx, *mut c_void> {
        // Safety: not inherently unsafe, but can lead to unsafe situations, so fn is unsafe
        self.cast_value(ctx)
    }
}

impl<'ctx, T, TU> BrandedValue<'ctx, T>
where
    T: PinionPointerType<Element = TU>,
    TU: PinionTaggedUnion,
{
    pub fn gep_disc(self, ctx: &Context<'ctx>) -> BrandedValue<'ctx, *mut TU::Tag> {
        let ptr = unsafe { ctx.const_gep(self.ptr_val(), &[0, 0]) };
        // Safety: assuming TUs are {enum, union}, doing [0,0] on a TU pointer will give you a tag
        // pointer.
        unsafe { BrandedValue::materialize(ctx, ptr.into()) }
    }

    pub fn gep_value(self, ctx: &Context<'ctx>) -> BrandedValue<'ctx, *mut TU::Union> {
        let ptr = unsafe { ctx.const_gep(self.ptr_val(), &[0, 1]) };
        // Safety: assuming TUs are {enum, union}, doing [0,1] on a TU pointer will give you a
        // union pointer.
        unsafe { BrandedValue::materialize(ctx, ptr.into()) }
    }
}

impl<'ctx, T, S> BrandedValue<'ctx, T>
where
    S: PinionStruct,
    T: PinionPointerType<Element = S>,
{
    // Something odd: _field here is unused in the body, but it's used to constrain the generics.
    // This is weird and hacky, but I don't think there's a better way to namespace types like in a
    // way that can be used for fns...
    pub fn gep_field<const N: u32, F: PinionData>(
        self, ctx: &Context<'ctx>, _field: PinionField<N, S, F>,
    ) -> BrandedValue<'ctx, *mut F> {
        let ptr = unsafe { ctx.const_gep(self.ptr_val(), &[0, N as _]) };
        // Safety: We need to assume that the PinionField has correct generic parameters. Creating
        // a PinionField is unsafe and (mostly) only done in derived code; in which case, we can
        // trust it.
        unsafe { BrandedValue::materialize(ctx, ptr.into()) }
    }
}

impl<'ctx> BrandedValue<'ctx, f32> {
    pub fn cast_f32_to_i32(self, ctx: &Context<'ctx>) -> BrandedValue<'ctx, i32> {
        let cast_maybe = ctx.builder.build_cast(
            InstructionOpcode::FPToSI,
            self.val,
            ctx.llvm_ctx.i32_type(),
            "",
        );
        // Safety: The FPToSI cast is configured to produce a signed int32.
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

    pub fn copy_to_alloca(self, ctx: &Context<'ctx>) -> Self {
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

pub struct GlobalValue<'ctx, T> {
    val: inkwell::values::GlobalValue<'ctx>,
    layout_id: LayoutId,
    phantom: PhantomData<T>,
}

impl<'ctx, T: PinionData> GlobalValue<'ctx, T> {
    pub fn create(ctx: &Context<'ctx>, name: Option<&str>) -> Self {
        let ty = ctx.r.get_type::<T>();
        let ty_ll = ty.get_ty().unwrap();
        let val = ctx.module.add_global(ty_ll, None, name.unwrap_or_default());
        Self {
            val,
            layout_id: ty.get_id(),
            phantom: PhantomData,
        }
    }

    pub fn set_constant(&mut self, constant: bool) {
        self.val.set_constant(constant)
    }

    pub fn as_ptr(&self, ctx: &Context<'ctx>) -> BrandedValue<'ctx, *mut T> {
        let ptr_val = self.val.as_pointer_value().into();
        // Safety: for a global value of type T, its pointer value is a valid *mut T (modulo
        // lifetimes)
        unsafe { BrandedValue::materialize(ctx, ptr_val) }
    }
}

pub trait PrimLiteral: PinionPrim {
    fn make_llval<'ctx>(self, ctx: &Context<'ctx>) -> BasicValueEnum<'ctx>;
}

macro_rules! primlit_ints {
    ($($($prim:ty)|+ => $cons:ident),+ $(,)?) => {
        $($(
            impl PrimLiteral for $prim {
                fn make_llval<'ctx>(self, ctx: &Context<'ctx>) -> BasicValueEnum<'ctx> {
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
                fn make_llval<'ctx>(self, ctx: &Context<'ctx>) -> BasicValueEnum<'ctx> {
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
    pub fn result(self, ctx: &Context<'ctx>) -> BrandedValue<'ctx, V> {
        unsafe { BrandedValue::materialize(ctx, self.maybe_val.unwrap()) }
    }
}
