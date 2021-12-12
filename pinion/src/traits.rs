use inkwell::context::Context;
use inkwell::types as ity;

pub trait PinionType<'ctx> {
    type Out: ity::AnyType<'ctx>;
    fn instantiate(ctx: &'ctx Context) -> Self::Out;
    fn debug_stringify() -> String;
}

pub trait PinionBasicType<'ctx>: PinionType<'ctx, Out = Self::BasicOut> {
    type BasicOut: ity::BasicType<'ctx>;
}

// These mostly exist as marker traits
pub trait PinionIntType<'ctx>: PinionType<'ctx, Out = ity::IntType<'ctx>> {}
pub trait PinionFloatType<'ctx>: PinionType<'ctx, Out = ity::FloatType<'ctx>> {}
pub trait PinionStructType<'ctx>: PinionType<'ctx, Out = ity::StructType<'ctx>> {}
pub trait PinionFuncType<'ctx>: PinionType<'ctx, Out = ity::FunctionType<'ctx>> {}
