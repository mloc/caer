use inkwell::context::Context;
use inkwell::types as ity;

pub trait Type<'ctx> {
    type Out: ity::AnyType<'ctx>;
    fn instantiate(ctx: &'ctx Context) -> Self::Out;
    fn debug_stringify() -> String;
}

pub trait BasicType<'ctx>: Type<'ctx, Out = Self::BasicOut> {
    type BasicOut: ity::BasicType<'ctx>;
}

pub trait IntType<'ctx>: Type<'ctx, Out = inkwell::types::IntType<'ctx>> {}
pub trait FloatType<'ctx>: Type<'ctx, Out = inkwell::types::FloatType<'ctx>> {}
pub trait StructType<'ctx>: Type<'ctx, Out = inkwell::types::StructType<'ctx>> {}
