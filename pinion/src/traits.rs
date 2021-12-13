use inkwell::context::Context;
use inkwell::types as ity;

pub trait PinionType<'ctx> {
    type Out: ity::AnyType<'ctx>;
    fn instantiate(ctx: &'ctx Context) -> Self::Out;
    fn debug_stringify() -> String;
}

pub trait PinionBasicType<'ctx>:
    PinionType<'ctx, Out = Self::BasicOut> + PinionIndexableType<'ctx>
{
    type BasicOut: ity::BasicType<'ctx>;

    // Box<dyn ...> *feels* nasty, but it turns out that the alloc gets optimized away if the
    // concrete type is a ZST. neat!
    fn create_empty() -> Box<dyn PinionIndexableType<'ctx>>;
}

// Split out of BasicType to avoid that needing to be object-safe
pub trait PinionIndexableType<'ctx> {
    fn resolve_index(
        &self, _field: &'static str,
    ) -> Option<(u64, Box<dyn PinionIndexableType<'ctx>>)> {
        None
    }
}

// These mostly exist as marker traits
pub trait PinionIntType<'ctx>: PinionType<'ctx, Out = ity::IntType<'ctx>> {}
pub trait PinionFloatType<'ctx>: PinionType<'ctx, Out = ity::FloatType<'ctx>> {}
pub trait PinionStructType<'ctx>: PinionType<'ctx, Out = ity::StructType<'ctx>> {}
pub trait PinionFuncType<'ctx>: PinionType<'ctx, Out = ity::FunctionType<'ctx>> {}
