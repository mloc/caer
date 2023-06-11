use crate::PinionData;

/// Spoofs around an inkwell/llvm context+module+builder, kinda.
pub trait Context {
    // Threaded through function calls, usually a ref.
    type Funcs: Funcs;
    type Repr;

    type ReprManager: ReprManager<Funcs = Self::Funcs, Repr = Self::Repr>;

    type BasicType;
    type FunctionType;

    fn get_funcs(&self) -> Self::Funcs;
    fn get_repr_manager(&self) -> Self::ReprManager;

    /*fn make_struct_type(
        &mut self, fields: &[Self::BasicType], packed: bool, name: &str,
    ) -> Self::BasicType;

    fn make_primitive_type(&mut self, prim: crate::types::Primitive) -> Self::BasicType;

    fn make_pointer_type(&mut self, ty: Self::BasicType, is_gc: bool) -> Self::BasicType;

    fn make_function_type(
        &mut self, param_tys: &[Self::BasicType], return_ty: Option<Self::BasicType>,
    ) -> Self::FunctionType;

    fn make_func_ptr_type(&mut self, func: Self::FunctionType) -> Self::BasicType;

    fn make_opaque_struct_type(&mut self, size: Option<u32>) -> Self::BasicType;*/
}

pub trait ReprManager {
    type Funcs: Funcs;
    type Repr;

    fn build_repr<T: PinionData>(&mut self, ctx: Self::Funcs) -> Self::Repr;
}

pub trait Funcs: Copy {}
