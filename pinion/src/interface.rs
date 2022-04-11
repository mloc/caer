/// Spoofs around an inkwell/llvm context+module+builder, kinda.
pub trait Context {
    type BasicType;
    type FunctionType;

    fn make_struct_type(
        &mut self, fields: &[Self::BasicType], packed: bool, name: &str,
    ) -> Self::BasicType;

    fn make_primitive_type(&mut self, prim: crate::types::Primitive) -> Self::BasicType;

    fn make_pointer_type(&mut self, ty: Self::BasicType, is_gc: bool) -> Self::BasicType;

    fn make_function_type(
        &mut self, param_tys: &[Self::BasicType], return_ty: Option<Self::BasicType>,
    ) -> Self::FunctionType;

    fn make_func_ptr_type(&mut self, func: Self::FunctionType) -> Self::BasicType;
}
