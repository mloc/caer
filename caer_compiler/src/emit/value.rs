use crate::ty::{self, Ty};

pub struct Value<'ctx> {
    pub val: Option<inkwell::values::BasicValueEnum<'ctx>>,
    pub ty: ty::Complex,
}

impl<'ctx> Value<'ctx> {
    pub fn new(val: Option<inkwell::values::BasicValueEnum<'ctx>>, ty: ty::Complex) -> Self {
        if val == None && ty != ty::Primitive::Null.into() {
            panic!("values with non-null ty must have a val")
        }
        Self {
            val: val,
            ty: ty,
        }
    }
}
