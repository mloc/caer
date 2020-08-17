use caer_types::ty;
use inkwell::values::{PointerValue, GlobalValue, BasicValue, BasicValueEnum, IntValue};

#[derive(Debug, Clone)]
pub struct StackValue<'ctx> {
    pub val: Option<PointerValue<'ctx>>,
    pub ty: ty::Complex,
}

impl<'ctx> StackValue<'ctx> {
    pub fn new(val: Option<PointerValue<'ctx>>, ty: ty::Complex) -> Self {
        if val == None && ty != ty::Primitive::Null.into() {
            panic!("values with non-null ty must have a val")
        }
        Self { val, ty }
    }
}

#[derive(Debug, Clone)]
pub struct SSAValue<'ctx> {
    pub val: BasicValueEnum<'ctx>,
    pub ty: ty::Complex,
}

impl<'ctx> SSAValue<'ctx> {
    pub fn new(val: BasicValueEnum<'ctx>, ty: ty::Complex) -> Self {
        Self { val, ty }
    }
}

pub(crate) struct BVEWrapper<'ctx> {
    pub bve: BasicValueEnum<'ctx>,
}

impl<'ctx> Into<BVEWrapper<'ctx>> for &SSAValue<'ctx> {
    fn into(self) -> BVEWrapper<'ctx> {
        self.val.into()
    }
}

impl<'ctx> Into<BVEWrapper<'ctx>> for SSAValue<'ctx> {
    fn into(self) -> BVEWrapper<'ctx> {
        (&self).into()
    }
}

impl<'ctx> Into<BVEWrapper<'ctx>> for BasicValueEnum<'ctx> {
    fn into(self) -> BVEWrapper<'ctx> {
        BVEWrapper { bve: self }
    }
}

impl<'ctx> Into<BVEWrapper<'ctx>> for PointerValue<'ctx> {
    fn into(self) -> BVEWrapper<'ctx> {
        self.as_basic_value_enum().into()
    }
}

impl<'ctx> Into<BVEWrapper<'ctx>> for GlobalValue<'ctx> {
    fn into(self) -> BVEWrapper<'ctx> {
        self.as_pointer_value().into()
    }
}

impl<'ctx> Into<BVEWrapper<'ctx>> for IntValue<'ctx> {
    fn into(self) -> BVEWrapper<'ctx> {
        self.as_basic_value_enum().into()
    }
}
