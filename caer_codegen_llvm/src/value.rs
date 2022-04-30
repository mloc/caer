use caer_types::id::TypeId;
use inkwell::values::{BasicValue, BasicValueEnum, GlobalValue, IntValue, PointerValue};

#[derive(Debug, Clone)]
pub struct StackValue<'ctx> {
    pub val: PointerValue<'ctx>,
    pub ty: TypeId,
}

impl<'ctx> StackValue<'ctx> {
    pub fn new(val: PointerValue<'ctx>, ty: TypeId) -> Self {
        Self { val, ty }
    }
}

#[derive(Debug, Clone)]
pub struct LocalValue<'ctx> {
    pub val: BasicValueEnum<'ctx>,
    pub ty: TypeId,
}

impl<'ctx> LocalValue<'ctx> {
    pub fn new(val: BasicValueEnum<'ctx>, ty: TypeId) -> Self {
        Self { val, ty }
    }
}

pub(crate) struct BveWrapper<'ctx> {
    pub bve: BasicValueEnum<'ctx>,
}

impl<'ctx> From<&LocalValue<'ctx>> for BveWrapper<'ctx> {
    fn from(val: &LocalValue<'ctx>) -> BveWrapper<'ctx> {
        val.val.into()
    }
}

impl<'ctx> From<LocalValue<'ctx>> for BveWrapper<'ctx> {
    fn from(val: LocalValue<'ctx>) -> BveWrapper<'ctx> {
        val.val.into()
    }
}

impl<'ctx> From<BasicValueEnum<'ctx>> for BveWrapper<'ctx> {
    fn from(bve: BasicValueEnum<'ctx>) -> BveWrapper<'ctx> {
        BveWrapper { bve }
    }
}

impl<'ctx> From<PointerValue<'ctx>> for BveWrapper<'ctx> {
    fn from(val: PointerValue<'ctx>) -> BveWrapper<'ctx> {
        val.as_basic_value_enum().into()
    }
}

impl<'ctx> From<GlobalValue<'ctx>> for BveWrapper<'ctx> {
    fn from(val: GlobalValue<'ctx>) -> BveWrapper<'ctx> {
        val.as_basic_value_enum().into()
    }
}

impl<'ctx> From<IntValue<'ctx>> for BveWrapper<'ctx> {
    fn from(val: IntValue<'ctx>) -> BveWrapper<'ctx> {
        val.as_basic_value_enum().into()
    }
}
