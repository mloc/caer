use std::marker::PhantomData;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::types as ity;
use inkwell::values::PointerValue;

use crate::traits::*;

pub struct Ptr<T, const ADDRSPACE: u32> {
    phantom: PhantomData<T>,
}

impl<'ctx, T: PinionBasicType<'ctx>, const ADDRSPACE: u32> Ptr<T, ADDRSPACE> {
    pub fn build_gep<F>(
        ctx: &'ctx Context, builder: Builder<'ctx>, ptr: PointerValue<'ctx>, index_closure: F,
    ) -> PointerValue<'ctx>
    where
        F: FnOnce(Box<dyn PinionIndexableType>) -> Vec<u64>,
    {
        let base = T::create_empty();
        let path = index_closure(base);
        let indices: Vec<_> = path
            .into_iter()
            .map(|i| ctx.i32_type().const_int(i, false))
            .collect();
        unsafe { builder.build_in_bounds_gep(ptr, &indices, "") }
    }
}

impl<'ctx, T: PinionBasicType<'ctx>, const ADDRSPACE: u32> PinionType<'ctx> for Ptr<T, ADDRSPACE> {
    type Out = inkwell::types::PointerType<'ctx>;

    fn instantiate(ctx: &'ctx Context) -> Self::Out {
        let ty = T::instantiate(ctx);
        <T::BasicOut as ity::BasicType>::ptr_type(&ty, map_addrspace(ADDRSPACE))
    }

    fn debug_stringify() -> String {
        format!("Ptr<{}, a{}>", T::debug_stringify(), ADDRSPACE)
    }
}

impl<'ctx, T: 'static + PinionBasicType<'ctx>, const ADDRSPACE: u32> PinionBasicType<'ctx>
    for Ptr<T, ADDRSPACE>
{
    type BasicOut = Self::Out;

    fn create_empty() -> Box<dyn PinionIndexableType<'ctx>> {
        Box::new(Self {
            phantom: PhantomData,
        })
    }
}

impl<'ctx, T: 'static + PinionBasicType<'ctx>, const ADDRSPACE: u32> PinionIndexableType<'ctx>
    for Ptr<T, ADDRSPACE>
{
    /*fn resolve_index(
        &self, field: &'static str,
    ) -> Option<(u64, Box<dyn PinionIndexableType<'ctx>>)> {
        if field == "*" {
            Some((0, T::create_empty()))
        } else {
            None
        }
    }*/
}

const fn map_addrspace(addrspace: u32) -> inkwell::AddressSpace {
    match addrspace {
        0 => inkwell::AddressSpace::Generic,
        _ => panic!("unknown addressspace id"),
    }
}
