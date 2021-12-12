use std::marker::PhantomData;

use inkwell::context::Context;
use inkwell::types as ity;

use crate::traits::*;

pub struct Ptr<T, const ADDRSPACE: u32> {
    phantom: PhantomData<T>,
}

impl<'ctx, T: BasicType<'ctx>, const ADDRSPACE: u32> Type<'ctx> for Ptr<T, ADDRSPACE> {
    type Out = inkwell::types::PointerType<'ctx>;

    fn instantiate(ctx: &'ctx Context) -> Self::Out {
        let ty = T::instantiate(ctx);
        <T::BasicOut as ity::BasicType>::ptr_type(&ty, map_addrspace(ADDRSPACE))
    }

    fn debug_stringify() -> String {
        format!("Ptr<{}, a{}>", T::debug_stringify(), ADDRSPACE)
    }
}

impl<'ctx, T: BasicType<'ctx>, const ADDRSPACE: u32> BasicType<'ctx> for Ptr<T, ADDRSPACE> {
    type BasicOut = Self::Out;
}

const fn map_addrspace(addrspace: u32) -> inkwell::AddressSpace {
    match addrspace {
        0 => inkwell::AddressSpace::Generic,
        _ => panic!("unknown addressspace id"),
    }
}
