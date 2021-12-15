use crate::interface::Context;

pub trait PinionStruct {
    fn create_in_context<C: Context>(ctx: &mut C) -> C::BasicType;
}
