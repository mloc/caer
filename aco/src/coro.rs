use crate::context::Context;
use crate::stack::Stack;
use aco_sys;
use std::marker::PhantomData;

pub struct Coro<'a, A> {
    pub(crate) handle: *mut aco_sys::aco_t,
    pub stack: &'a Stack,

    arg_type: PhantomData<A>,
}

impl<'a, A> Coro<'a, A> {
    pub fn new<F>(ctx: &'a Context, stack: &'a Stack, entry: F, arg: A) -> Coro<'a, A>
    where
        F: FnOnce(A),
    {
        let payload = Box::into_raw(Box::new((entry, arg))) as *mut std::ffi::c_void;
        let bootstrap = bootstrap_coro::<A, F>;

        let handle = unsafe {
            aco_sys::aco_create(ctx.main_coro, stack.handle, 0, Some(bootstrap), payload)
        };

        Coro {
            handle,
            stack,
            arg_type: PhantomData,
        }
    }
}

impl<'a, A> Drop for Coro<'a, A> {
    fn drop(&mut self) {
        unsafe { aco_sys::aco_destroy(self.handle) };
    }
}

extern "C" fn bootstrap_coro<A, F: FnOnce(A)>() {
    unsafe {
        let (entry, arg) = *Box::from_raw(aco_sys::aco_get_arg() as *mut (F, A));
        entry(arg);
        aco_sys::aco_exit();
    }
}
