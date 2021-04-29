use crate::context::Context;
use crate::stack::Stack;

#[derive(Debug)]
pub struct Coro {
    pub(crate) handle: *mut aco_sys::aco_t,
}

impl Coro {
    pub fn new<A, F>(ctx: &Context, stack: &Stack, entry: F, arg: A) -> Coro
    where
        F: FnOnce(A),
    {
        let payload = Box::into_raw(Box::new((entry, arg))) as *mut std::ffi::c_void;
        let bootstrap = bootstrap_coro::<A, F>;

        let handle = unsafe {
            aco_sys::aco_create(ctx.main_coro, stack.handle, 0, Some(bootstrap), payload)
        };

        Coro { handle }
    }

    pub fn is_end(&self) -> bool {
        unsafe { (*self.handle).is_end != 0 }
    }
}

impl Drop for Coro {
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
