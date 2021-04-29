use crate::coro::Coro;
use crate::stack::Stack;

#[derive(Debug)]
pub struct Context {
    pub(crate) main_coro: *mut aco_sys::aco_t,
}

impl Context {
    pub fn create() -> Self {
        unsafe { aco_sys::aco_thread_init(None) };
        let main_coro = unsafe {
            aco_sys::aco_create(
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                0,
                None,
                std::ptr::null_mut(),
            )
        };

        Context { main_coro }
    }

    pub fn make_stack(&self) -> Stack {
        let handle = unsafe { aco_sys::aco_share_stack_new(0) };

        Stack { handle }
    }

    /// # Safety
    /// `coro` must belong to this context.
    pub unsafe fn resume(&self, coro: &Coro) -> bool {
        assert!(
            (*coro.handle).main_co == self.main_coro,
            "attempted to resume coroutine in wrong context"
        );
        assert!(
            (*coro.handle).is_end == 0,
            "attemped to resume finished coroutine"
        );
        aco_sys::aco_resume(coro.handle);

        coro.is_end()
    }
}
