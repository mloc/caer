use aco_sys;

pub struct Stack {
    pub(crate) handle: *mut aco_sys::aco_share_stack_t,
}

impl Stack {
    pub fn new() -> Stack {
        let handle = unsafe { aco_sys::aco_share_stack_new(0) };

        Stack {
            handle,
        }
    }
}

impl Drop for Stack {
    fn drop(&mut self) {
        unsafe { aco_sys::aco_share_stack_destroy(self.handle) };
    }
}
