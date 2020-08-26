#[derive(Debug)]
pub struct Stack {
    pub(crate) handle: *mut aco_sys::aco_share_stack_t,
}

impl Drop for Stack {
    fn drop(&mut self) {
        unsafe { aco_sys::aco_share_stack_destroy(self.handle) };
    }
}
