mod context;
mod coro;
mod stack;

pub use context::Context;
pub use coro::Coro;
pub use stack::Stack;

/// # Safety
/// Must be called from a non-main coroutine.
pub unsafe fn yield_to_main() {
    aco_sys::aco_yield()
}
