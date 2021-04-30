mod context;
mod coro;
mod stack;

use std::sync::atomic::{fence, Ordering};

pub use context::Context;
pub use coro::Coro;
pub use stack::Stack;

/// # Safety
/// Must be called from a non-main coroutine.
pub unsafe fn yield_to_main() {
    fence(Ordering::SeqCst);
    aco_sys::aco_yield();
    fence(Ordering::SeqCst);
}
