mod mark;
mod scan;
mod state;
mod sweep;

pub use scan::scan_stack;
pub use state::State;

use self::mark::Mark;
use self::sweep::Sweep;
use crate::meta_runtime::MetaRuntime;

pub fn run(meta: &mut MetaRuntime) {
    let state = State::new();
    let state = meta.gc_mark_fibres(state);

    let mut mark = Mark::new(meta.dmrt);
    mark.mark_all(&state);

    let mut sweep = Sweep::new(&mut meta.dmrt.alloc);
    sweep.sweep();
}
