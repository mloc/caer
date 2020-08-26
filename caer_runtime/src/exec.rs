use crate::arg_pack::ArgPack;
use crate::runtime::Runtime;
use crate::vtable::ProcPtr;
use std::collections::VecDeque;
use std::ptr::NonNull;
use crate::val::Val;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct FibreId(usize);

#[derive(Debug)]
pub struct Executor {
    coro_ctx: CoroCtx,

    fibres: HashMap<FibreId, FibreRecord>,
    next_fibre_id: FibreId,
    run_queue: VecDeque<FibreId>,
}

impl Executor {
    pub fn setup() -> Self {
        Self {
            coro_ctx: CoroCtx::create(),
            fibres: HashMap::new(),
            next_fibre_id: FibreId(0),
            run_queue: VecDeque::new(),
        }
    }

    pub fn queue_proc(&mut self, proc: ProcPtr, args: ArgPack) {
        let fibre_id = self.next_fibre_id;
        self.next_fibre_id.0 += 1;

        let fibre = FibreRecord {
            id: fibre_id,
            state: FibreState::Created(proc, args),
        };

        self.fibres.insert(fibre_id, fibre);
        self.run_queue.push_back(fibre_id);
    }

    pub fn run_next(&mut self, rt: &mut Runtime) -> bool {
        if self.run_queue.is_empty() {
            return false
        }

        let next_id = self.run_queue.pop_front().unwrap();
        let next_fibre = self.fibres.get_mut(&next_id).unwrap();

        let coro = match &next_fibre.state {
            FibreState::Running(coro) => coro,
            FibreState::Created(proc, args) => {
                let coro = self.coro_ctx.spawn_coro(rt, *proc, args as *const ArgPack);
                next_fibre.state = FibreState::Running(coro);

                // TODO: is there a better way to do this?
                if let FibreState::Running(ref coro_ref) = next_fibre.state {
                    coro_ref
                } else {
                    unreachable!();
                }
            },
            FibreState::Finished => panic!("finished fibre in queue: {:?}", next_id),
        };

        let ended = unsafe {
            self.coro_ctx.resume_coro(coro)
        };

        if ended {
            next_fibre.state = FibreState::Finished;
        } else {
            self.run_queue.push_back(next_id);
        }

        true
    }
}

#[derive(Debug)]
pub struct FibreRecord {
    id: FibreId,
    state: FibreState,
}

#[derive(Debug)]
pub enum FibreState {
    Created(ProcPtr, ArgPack),
    Running(aco::Coro),
    Finished,
}

#[derive(Debug)]
struct CoroCtx {
    ctx: aco::Context,
    stack: aco::Stack,
}

impl CoroCtx {
    fn create() -> Self {
        let ctx = aco::Context::create();
        let stack = ctx.make_stack();
        Self {
            ctx, stack
        }
    }

    fn spawn_coro(&self, rt: &mut Runtime, proc: ProcPtr, args: *const ArgPack) -> aco::Coro {
        fn proc_entry(cargs: (ProcPtr, *const ArgPack, NonNull<Runtime>)) {
            let mut ret = Val::Null;
            cargs.0(cargs.1, cargs.2, (&mut ret) as *mut _);
        }

        aco::Coro::new(
            &self.ctx,
            &self.stack,
            proc_entry,
            (proc, args, NonNull::new(rt as *mut Runtime).unwrap()),
        )
    }

    unsafe fn resume_coro(&self, coro: &aco::Coro) -> bool {
        self.ctx.resume(coro)
    }
}
