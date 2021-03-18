use crate::{arg_pack::{CallBundle, ClosureArgs, ProcArgs}, vtable::ClosurePtr};
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

    pub fn queue_func(&mut self, bundle: CallBundle) {
        let fibre_id = self.next_fibre_id;
        self.next_fibre_id.0 += 1;

        let fibre = FibreRecord {
            id: fibre_id,
            state: FibreState::Created(bundle),
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
            FibreState::Created(bundle) => {
                let coro = self.coro_ctx.spawn_coro(rt, bundle);
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
    Created(CallBundle),
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

    fn spawn_coro(&self, rt: &mut Runtime, bundle: &CallBundle) -> aco::Coro {
        fn proc_entry(cargs: (ProcPtr, ProcArgs, NonNull<Runtime>)) {
            let mut ret = Val::Null;
            let pack = (&cargs.1.as_pack()) as *const _;
            cargs.0(pack, cargs.2, (&mut ret) as *mut _);
        }
        fn closure_entry(cargs: (ClosurePtr, ClosureArgs, NonNull<Runtime>)) {
            let mut ret = Val::Null;
            let env = cargs.1.environment.as_ptr();
            cargs.0(env, cargs.2, (&mut ret) as *mut _);
        }

        match bundle {
            CallBundle::Proc((func, args)) => {
                let ptr = unsafe { rt.vtable.lookup_func(*func).unwrap().as_proc() };
                aco::Coro::new(
                    &self.ctx,
                    &self.stack,
                    proc_entry,
                    (ptr, args.clone(), NonNull::new(rt as *mut Runtime).unwrap()),
                )
            },
            CallBundle::Closure((func, args)) => {
                let ptr = unsafe { rt.vtable.lookup_func(*func).unwrap().as_closure() };
                aco::Coro::new(
                    &self.ctx,
                    &self.stack,
                    closure_entry,
                    (ptr, args.clone(), NonNull::new(rt as *mut Runtime).unwrap()),
                )
            },
        }
    }

    unsafe fn resume_coro(&self, coro: &aco::Coro) -> bool {
        self.ctx.resume(coro)
    }
}
