use std::collections::HashMap;
use std::ptr::NonNull;

use crate::arg_pack::{CallBundle, ClosureArgs, ProcArgs};
use crate::meta_runtime;
use crate::runtime::Runtime;
use crate::val::Val;
use crate::vtable::{ClosurePtr, ProcPtr};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct FibreId(usize);

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, PartialOrd, Ord)]
pub struct TickTime(pub u64);

#[derive(Debug)]
pub struct Executor {
    coro_ctx: CoroCtx,

    fibres: HashMap<FibreId, FibreRecord>,
    next_fibre_id: FibreId,
    run_queue: Queue,
}

impl Executor {
    pub fn setup() -> Self {
        Self {
            coro_ctx: CoroCtx::create(),
            fibres: HashMap::new(),
            next_fibre_id: FibreId(0),
            run_queue: Queue::new(),
        }
    }

    pub fn queue_func(&mut self, bundle: CallBundle, at_time: TickTime) {
        let fibre_id = self.next_fibre_id;
        self.next_fibre_id.0 += 1;

        let fibre = FibreRecord {
            id: fibre_id,
            state: FibreState::Created(bundle),
        };

        self.fibres.insert(fibre_id, fibre);
        self.run_queue.enqueue(fibre_id, at_time);
    }

    pub fn run_next(&mut self, rt: &mut Runtime) -> bool {
        println!("[EXEC] queue: {:?}", self.run_queue);
        match self.run_queue.earliest_wake() {
            // Nothing in runqueue
            None => return false,
            // Earliest in runqueue is in the future
            Some(t) if t > TickTime(rt.world_time) => return false,
            _ => {},
        }

        let (next_id, _) = self.run_queue.pop().unwrap();
        let next_fibre = self.fibres.get_mut(&next_id).unwrap();

        let coro = match &next_fibre.state {
            FibreState::Running(coro) => {
                println!("[EXEC] Resuming {:?}", next_id);
                coro
            },
            FibreState::Created(bundle) => {
                println!("[EXEC] Starting {:?} with {:?}", next_id, bundle);
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

        let ended = unsafe { self.coro_ctx.resume_coro(coro) };

        if ended {
            println!("[EXEC] Coro {:?} finished", next_id);
            next_fibre.state = FibreState::Finished;
        }

        if !ended {
            println!("[EXEC] Coro {:?} suspended", next_id);
            // TODO: what's the SoT for "world time"? - currently, Runtime
            // TODO: better handling, especially wrt floats
            let duration = if rt.sleep_duration < 0f32.into() {
                // TODO: "priority" queueing
                0
            } else if rt.sleep_duration > 0f32.into() {
                // TODO: integrate with tick_lag
                (rt.sleep_duration.into_inner() / 10f32 / meta_runtime::TICK_LAG.as_secs_f32())
                    .ceil() as u64
            } else {
                // TODO: "non priority" queueing
                0
            };
            println!("[EXEC] Sleeping for {} ticks", duration);
            let sleep_until = rt.world_time + duration;
            self.run_queue.enqueue(next_id, TickTime(sleep_until));
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
        Self { ctx, stack }
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

#[derive(Debug)]
struct Queue {
    queue: Vec<(FibreId, TickTime)>,
}

impl Queue {
    pub fn new() -> Self {
        Self {
            queue: Default::default(),
        }
    }

    fn ensure_sorted(&mut self) {
        // Stable sorting is desirable.
        self.queue.reverse();
        self.queue.sort_by_key(|(_, t): &(_, TickTime)| *t);
        self.queue.reverse();
    }

    pub fn enqueue(&mut self, fibre: FibreId, wake_time: TickTime) {
        self.queue.reverse();
        self.queue.push((fibre, wake_time));
        self.queue.reverse();
    }

    pub fn earliest_wake(&mut self) -> Option<TickTime> {
        self.ensure_sorted();
        self.queue.last().map(|(_, t)| *t)
    }

    pub fn pop(&mut self) -> Option<(FibreId, TickTime)> {
        self.ensure_sorted();
        self.queue.pop()
    }

    pub fn is_empty(&self) -> bool {
        self.queue.is_empty()
    }
}
