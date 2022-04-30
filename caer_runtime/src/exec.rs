use std::collections::HashMap;
use std::ptr::NonNull;

use index_vec::define_index_type;

use crate::arg_pack::{CallBundle, ClosureArgs, ProcArgs};
use crate::meta_runtime;
use crate::runtime::Runtime;
use crate::val::Val;
use crate::vtable::{ClosurePtr, ProcPtr};

const STACK_SIZE: u64 = 8 * 1024 * 1024; // 8M

define_index_type! {pub struct FibreId = u32;}

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
            next_fibre_id: FibreId::new(0),
            run_queue: Queue::new(),
        }
    }

    pub fn queue_func(&mut self, bundle: CallBundle, at_time: TickTime) {
        let fibre_id = self.next_fibre_id;
        self.next_fibre_id += 1;

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
        let ended = self.run(rt, next_id);

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

    pub fn wake_all_once(&mut self, rt: &mut Runtime) {
        // TODO: wrangle this
        for rid in 0..self.next_fibre_id.raw() {
            let id = FibreId::new(rid as _);
            if matches!(self.fibres[&id].state, FibreState::Finished) {
                continue;
            }
            self.run(rt, id);
        }
    }

    pub fn run(&mut self, rt: &mut Runtime, id: FibreId) -> bool {
        let rec = self.fibres.get_mut(&id).unwrap();

        match &rec.state {
            FibreState::Running => {
                println!("[EXEC] Resuming {:?}", id);
            },
            FibreState::Created(bundle) => {
                println!("[EXEC] Starting {:?} with {:?}", id, bundle);
                self.coro_ctx.spawn_coro(id, rt, bundle);
                rec.state = FibreState::Running;
            },
            FibreState::Finished => panic!("tried to resume finished fibre: {:?}", id),
        };

        let ended = self.coro_ctx.resume_coro(id);

        if ended {
            println!("[EXEC] Coro {:?} finished", id);
            rec.state = FibreState::Finished;
            self.coro_ctx.destroy_coro(id);
        }

        ended
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
    Running,
    Finished,
}

#[derive(Debug)]
struct Coro {
    id: FibreId,
    // This order matters for Drop
    handle: aco::Coro,
    stack: aco::Stack,
}

#[derive(Debug)]
struct CoroCtx {
    ctx: aco::Context,
    // TODO: maybe not a hashmap
    coros: HashMap<FibreId, Coro>,
}

impl CoroCtx {
    fn create() -> Self {
        let ctx = aco::Context::create();
        Self {
            ctx,
            coros: HashMap::new(),
        }
    }

    // TODO(ERRH)
    fn spawn_coro(&mut self, id: FibreId, rt: &mut Runtime, bundle: &CallBundle) {
        assert!(!self.coros.contains_key(&id));
        fn proc_entry(mut cargs: (ProcPtr, ProcArgs, NonNull<Runtime>)) {
            unsafe { cargs.2.as_mut().on_wake() };
            let mut ret = Val::Null;
            let pack = (&cargs.1.as_pack()) as *const _;
            cargs.0 .0(pack, unsafe { cargs.2.as_mut() }, (&mut ret) as *mut _);
        }
        fn closure_entry(mut cargs: (ClosurePtr, ClosureArgs, NonNull<Runtime>)) {
            unsafe { cargs.2.as_mut().on_wake() };
            let mut ret = Val::Null;
            let env = cargs.1.environment.as_ptr();
            cargs.0(env, unsafe { cargs.2.as_mut() }, (&mut ret) as *mut _);
        }

        let stack = self.ctx.create_stack(Some(STACK_SIZE));
        let handle = match bundle {
            CallBundle::Proc((func, args)) => {
                let ptr = unsafe { rt.vtable.lookup_func(*func).unwrap().as_proc() };
                aco::Coro::new(
                    &self.ctx,
                    &stack,
                    proc_entry,
                    (ptr, args.clone(), NonNull::new(rt as *mut Runtime).unwrap()),
                )
            },
            CallBundle::Closure((func, args)) => {
                let ptr = unsafe { rt.vtable.lookup_func(*func).unwrap().as_closure() };
                aco::Coro::new(
                    &self.ctx,
                    &stack,
                    closure_entry,
                    (ptr, args.clone(), NonNull::new(rt as *mut Runtime).unwrap()),
                )
            },
        };

        let coro = Coro { id, handle, stack };

        self.coros.insert(id, coro);
    }

    // TODO(ERRH)
    fn resume_coro(&self, id: FibreId) -> bool {
        let handle = &self.coros[&id].handle;
        unsafe { self.ctx.resume(handle) }
    }

    // TODO(ERRH)
    fn destroy_coro(&mut self, id: FibreId) {
        self.coros.remove(&id).expect("no such coro");
    }
}

// The current implementation of this is pretty bad, but it needs to be replaced with something
// smarter anyway.
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
