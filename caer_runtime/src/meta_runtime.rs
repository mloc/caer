use std::time::{self, Duration, Instant};

use aed_common::messages;
use aed_server::server::ClientId;
use caer_types::id::FuncId;
use exec::TickTime;

use crate::{exec, gc, runtime, sync};

pub const TICK_LAG: time::Duration = time::Duration::from_millis(1000);

// TODO: rename!
// what a name
// stuff that only the main fibre needs to worry about
// dark and ephemeral
// the lie that keeps the world running
pub struct MetaRuntime {
    executor: exec::Executor,
    pub dmrt: &'static mut runtime::Runtime,
    sync_server: sync::SyncServer,

    // Scheduling stuff
    // TODO: move into scheduler
    tick_base: TickTime,
    tick_base_time: Instant,
}

impl MetaRuntime {
    pub fn new(dmrt: &'static mut runtime::Runtime, sync_server: sync::SyncServer) -> Self {
        Self {
            executor: exec::Executor::setup(),
            dmrt,
            sync_server,

            tick_base: TickTime(0),
            tick_base_time: Instant::now(),
        }
    }

    pub fn start(&mut self, entry_proc: FuncId) {
        println!("runtime starting");
        self.executor.queue_func(
            crate::arg_pack::CallBundle::Proc((entry_proc, crate::arg_pack::ProcArgs::empty())),
            TickTime(0),
        );
        self.reset_tick_base();
        self.run_forever();
        println!("runtime finished");
    }

    fn run_forever(&mut self) {
        loop {
            let deadline = self.cur_tick_deadline();
            println!(
                "running tick {:?} with deadline {:?}",
                self.dmrt.world_time, deadline
            );
            self.run_tick();

            while let Some(underrun) = deadline.checked_duration_since(time::Instant::now()) {
                self.handle_messages(Some(underrun));
            }

            let overrun = time::Instant::now().saturating_duration_since(deadline);
            println!("overran tick by {:?}", overrun);
            if overrun > TICK_LAG / 2 {
                println!(
                    "overrun by more than TICK_LAG/2 ({:?}), resetting tick base",
                    TICK_LAG / 2
                );
                self.reset_tick_base();
            }

            self.dmrt.world_time += 1;

            if self.executor.queue_len() == 0 {
                break;
            }
        }
    }

    fn reset_tick_base(&mut self) {
        self.tick_base = TickTime(self.dmrt.world_time);
        self.tick_base_time = Instant::now();
    }

    fn cur_tick_deadline(&mut self) -> Instant {
        self.tick_base_time + (TICK_LAG * (self.dmrt.world_time - self.tick_base.0) as _)
    }

    fn run_tick(&mut self) {
        self.handle_messages(None);
        loop {
            println!("running next proc");
            if !self.run_one() {
                println!("no procs left to run, GCing");
                crate::gc::run(self);
                break;
            }
            crate::gc::run(self);
            self.handle_messages(None);
        }
    }

    fn run_one(&mut self) -> bool {
        if !self.executor.run_next(self.dmrt) {
            return false;
        }

        for (b, t) in self.dmrt.queued_funcs.drain(..) {
            self.executor.queue_func(b, TickTime(t));
        }

        true
    }

    fn handle_messages(&mut self, timeout: Option<Duration>) {
        let iter = match timeout {
            Some(t) => self.sync_server.iter_messages_timeout(t),
            None => self.sync_server.iter_messages_nonblocking(),
        };
        // TODO: remove collect
        for (id, msg) in iter.collect::<Vec<_>>() {
            self.handle_message(id, msg);
        }
    }

    fn handle_message(&mut self, id: ClientId, msg: messages::Client) {
        println!("{:?}: msg: {:?}", id, msg);
    }

    // TODO: take state by ref when rtcontext does
    pub fn gc_mark_fibres(&mut self, state: gc::State) -> gc::State {
        assert!(self.dmrt.scan_state.is_none());
        self.dmrt.scan_state = Some(state);
        self.executor.wake_all_once(self.dmrt);
        self.dmrt.scan_state.take().unwrap()
    }
}
