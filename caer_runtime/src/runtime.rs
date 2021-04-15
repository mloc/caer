use crate::alloc::Alloc;
use crate::datum::Datum;
use crate::exec;
use crate::gc_stackmap::GcStackmap;
use crate::list::List;
use crate::string_table::StringTable;
use crate::val::Val;
use crate::vtable;
use caer_types::{id::{FuncId, StringId, TypeId}, rt_env::RtEnv};
use caer_types::type_tree::Specialization;
use crate::arg_pack::CallBundle;
use crate::sync;

use std::fs::File;
use std::mem;
use std::ptr::NonNull;

#[derive(Debug)]
pub struct Runtime {
    pub(crate) string_table: StringTable,
    pub(crate) vtable: vtable::Vtable,
    pub(crate) env: RtEnv,
    pub(crate) gc_stackmap: GcStackmap,
    pub(crate) alloc: Alloc,
    // Updated by executor
    pub(crate) world_time: u64,

    // These are used to communicate back to the executor when yielding
    pub(crate) queued_funcs: Vec<(CallBundle, u64)>,
    pub(crate) sleep_duration: u64
}

// TODO: ERRH
/// # Safety
/// Should only be called by generated code that knows what it's doing; in particular, the
/// pointers need to be valid, and global_rt must point to an appropriately sized section of
/// memory.
#[no_mangle]
pub unsafe extern "C" fn rt_runtime_init(
    global_rt: &'static mut Runtime,
    stackmap_start: *const u8,
    stackmap_end: *const u8,
    vtable_ptr: *const vtable::Entry,
    funcs_ptr: *const vtable::FuncPtr,
    entry_proc: FuncId,
) {
    // TODO: env checksums in binary
    let init_st = StringTable::deserialize(File::open("stringtable.bincode").unwrap());
    let env: RtEnv = bincode::deserialize_from(File::open("environment.bincode").unwrap()).unwrap();
    let vtable = vtable::Vtable::from_static(vtable_ptr, env.type_tree.types.len(), funcs_ptr, env.func_specs.len());

    let stackmaps_raw = {
        let len = stackmap_end.offset_from(stackmap_start);
        if len < 0 {
            panic!("bad stackmap, len is {}", len);
        }
        std::slice::from_raw_parts(stackmap_start, len as usize)
    };

    let gc_stackmap = GcStackmap::parse(stackmaps_raw);

    let new = Runtime {
        string_table: init_st,
        vtable,
        env,
        gc_stackmap,
        alloc: Alloc::new(),
        world_time: 0,
        queued_funcs: Vec::new(),
        sleep_duration: 0,
    };

    // update the global runtime seen by user code
    mem::forget(mem::replace(global_rt, new));

    let mut meta = MetaRuntime {
        executor: exec::Executor::setup(),
        dmrt: global_rt,
        sync_server: sync::SyncServer::setup(),
    };
    meta.start(entry_proc);
}

impl Runtime {
    // TODO: fix lifetimes
    pub fn new_datum(&mut self, ty: TypeId) -> &mut Datum {
        unsafe { &mut *self.rt_runtime_alloc_datum(ty.index() as u32).as_ptr() }
    }

    // TODO: genericify? + break out of Runtime
    pub fn alloc_list(&mut self) -> NonNull<List> {
        let size = mem::size_of::<List>();
        self.alloc.alloc(size).cast()
    }
}

impl Runtime {
    #[no_mangle]
    pub extern "C" fn rt_runtime_concat_strings(
        &mut self,
        lhs: StringId,
        rhs: StringId,
    ) -> StringId {
        self.string_table.concat(lhs, rhs)
    }

    #[no_mangle]
    pub extern "C" fn rt_runtime_suspend(&mut self, sleep_duration: u64) {
        self.sleep_duration = sleep_duration;

        // Is this necessary?
        // To be on the safe side, we end the ref on self.
        #[allow(clippy::drop_ref)]
        std::mem::drop(self);

        unsafe {
            aco::yield_to_main()
        }
    }

    #[no_mangle]
    pub extern "C" fn rt_runtime_alloc_datum(&mut self, ty: u32) -> NonNull<Datum> {
        let ty = TypeId::new(ty as usize);
        let ventry = &self.vtable[ty];
        // TODO: put spec in ventry?
        let spec = self.env.type_tree.types[ty].specialization;

        match spec {
            Specialization::Datum => {
                unsafe {
                    let mut ptr: NonNull<Datum> = self.alloc.alloc(ventry.size as usize).cast();
                    ptr.as_mut().ty = ty;

                    // TODO: init vars instead of nulling
                    for val in ptr.as_mut().get_vars(self) {
                        *val = crate::val::Val::Null;
                    }

                    ptr
                }
            }
            Specialization::List => {
                let mut ptr = self.alloc_list();
                unsafe {
                    *ptr.as_mut() = List::new(ty);
                }
                ptr.cast()
            }
        }
    }

    #[no_mangle]
    pub extern "C" fn rt_runtime_spawn_closure(
        &mut self,
        closure_func: FuncId,
        num_args: u64,
        args: NonNull<Val>,
    ) {
        // TODO: handle delayed spawns
        println!("doing spawn: {:?} / {:?} / {:?}", closure_func, num_args, args);
        //let spec = &self.env.func_specs[closure_func];
        let env = unsafe { std::slice::from_raw_parts(args.as_ptr(), num_args as usize).to_vec() };

        let args = crate::arg_pack::ClosureArgs {
            environment: env,
        };
        let bundle = crate::arg_pack::CallBundle::Closure((closure_func, args));

        // DM fibres can't directly interact with the executor, so we queue spawns in the runtime
        // to be picked up at next sleep
        self.queued_funcs.push((bundle, 0));
    }

    #[no_mangle]
    pub extern "C" fn rt_runtime_get_time(&mut self) -> u64 {
        self.world_time
    }
}

// what a name
// stuff that only the main fibre needs to worry about
// dark and ephemeral
// the lie that keeps the world running
// TODO: move
pub struct MetaRuntime {
    executor: exec::Executor,
    dmrt: &'static mut Runtime,
    sync_server: sync::SyncServer,
}

impl MetaRuntime {
    fn start(&mut self, entry_proc: FuncId) {
        println!("runtime starting");
        self.executor
            .queue_func(crate::arg_pack::CallBundle::Proc((entry_proc, crate::arg_pack::ProcArgs::empty())), 0);
        loop {
            println!("running next proc");
            if !self.executor.run_next(self.dmrt) {
                println!("no procs left in queue, running final GC and finishing");
                crate::gc::run(self.dmrt);
                break;
            }
            for msg in self.sync_server.iter_messages() {
                println!("msg: {:?}", msg);
            }
        }
        println!("runtime finished");
    }
}
