use crate::alloc::Alloc;
use crate::datum::Datum;
use crate::environment::Environment;
use crate::exec;
use crate::gc_stackmap::GcStackmap;
use crate::list::List;
use crate::string_table::StringTable;
use crate::val::Val;
use crate::vtable::{self, ProcPtr};
use caer_types::{id::{FuncId, StringId, TypeId}, rt_env::RtEnv};
use caer_types::type_tree::Specialization;
use crate::arg_pack::CallBundle;

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
    // TODO: delay/target time
    pub(crate) queued_funcs: Vec<CallBundle>
}

// TODO: ERRH
/// # Safety
/// Should only be called by generated code that knows what it's doing; in particular, the
/// stackmap pointers need to be safe.
#[no_mangle]
pub unsafe extern "C" fn rt_runtime_init(
    global_rt: &mut Runtime,
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
        queued_funcs: Vec::new(),
    };

    // update the global runtime seen by user code
    mem::forget(mem::replace(global_rt, new));

    let mut meta = MetaRuntime {
        executor: exec::Executor::setup(),
        dmrt: global_rt,
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
    pub extern "C" fn rt_runtime_suspend(&mut self) {
        crate::gc::run(self);
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
        println!("doing spawn: {:?} / {:?} / {:?}", closure_func, num_args, args);
        //let spec = &self.env.func_specs[closure_func];
        let env = unsafe { std::slice::from_raw_parts(args.as_ptr(), num_args as usize).to_vec() };

        let args = crate::arg_pack::ClosureArgs {
            environment: env,
        };
        let bundle = crate::arg_pack::CallBundle::Closure((closure_func, args));

        // DM fibres can't directly interact with the executor, so we queue spawns in the runtime
        // to be picked up at next sleep
        self.queued_funcs.push(bundle);
    }
}

// what a name
// stuff that only the main fibre needs to worry about
// dark and ephemeral
// the lie that keeps the world running
// TODO: move
#[derive(Debug)]
pub struct MetaRuntime<'rt> {
    executor: exec::Executor,
    // this lifetime is bad
    // deal with it
    dmrt: &'rt mut Runtime,
}

impl MetaRuntime<'_> {
    fn start(&mut self, entry_proc: FuncId) {
        println!("runtime starting");
        self.executor
            .queue_func(crate::arg_pack::CallBundle::Proc((entry_proc, crate::arg_pack::ProcArgs::empty())));
        loop {
            println!("running next proc");
            if !self.executor.run_next(self.dmrt) {
                println!("no procs left in queue, running final GC and finishing");
                crate::gc::run(self.dmrt);
                break;
            }

            let queue: Vec<_> = self.dmrt.queued_funcs.drain(..).collect();
            for b in queue {
                self.executor.queue_func(b);
            }
        }
        println!("runtime finished");
    }
}
