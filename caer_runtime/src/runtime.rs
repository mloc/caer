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
}

// what a name
// stuff that only the main fibre needs to worry about
// dark and ephemeral
// runs the show
#[derive(Debug)]
pub struct MetaRuntime<'rt> {
    executor: exec::Executor,
    // this lifetime is bad
    // deal with it
    dmrt: &'rt mut Runtime,
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
    entry_proc: ProcPtr,
) {
    let init_st = StringTable::deserialize(File::open("stringtable.bincode").unwrap());
    let env: RtEnv = bincode::deserialize_from(File::open("environment.bincode").unwrap()).unwrap();
    let vtable = vtable::Vtable::from_static(vtable_ptr, env.type_tree.types.len());

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
    };

    // update the global runtime seen by user code
    mem::forget(mem::replace(global_rt, new));

    let mut meta = MetaRuntime {
        executor: exec::Executor::setup(),
        dmrt: global_rt,
    };
    meta.start(entry_proc);
}

impl MetaRuntime<'_> {
    fn start(&mut self, entry_proc: ProcPtr) {
        println!("runtime starting");
        self.executor
            .queue_proc(entry_proc, crate::arg_pack::ArgPack::empty());
        loop {
            println!("running next proc");
            if !self.executor.run_next(self.dmrt) {
                println!("no procs left in queue, finishing");
                break;
            }
        }
        println!("runtime finished");
    }
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
        let spec = &self.env.func_specs[closure_func];
    }
}
