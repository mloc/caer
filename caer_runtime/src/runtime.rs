use std::fs::File;
use std::mem::{self, size_of};
use std::ptr::NonNull;

use aed_common::messages;
use caer_types::id::{
    FuncId, InstanceTypeId, PathTypeId, StringId, TypeId, INSTANCE_TYPE_ID_LIST, TYPE_ID_LIST,
};
use caer_types::rt_env::RtEnvBundle;
use caer_types::type_tree::Specialization;
use ordered_float::OrderedFloat;
use pinion::PinionData;

use crate::alloc::Alloc;
use crate::arg_pack::CallBundle;
use crate::datum::Datum;
use crate::environment::Environment;
use crate::gc_stackmap::GcStackmap;
use crate::list::List;
use crate::meta_runtime::MetaRuntime;
use crate::string::{resolve_string, RtString};
use crate::sync::SyncServer;
use crate::val::Val;
use crate::{gc, vtable};

#[derive(Debug)]
pub struct Runtime {
    pub(crate) vtable: vtable::Vtable,
    pub(crate) env: Environment,
    pub(crate) gc_stackmap: GcStackmap,
    pub(crate) alloc: Alloc,
    // Updated by executor
    // TODO: move to scheduler/timemaster(?)
    pub(crate) world_time: u64,

    // These are used to communicate back to the executor when yielding
    // TODO: encapsulate somewhere else
    pub(crate) queued_funcs: Vec<(CallBundle, u64)>,
    pub(crate) sleep_duration: OrderedFloat<f32>,

    // TODO: move into rtcontext
    pub(crate) scan_state: Option<gc::State>,

    // TEMPORARY
    // TODO: replace with better message plumbing
    sync_send: aed_server::server::Server,
}

impl PinionData for Runtime {
    type Static = Runtime;

    unsafe fn validate(ptr: *const u8) {
        // TODO: validate against some rust-side global?
    }

    fn get_layout(_lctx: &mut pinion::layout_ctx::LayoutCtx) -> pinion::layout::BasicType {
        pinion::layout::BasicType::OpaqueStruct(Some(size_of::<Runtime>() as _))
    }
}

// TODO: ERRH
/// # Safety
/// Should only be called by generated code that knows what it's doing; in particular, the
/// pointers need to be valid, and global_rt must point to an appropriately sized section of
/// memory.
#[no_mangle]
pub unsafe extern "C" fn rt_runtime_init(
    global_rt: &'static mut Runtime, stackmap_start: *const u8, stackmap_end: *const u8,
    vtable_ptr: *const vtable::Entry, funcs_ptr: *const vtable::FuncPtr, entry_proc: FuncId,
) {
    // TODO: improve. Can be removed if we use rust-init
    assert!(libc::signal(libc::SIGPIPE, libc::SIG_IGN) != libc::SIG_ERR);

    // TODO: env checksums in binary
    let bundle: RtEnvBundle =
        bincode::deserialize_from(File::open("environment.bincode").unwrap()).unwrap();
    let vtable = vtable::Vtable::from_static(
        vtable_ptr,
        bundle.type_tree.len(),
        funcs_ptr,
        bundle.func_specs.len(),
    );

    let stackmaps_raw = {
        let len = stackmap_end.offset_from(stackmap_start);
        if len < 0 {
            panic!("bad stackmap, len is {}", len);
        }
        std::slice::from_raw_parts(stackmap_start, len as usize)
    };

    let gc_stackmap = GcStackmap::parse(stackmaps_raw);

    let sync_server = SyncServer::setup();

    let new = Runtime {
        vtable,
        env: Environment::from_rt_env(bundle),
        gc_stackmap,
        alloc: Alloc::new(),
        world_time: 0,
        queued_funcs: Vec::new(),
        sleep_duration: 0f32.into(),
        scan_state: None,
        sync_send: sync_server.get_sync_send(),
    };

    // update the global runtime seen by user code
    mem::forget(mem::replace(global_rt, new));

    let mut meta = MetaRuntime::new(global_rt, sync_server);
    meta.start(entry_proc);
}

impl Runtime {
    // TODO: fix lifetimes
    #[deprecated]
    pub fn new_datum(&mut self, ty: PathTypeId) -> &mut Datum {
        unsafe { &mut *self.rt_runtime_alloc_datum(ty.index() as u32).as_ptr() }
    }

    // TODO: genericify? + break out of Runtime
    pub fn alloc_list(&mut self) -> NonNull<List> {
        let size = mem::size_of::<List>();
        self.alloc.alloc(size, INSTANCE_TYPE_ID_LIST).cast()
    }

    pub fn send_message(&self, msg: &messages::Server) {
        self.sync_send.send_all(msg)
    }

    // TODO: mvoe into rtcontext
    pub fn yield_to_meta(&mut self) {
        unsafe { aco::yield_to_main() };
        self.on_wake();
    }

    pub fn on_wake(&mut self) {
        loop {
            // If scan_state is populated, we've been woken up just to do a GC scan
            if let Some(ref mut gc_state) = self.scan_state {
                gc::scan_stack(gc_state, &self.gc_stackmap)
            } else {
                break;
            }

            unsafe { aco::yield_to_main() };
        }
    }
}

impl Runtime {
    #[no_mangle]
    pub extern "C" fn rt_runtime_concat_strings(
        &mut self, lhs: Option<NonNull<RtString>>, rhs: Option<NonNull<RtString>>,
    ) -> NonNull<RtString> {
        RtString::from_str(format!("{}{}", resolve_string(lhs), resolve_string(rhs)))
            .heapify(&self.alloc)
    }

    #[no_mangle]
    pub extern "C" fn rt_runtime_string_to_id(&mut self, string: NonNull<RtString>) -> StringId {
        self.env
            .string_table
            .lookup(resolve_string(Some(string)))
            .unwrap_or_else(|| StringId::from_raw(!0u64))
    }

    #[no_mangle]
    pub extern "C" fn rt_runtime_suspend(&mut self, sleep_duration: &Val) {
        println!("SLEEP({:?})", sleep_duration);
        self.sleep_duration = match sleep_duration {
            Val::Float(f) => *f,
            _ => panic!("invalid val for sleep(): {:?}", sleep_duration),
        };

        self.yield_to_meta();
    }

    #[no_mangle]
    pub extern "C" fn rt_runtime_alloc_datum(&mut self, ty: u32) -> NonNull<Datum> {
        let ty = InstanceTypeId::new(ty as usize);
        let ventry = &self.vtable[ty];
        // TODO: put spec in ventry?
        let ity = self.env.instances.lookup_instance(ty).unwrap();

        match ity.pty.specialization {
            Specialization::Datum => {
                unsafe {
                    let mut ptr: NonNull<Datum> = self.alloc.alloc(ventry.size as usize, ty).cast();

                    // TODO: init vars instead of nulling
                    for val in Datum::get_vars(ptr.as_mut(), ty, self) {
                        *val = crate::val::Val::Null;
                    }

                    ptr
                }
            },
            Specialization::List => {
                let mut ptr = self.alloc_list();
                unsafe {
                    *ptr.as_mut() = List::new();
                }
                ptr.cast()
            },
            _ => panic!("cannot alloc {:?} / {:?}", ity.pty.specialization, ty),
        }
    }

    #[no_mangle]
    pub extern "C" fn rt_runtime_spawn_closure(
        &mut self, closure_func: FuncId, num_args: u64, args: NonNull<Val>,
    ) {
        // TODO: handle delayed spawns
        println!(
            "doing spawn: {:?} / {:?} / {:?}",
            closure_func, num_args, args
        );
        //let spec = &self.env.func_specs[closure_func];
        let env = unsafe { std::slice::from_raw_parts(args.as_ptr(), num_args as usize).to_vec() };

        let args = crate::arg_pack::ClosureArgs { environment: env };
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
