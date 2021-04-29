use std::cmp::Ordering;
use std::marker::PhantomData;
use std::ptr::NonNull;

use caer_types::func::CallingSpec;
use caer_types::id::{FuncId, StringId};

use crate::ffi::FfiArray;
use crate::runtime::Runtime;
use crate::val::Val;

#[derive(Clone, Debug)]
pub enum CallBundle {
    Proc((FuncId, ProcArgs)),
    Closure((FuncId, ClosureArgs)),
}

impl CallBundle {
    // TODO: rework error handling here. I guess in theory this can be a hard crash..
    pub fn validate_against(&self, spec: &CallingSpec) {
        match (self, spec) {
            (CallBundle::Proc(_), CallingSpec::Proc(_)) => {},
            (CallBundle::Closure((_, args)), CallingSpec::Closure(s)) => {
                assert_eq!(args.environment.len(), s.env_size as usize);
            },
            _ => {
                panic!("incompatible bundle/spec pair: {:?} / {:?}", self, spec);
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct ProcArgs {
    pub unnamed: Vec<Val>,
    pub named: Vec<(StringId, Val)>,

    pub src: Val,
}

impl ProcArgs {}

#[repr(C)]
#[derive(Debug)]
pub struct ProcPack<'a> {
    pub unnamed: FfiArray<Val>,
    pub named: FfiArray<(StringId, Val)>,

    pub src: Val,

    phantom: PhantomData<&'a ()>,
}

impl ProcArgs {
    pub fn empty() -> Self {
        Self {
            unnamed: Vec::new(),
            named: Vec::new(),
            src: Val::Null,
        }
    }

    pub fn as_pack(&self) -> ProcPack<'_> {
        unsafe {
            ProcPack {
                unnamed: FfiArray::from_vec(&self.unnamed),
                named: FfiArray::from_vec(&self.named),
                src: self.src,
                phantom: PhantomData,
            }
        }
    }
}

impl ProcPack<'_> {
    // this is p. inefficient.
    // unpacking probably shouldn't be done in libcode, probably shouldn't be copying vals
    // TODO: rework unpacking
    // TODO: support passing in proc_id as primitive idx
    fn unpack_into(&self, target_ptrs: NonNull<*mut Val>, func_id: FuncId, rt: &mut Runtime) {
        let spec = rt.env.func_specs[func_id]
            .calling_spec
            .get_proc_spec()
            .unwrap_or_else(|| panic!("func {:?} cannot be called as a proc", func_id));
        // compiler should ensure that targets is an array with the same sizes as the spec
        let targets_arr = unsafe { FfiArray::with_len(target_ptrs, spec.params.len()) };
        let targets = targets_arr.as_slice();

        for (i, arg) in self.unnamed.as_slice().iter().enumerate() {
            if i >= targets.len() {
                break; // too many unnamed args
            }
            unsafe { *targets[i] = *arg }
        }

        let named_args = self.named.as_slice();

        let mut i_arg = 0;
        let mut i_param = 0;

        while i_arg < named_args.len() && i_param < targets.len() {
            match named_args[i_arg].0.cmp(&spec.names[i_param].0) {
                Ordering::Less => i_arg += 1,
                Ordering::Greater => i_param += 1,
                Ordering::Equal => {
                    unsafe { *targets[spec.names[i_param].1 as usize] = named_args[i_arg].1 };
                    i_param += 1;
                    i_arg += 1;
                },
            }
        }

        // TODO: handle defaulting args
    }

    pub fn get(&self, idx: usize) -> Option<Val> {
        // TODO: something more efficient than building a slice every time
        self.unnamed.as_slice().get(idx).cloned()
    }
}

#[no_mangle]
pub extern "C" fn rt_arg_pack_unpack_into(
    argpack: &ProcPack, target_ptrs: NonNull<*mut Val>, proc_id: FuncId, rt: &mut Runtime,
) {
    argpack.unpack_into(target_ptrs, proc_id, rt)
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct ClosureArgs {
    pub environment: Vec<Val>,
}

#[repr(C)]
#[derive(Debug)]
pub struct ClosurePack<'a> {
    pub environment: FfiArray<Val>,
    phantom: PhantomData<&'a ()>,
}

impl ClosureArgs {
    pub fn as_pack(&self) -> ClosurePack<'_> {
        unsafe {
            ClosurePack {
                environment: FfiArray::from_vec(&self.environment),
                phantom: PhantomData,
            }
        }
    }
}
