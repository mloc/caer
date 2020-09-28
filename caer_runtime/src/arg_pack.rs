use crate::ffi::FFIArray;
use crate::runtime::Runtime;
use crate::val::Val;
use caer_types::id::{FuncId, StringId};
use std::cmp::Ordering;
use std::ptr::NonNull;

#[repr(C)]
#[derive(Debug)]
pub struct ArgPack {
    pub unnamed: FFIArray<Val>,
    pub named: FFIArray<(StringId, Val)>,

    pub src: Val,
}

impl ArgPack {
    pub fn empty() -> Self {
        Self {
            unnamed: FFIArray::empty(),
            named: FFIArray::empty(),
            src: Val::Null,
        }
    }

    // this is p. inefficient.
    // unpacking probably shouldn't be done in libcode, probably shouldn't be copying vals
    // TODO: rework unpacking
    // TODO: support passing in proc_id as primitive idx
    fn unpack_into(&self, target_ptrs: NonNull<*mut Val>, proc_id: FuncId, rt: &mut Runtime) {
        let spec = &rt.env.proc_specs[proc_id];
        // compiler should ensure that targets is an array with the same sizes as the spec
        let targets_arr = unsafe { FFIArray::with_len(target_ptrs, spec.params.len()) };
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
                }
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
    argpack: &mut ArgPack,
    target_ptrs: NonNull<*mut Val>,
    proc_id: FuncId,
    rt: &mut Runtime,
) {
    argpack.unpack_into(target_ptrs, proc_id, rt)
}
