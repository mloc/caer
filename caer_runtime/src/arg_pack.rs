use cstub_bindgen_macro::expose_c_stubs;
use std::mem;
use index_vec::Idx;

use crate::string_table::StringId;
use crate::runtime::Runtime;
use crate::val::Val;
use crate::environment::ProcId;

// meh, saves some duplicated code below, probably should be factored out
#[repr(C)]
struct FFIArray<T> {
    size: u64,
    data: *const T,
}

impl<T> FFIArray<T> {
    fn as_slice(&self) -> &[T] {
        unsafe { std::slice::from_raw_parts(self.data, self.size as usize) }
    }
}

#[repr(C)]
pub struct ArgPack {
    unnamed: FFIArray<Val>,
    named: FFIArray<(StringId, Val)>,
}

#[expose_c_stubs(rt_arg_pack)]
impl ArgPack {
    // this is p. inefficient.
    // unpacking probably shouldn't be done in libcode, probably shouldn't be copying vals
    // TODO: rework unpacking
    // TODO: support passing in proc_id as primitive idx
    fn unpack_into(&mut self, target_ptrs: *const *mut Val, proc_id: u64, rt: &mut Runtime) {
        let spec = &rt.env.proc_specs[ProcId::new(proc_id as usize)];
        // compiler should ensure that targets is an array with the same sizes as the spec
        let n = spec.params.len();
        let target_ptrs = unsafe { std::slice::from_raw_parts(target_ptrs, n) };

        for (i, arg) in self.unnamed.as_slice().iter().enumerate() {
            if i >= n {
                break // too many unnamed args
            }
            unsafe { *target_ptrs[i] = *arg }
        }

        let named_args = self.named.as_slice();

        let mut i_arg = 0;
        let mut i_param = 0;

        while i_arg < named_args.len() && i_param < n {
            if named_args[i_arg].0 < spec.names[i_param].0 {
                i_param += 1
            } else if named_args[i_arg].0 > spec.names[i_param].0 {
                i_arg += 1
            } else {
                unsafe { *target_ptrs[spec.names[i_param].1 as usize] = named_args[i_arg].1 };
                i_param += 1;
                i_arg += 1;
            }
        }

        // TODO: handle defaulting args
    }
}
