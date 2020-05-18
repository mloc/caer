use cstub_bindgen_macro::expose_c_stubs;
use std::mem;
use crate::string::DmString;
use crate::runtime::Runtime;
use crate::val::Val;

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
    named: FFIArray<(DmString, Val)>,
}

#[expose_c_stubs(rt_arg_pack)]
impl ArgPack {
    // this is p. inefficient.
    // unpacking probably shouldn't be done in libcode, probably shouldn't be copying vals
    // TODO: rework unpacking
    fn unpack_into(&mut self, targets: *mut Val, proc_id: u64, rt: &mut Runtime) {
        let spec = &rt.proc_specs[proc_id as usize];
        // compiler should ensure that targets is an array with the same sizes as the spec
        let n = spec.params.len();
        let targets = unsafe { std::slice::from_raw_parts_mut(targets, n) };

        for (i, arg) in self.unnamed.as_slice().iter().enumerate() {
            if i >= n {
                break // too many unnamed args
            }
            targets[i] = *arg
        }

        let named_args = self.named.as_slice();

        let mut i_arg = 0;
        let mut i_param = 0;

        while i_arg <= named_args.len() && i_param <= n {
            if named_args[i_arg].0 < spec.names[i_param].0 {
                i_param += 1
            } else if named_args[i_arg].0 > spec.names[i_param].0 {
                i_arg += 1
            } else {
                targets[spec.names[i_param].1 as usize] = named_args[i_arg].1;
                i_param += 1;
                i_arg += 1;
            }
        }

        // TODO: handle defaulting args
    }
}
