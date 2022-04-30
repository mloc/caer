use std::ptr::NonNull;

use caer_types::id::InstanceTypeId;
use caer_types::type_tree::Specialization;

use super::state::State;
use crate::datum::Datum;
use crate::heap_object::{GcMarker, HeapHeader};
use crate::list::List;
use crate::runtime::Runtime;
use crate::string::RtString;
use crate::val::Val;

pub struct Mark<'rt> {
    runtime: &'rt Runtime,
}

impl<'rt> Mark<'rt> {
    pub fn new(runtime: &'rt Runtime) -> Self {
        Self { runtime }
    }

    pub fn mark_all(&mut self, state: &State) {
        for (ptr, ty_id) in state.iter_roots() {
            self.mark_object(ty_id, ptr.cast());
        }
        for ptr in state.iter_stack_ptrs() {
            self.mark_val(unsafe { ptr.cast().as_ref() });
        }
    }

    // Roots are pointers to something on the stack.
    // (Dreams are messages from the deep)
    // BEFORE SLEEP: wtf; stack objs no longer follow the structure! why does this work!!
    /*fn mark_root(&mut self, root: NonNull<u8>, ty_id: InstanceTypeId) {
        // TODO: wo fetch this multiple times, clean up the flow
        let ity_data = self.runtime.env.type_tree.lookup_instance(ty_id).unwrap();

        println!("handling root at {:?}", root);
        println!("type is {:?}", ty_id);

        if ty.is_any() {
            // ptr points to a stack value
            let val: &Val = unsafe { root.cast().as_ref() };
            self.mark_val(val);
        } else {
            // ptr points to a heap gdatum
            self.mark_object(ty, root.cast());
        }
    }*/

    fn mark_val(&self, val: &Val) {
        dbg!(val);
        match val {
            Val::String(Some(string_ptr)) => {
                self.mark_string(string_ptr.cast());
            },
            Val::Ref(rr) => {
                //let ty = self.runtime.env.get_type(rr.vptr.id).unwrap();
                self.mark_object(rr.vptr.id, rr.ptr.cast());
            },
            _ => {},
        }
    }

    /// Returns false if marker was already set, true otherwise.
    fn mark_header(mut ptr: NonNull<HeapHeader>) -> bool {
        let header_ref = unsafe { ptr.as_mut() };
        let gc_marker = &mut header_ref.gc_marker;
        // TODO: incremental, use grey
        match *gc_marker {
            GcMarker::Black => {
                return false;
            },
            // Grey is implicit in the stack right now
            GcMarker::Grey => unimplemented!("grey unimplemented, no incremental"),
            GcMarker::White => {
                *gc_marker = GcMarker::Black;
            },
        }
        return true;
    }

    fn mark_object(&self, ty: InstanceTypeId, mut object_ptr: NonNull<HeapHeader>) {
        dbg!(unsafe { object_ptr.as_mut() });
        if !self.runtime.alloc.contains(object_ptr.cast()) {
            // Likely a constant string
            // TODO: specifically skip const strings. header flag?
            return;
        }
        if !Self::mark_header(object_ptr) {
            return;
        }

        let ity_data = self.runtime.env.instances.lookup_instance(ty).unwrap();
        println!(
            "found object at {:?} with ty {:?}",
            object_ptr.as_ptr(),
            ity_data
        );
        match ity_data.pty.specialization {
            Specialization::Datum => self.mark_datum(object_ptr.cast(), ty),
            Specialization::List => {
                let mut list_ptr = object_ptr.cast();
                let list_ref = unsafe { list_ptr.as_mut() };
                self.mark_list(list_ref);
            },
            x => todo!("{:?}", x),
        }
    }

    fn mark_string(&self, ptr: NonNull<RtString>) {
        if !self.runtime.alloc.contains(ptr.cast()) {
            // Likely a constant string
            // TODO: specifically skip const strings. header flag?
            return;
        }
        Self::mark_header(ptr.cast());
    }

    fn mark_datum(&self, mut datum_ptr: NonNull<Datum>, ty: InstanceTypeId) {
        println!(
            "found datum at {:?} with pathty {:?}",
            datum_ptr.as_ptr(),
            self.runtime.env.instances.lookup_instance(ty),
        );

        let vars = unsafe { Datum::get_vars(datum_ptr.as_mut(), ty, self.runtime) };
        println!("datum has {:?} vars", vars.len());
        for val in vars {
            self.mark_val(val);
        }
    }

    fn mark_list(&self, list: &mut List) {
        println!("found list at {:?}", list as *mut List);
        for val in list.gc_iter() {
            self.mark_val(val);
        }
    }
}
