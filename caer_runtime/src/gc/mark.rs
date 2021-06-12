use std::ptr::NonNull;

use caer_types::id::TypeId;
use caer_types::layout;
use caer_types::type_tree::Specialization;

use super::state::State;
use crate::datum::Datum;
use crate::heap_object::{GcMarker, HeapHeader, HeapKind};
use crate::list::List;
use crate::runtime::Runtime;
use crate::val::Val;

pub struct Mark<'rt> {
    runtime: &'rt Runtime,
}

impl<'rt> Mark<'rt> {
    pub fn new(runtime: &'rt Runtime) -> Self {
        Self { runtime }
    }

    pub fn mark_all(&mut self, state: &State) {
        for ptr in state.iter_roots() {
            self.mark_root(ptr);
        }
    }

    // Roots are pointers to something on the stack.
    // BEFORE SLEEP: wtf; stack objs no longer follow the structure! why does this work!!
    fn mark_root(&mut self, root: NonNull<u32>) {
        let disc = unsafe { *root.as_ref() };

        println!("handling root at {:?}", root);
        println!("disc is {:x}", disc);

        if disc >= layout::VAL_DISCRIM_NULL {
            // ptr points to a stack value
            let val: &Val = unsafe { root.cast().as_ref() };
            self.mark_val(val);
        } else {
            // ptr points to a heap gdatum
            self.mark_object(root.cast());
        }
    }

    fn mark_val(&mut self, val: &Val) {
        dbg!(val);
        match val {
            Val::String(Some(string_ptr)) => {
                // Strings are gdatums for now.
                self.mark_object(string_ptr.cast());
            },
            Val::Ref(Some(datum_ptr)) => {
                self.mark_object(datum_ptr.cast());
            },
            _ => {},
        }
    }

    fn mark_object(&mut self, mut object_ptr: NonNull<HeapHeader>) {
        dbg!(unsafe { object_ptr.as_mut() });
        if !self.runtime.alloc.contains(object_ptr.cast()) {
            // Likely a constant string
            // TODO: specifically skip const strings. header flag?
            return;
        }
        let header_ref = unsafe { object_ptr.as_mut() };
        dbg!(header_ref.kind);

        let gc_marker = &mut header_ref.gc_marker;
        // TODO: incremental, use grey
        match *gc_marker {
            GcMarker::Black => {
                return;
            },
            // Grey is implicit in the stack right now
            GcMarker::Grey => unimplemented!("grey unimplemented, no incremental"),
            GcMarker::White => {
                *gc_marker = GcMarker::Black;
            },
        }

        println!(
            "found object at {:?} with kind {:?}",
            object_ptr.as_ptr(),
            header_ref.kind,
        );

        match header_ref.kind {
            HeapKind::Datum => {
                self.mark_datum(object_ptr.cast());
            },
            HeapKind::List => {
                let mut list_ptr = object_ptr.cast();
                let list_ref = unsafe { list_ptr.as_mut() };
                self.mark_list(list_ref);
            },
            // Strings are leaves
            HeapKind::String => {},
        }
    }

    fn mark_datum(&mut self, mut datum_ptr: NonNull<Datum>) {
        let datum_ref = unsafe { datum_ptr.as_ref() };
        println!(
            "found datum at {:?} with pathty {:?}",
            datum_ptr.as_ptr(),
            datum_ref.ty,
        );

        let vars = unsafe { datum_ptr.as_mut().get_vars(self.runtime) };
        println!("datum has {:?} vars", vars.len());
        for val in vars {
            self.mark_val(val);
        }
    }

    fn mark_list(&mut self, list: &mut List) {
        println!("found list at {:?}", list as *mut List);
        for val in list.gc_iter() {
            self.mark_val(val);
        }
    }
}
