use std::ptr::NonNull;

use caer_types::id::TypeId;
use caer_types::layout;
use caer_types::type_tree::Specialization;

use super::state::State;
use crate::datum::Datum;
use crate::heap_object::{GcMarker, HeapKind};
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
            self.mark_gdatum(root.cast());
        }
    }

    fn mark_val(&mut self, val: &Val) {
        dbg!(val);
        match val {
            Val::String(Some(string_ptr)) => {
                // Strings are gdatums for now.
                self.mark_gdatum(string_ptr.cast());
            },
            Val::Ref(Some(datum_ptr)) => {
                self.mark_gdatum(*datum_ptr);
            },
            _ => {},
        }
    }

    // TODO: the name "datum" is overloaded, as is "type". split it up.
    fn mark_gdatum(&mut self, mut datum_ptr: NonNull<Datum>) {
        dbg!(unsafe { datum_ptr.as_mut() });
        if !self.runtime.alloc.contains(datum_ptr.cast()) {
            // Likely a constant string
            // TODO: specifically skip const strings
            return;
        }
        let datum_ref = unsafe { datum_ptr.as_mut() };
        dbg!(datum_ref.heap_header.kind);
        assert!(matches!(datum_ref.heap_header.kind, HeapKind::Datum));

        let gc_marker = &mut datum_ref.heap_header.gc_marker;
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

        let dty_id = unsafe { datum_ptr.as_ref().ty };
        assert!(dty_id.index() < layout::VAL_DISCRIM_NULL as usize);
        let dty = &self.runtime.env.type_tree.types[dty_id];

        println!(
            "found gdatum at {:?} with dty {:?} ({})",
            datum_ptr.as_ptr(),
            dty_id,
            dty.path_string
        );

        match dty.specialization {
            Specialization::Datum => {
                self.mark_datum(datum_ptr, dty_id);
            },
            Specialization::List => {
                let mut list_ptr = datum_ptr.cast();
                let list_ref = unsafe { list_ptr.as_mut() };
                self.mark_list(list_ref);
            },
        }
    }

    fn mark_datum(&mut self, mut datum_ptr: NonNull<Datum>, dty_id: TypeId) {
        println!(
            "found datum at {:?} with dty {:?}",
            datum_ptr.as_ptr(),
            dty_id
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
