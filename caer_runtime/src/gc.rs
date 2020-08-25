use crate::datum::{Datum, GcMarker};
use crate::gc_stackmap::*;
use crate::list::List;
use crate::runtime::Runtime;
use crate::val::{self, Val};
use caer_types::id::TypeId;
use caer_types::layout;
use caer_types::type_tree::{DType, Specialization};
use std::ptr::NonNull;

pub fn run(rt: &mut crate::runtime::Runtime) {
    use unwind::{Cursor, RegNum};

    let mut ptrs = Vec::new();
    Cursor::local(|mut cursor| {
        loop {
            let ip = cursor.register(RegNum::IP)?;
            let record = rt.gc_stackmap.addr_to_record.get(&ip);

            if let Some(record) = record {
                for location in record.locations.iter() {
                    let ptr = match location.pointer {
                        LocationPointer::Direct { reg, offset } => {
                            // TODO: very x86-64 specific
                            if reg != 7 {
                                panic!("can only handle rsp in direct, got {}", reg);
                            }
                            let addr =
                                (cursor.register(RegNum::SP).unwrap() as i64) + (offset as i64);
                            addr as *mut u32 // alignment?
                        }
                        LocationPointer::Indirect { reg, offset } => {
                            // TODO: very x86-64 specific
                            if reg != 7 {
                                panic!("can only handle rsp in indirect, got {}", reg);
                            }
                            let addr =
                                (cursor.register(RegNum::SP).unwrap() as i64) + (offset as i64);
                            unsafe { *(addr as *const *mut u32) }
                        }
                        _ => panic!("unhandled stackmap location: {:?}", location),
                    };

                    ptrs.push(ptr);
                }
            }

            if !cursor.step()? {
                break;
            }
        }

        Ok(())
    })
    .unwrap();

    let mut gc = Gc { runtime: rt };

    for ptr in ptrs {
        gc.scan_root(ptr);
    }
    gc.sweep();
}

struct Gc<'a> {
    runtime: &'a mut Runtime,
}

impl<'a> Gc<'a> {
    fn scan_root(&mut self, root: *mut u32) {
        let disc = unsafe { *root };

        //println!("ADDR: {:?}", root);
        //println!("DISC: {:x}", disc);

        println!("handling root at {:?}", root);

        if disc >= layout::VAL_DISCRIM_NULL {
            // ptr points to a stack value
            let val = unsafe { (root as *mut val::Val).as_ref().unwrap() };
            self.scan_val(val);
        } else {
            // ptr points to a heap gdatum
            self.scan_gdatum(NonNull::new(root as *mut Datum).unwrap());
        }
    }

    fn scan_val(&mut self, val: &Val) {
        println!("found val at {:?}, {:?}", val as *const Val, val);
        match val {
            // TODO: string GC
            Val::String(_) => {}
            Val::Ref(Some(datum_ptr)) => {
                self.scan_gdatum(*datum_ptr);
            }
            _ => {}
        }
    }

    // TODO: the name "datum" is overloaded, as is "type". split it up.
    fn scan_gdatum(&mut self, mut datum_ptr: NonNull<Datum>) {
        let gc_marker = unsafe { &mut datum_ptr.as_mut().gc_marker };
        // TODO: incremental, use grey
        match *gc_marker {
            GcMarker::Black => {
                return;
            }
            GcMarker::Grey => unimplemented!("grey unimplemented, no incremental"),
            GcMarker::White => {
                *gc_marker = GcMarker::Black;
            }
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
                self.scan_datum(datum_ptr, dty_id);
            }
            Specialization::List => {
                let mut list_ptr = datum_ptr.cast();
                let list_ref = unsafe { list_ptr.as_mut() };
                self.scan_list(list_ref);
            }
        }
    }

    fn scan_datum(&mut self, mut datum_ptr: NonNull<Datum>, dty_id: TypeId) {
        println!(
            "found datum at {:?} with dty {:?}",
            datum_ptr.as_ptr(),
            dty_id
        );
    }

    fn scan_list(&mut self, list: &mut List) {
        println!("found list at {:?}", list as *mut List);
        for val in list.gc_iter() {
            self.scan_val(val);
        }
    }

    fn sweep(&mut self) {
        let mut to_free = Vec::new();
        let iter = self.runtime.alloc.iter_allocations();
        println!("sweeping {} allocations", iter.len());
        for ptr in iter {
            println!("sweeping {:?}", ptr);
            let mut datum_ptr = ptr.cast();
            let datum_ref: &mut Datum = unsafe { datum_ptr.as_mut() };
            println!("marker is {:?}", datum_ref.gc_marker);
            match datum_ref.gc_marker {
                GcMarker::Black => datum_ref.gc_marker = GcMarker::White,
                GcMarker::Grey => panic!("should have no grey left during sweep?"),
                GcMarker::White => to_free.push(ptr),
            }
        }

        unsafe {
            for ptr in to_free {
                println!("freeing {:?}", ptr);
                self.runtime.alloc.dealloc(ptr);
            }
        }
    }
}
