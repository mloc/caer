use std::ptr::NonNull;

use super::state::State;
use crate::gc_stackmap::{GcStackmap, LocationPointer};

// Stack scanner
pub fn scan_stack(state: &mut State, stackmap: &GcStackmap) {
    backtrace::trace(|frame| {
        let mut name = None;
        let mut sym_base = None;
        backtrace::resolve_frame(frame, |sym| {
            name = sym.name().map(|n| format!("{}", n));
            sym_base = sym.addr();
        });
        let name = name.unwrap_or_else(|| "{unknown}".into());
        let sym_base = sym_base.expect("!!! no base") as u64;

        let ip = frame.ip() as u64;

        assert!(ip >= sym_base);
        let offset = (ip as i64) - (sym_base as i64);

        let record = stackmap.addr_to_record.get(&ip);

        if let Some(record) = record {
            for loc in record.root_locations.iter() {
                let ptr = resolve_pointer(frame, loc.location.pointer);
                println!("found root at {:?} in {}+{}", ptr, name, offset);
                println!(" -> location: {:?}", loc);

                state.add_root(ptr, loc.tag);
            }

            for loc in record.stack_locations.iter() {
                let ptr = resolve_pointer(frame, loc.pointer);
                println!("found stack val at {:?} in {}+{}", ptr, name, offset);
                println!(" -> location: {:?}", loc);

                state.add_stack_ptr(ptr);
            }
        }
        true
    });
}

fn resolve_pointer(frame: &backtrace::Frame, pointer: LocationPointer) -> NonNull<u8> {
    match pointer {
        LocationPointer::Direct { reg, offset } => {
            // TODO: x64 specific
            assert_eq!(reg, 7, "reg must be SP");
            let sp = frame.sp();
            assert!(!sp.is_null());
            let addr = (sp as i64) + (offset as i64);
            NonNull::new(addr as *mut u8).unwrap() // alignment..?
        },
        LocationPointer::Indirect { reg, offset } => {
            // TODO: x64 specific
            assert_eq!(reg, 7, "reg must be SP");
            let sp = frame.sp();
            assert!(!sp.is_null());
            let addr = (sp as i64) + (offset as i64);
            let ptr = unsafe { *(addr as *const *mut u8) };
            NonNull::new(ptr).unwrap()
        },
        _ => panic!("unhandled stackmap pointer: {:?}", pointer),
    }
}
