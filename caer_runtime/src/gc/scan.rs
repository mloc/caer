use std::ptr::NonNull;

use libunwind_rs::Cursor;

use super::state::State;
use crate::gc_stackmap::{GcStackmap, LocationPointer};

// Stack scanner
pub fn scan_stack(state: &mut State, stackmap: &GcStackmap) {
    Cursor::local(|mut cursor| {
        loop {
            let ip = cursor.ip()? as u64;
            let record = stackmap.addr_to_record.get(&ip);

            if let Some(record) = record {
                let name = cursor.proc_name()?;
                let offset = (cursor.ip()? as i64) - (cursor.proc_info()?.start() as i64);

                for loc in record.root_locations.iter() {
                    let ptr = resolve_pointer(&mut cursor, loc.location.pointer)?;
                    println!("found root at {:?} in {}+{}", ptr, name, offset);
                    println!(" -> location: {:?}", loc);

                    state.add_root(ptr, loc.tag);
                }

                for loc in record.stack_locations.iter() {
                    let ptr = resolve_pointer(&mut cursor, loc.pointer)?;
                    println!("found stack val at {:?} in {}+{}", ptr, name, offset);
                    println!(" -> location: {:?}", loc);

                    state.add_stack_ptr(ptr);
                }
            }

            if !cursor.step()? {
                break;
            }
        }

        Ok(())
    })
    .unwrap();
}

fn resolve_pointer(
    cursor: &mut Cursor, pointer: LocationPointer,
) -> Result<NonNull<u8>, libunwind_rs::Error> {
    match pointer {
        LocationPointer::Direct { reg, offset } => {
            let addr = (cursor.register(map_regnum(reg))? as i64) + (offset as i64);
            Ok(NonNull::new(addr as *mut u8).unwrap()) // alignment?
        },
        LocationPointer::Indirect { reg, offset } => {
            let addr = (cursor.register(map_regnum(reg))? as i64) + (offset as i64);
            let ptr = unsafe { *(addr as *const *mut u8) };
            Ok(NonNull::new(ptr).unwrap())
        },
        _ => panic!("unhandled stackmap pointer: {:?}", pointer),
    }
}

#[inline(always)]
fn map_regnum(reg: u16) -> i32 {
    match reg {
        // RBP
        6 => 6,
        // RSP
        7 => 7,
        _ => panic!("bad regnum: {}", reg),
    }
}
