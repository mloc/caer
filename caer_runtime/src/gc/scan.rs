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
                for location in record.locations.iter() {
                    let ptr = match location.pointer {
                        LocationPointer::Direct { reg, offset } => {
                            let addr = (cursor.register(map_regnum(reg))? as i64) + (offset as i64);
                            NonNull::new(addr as *mut u32).unwrap() // alignment?
                        },
                        LocationPointer::Indirect { reg, offset } => {
                            let addr = (cursor.register(map_regnum(reg))? as i64) + (offset as i64);
                            let ptr = unsafe { *(addr as *const *mut u32) };
                            NonNull::new(ptr).unwrap()
                        },
                        _ => panic!("unhandled stackmap location: {:?}", location),
                    };

                    let name = cursor.proc_name()?;
                    let offset = (cursor.ip()? as i64) - (cursor.proc_info()?.start() as i64);
                    println!("found root at {:?} in {}+{}", ptr, name, offset);
                    println!(" -> location: {:?}", location);

                    state.add_root(ptr);
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
