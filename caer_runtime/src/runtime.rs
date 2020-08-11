use crate::datum::Datum;
use crate::environment::Environment;
use crate::list::List;
use crate::string_table::{StringId, StringTable};
use crate::type_tree::{Specialization, TypeId};
use crate::vtable;

use cstub_bindgen_macro::expose_c_stubs;

use std::alloc;
use std::fs::File;
use std::mem;

#[derive(Debug)]
pub struct Runtime {
    pub(crate) string_table: StringTable,
    pub(crate) vtable: vtable::Vtable,
    pub(crate) env: Environment,
}

#[expose_c_stubs(rt_runtime)]
impl Runtime {
    // TODO: ERRH
    pub fn init(&mut self, stackmap_start: *const u8, stackmap_end: *const u8, vtable_ptr: *const vtable::Entry) {
        let init_st = StringTable::deserialize(File::open("stringtable.bincode").unwrap());
        let init_env: Environment =
            bincode::deserialize_from(File::open("environment.bincode").unwrap()).unwrap();
        let vtable = vtable::Vtable::from_static(vtable_ptr, init_env.type_tree.types.len());

        let new = Runtime {
            string_table: init_st,
            vtable,
            env: init_env,
        };

        let stackmaps_raw = unsafe {
            let len = stackmap_end.offset_from(stackmap_start);
            if len <= 0 {
                panic!("bad stackmap, len is {}", len);
            }
            std::slice::from_raw_parts(stackmap_start, len as usize)
        };

        let stackmap = llvm_stackmaps::Parser::<llvm_stackmaps::LittleEndian>::parse(stackmaps_raw).unwrap();
        println!("STACKMAP: {:#?}", stackmap);

        // this fn should only be called by init on a zeroed-out Runtime, which we need to ignore
        mem::forget(mem::replace(self, new));
    }

    // TODO: change arg when better prim support in macro
    pub fn alloc_datum(&self, ty: u32) -> *mut Datum {
        let ty = TypeId::new(ty as usize);
        let ventry = &self.vtable[ty];
        // TODO: put spec in ventry?
        let spec = self.env.type_tree.types[ty].specialization;

        match spec {
            Specialization::Datum => {
                // TODO: revisit alignment
                let layout = alloc::Layout::from_size_align(ventry.size as usize, 8).unwrap();
                unsafe {
                    // TODO: init vars instead of zeroing
                    let ptr = alloc::alloc_zeroed(layout) as *mut Datum;
                    (*ptr).ty = ty;

                    ptr
                }
            }
            Specialization::List => {
                let list = Box::new(List::new(ty));
                Box::into_raw(list) as _
            }
        }
    }
}

impl Runtime {
    // TODO: fix lifetimes
    pub fn new_datum(&self, ty: TypeId) -> &mut Datum {
        unsafe { self.alloc_datum(ty.index() as u32).as_mut().unwrap() }
    }
}

impl Runtime {
    #[no_mangle]
    pub extern "C" fn rt_runtime_concat_strings(
        &mut self,
        lhs: StringId,
        rhs: StringId,
    ) -> StringId {
        self.string_table.concat(lhs, rhs)
    }
}
