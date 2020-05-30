use crate::string_table::StringTable;
use crate::environment::Environment;
use crate::vtable;
use crate::type_tree::TypeId;
use crate::datum::Datum;

use cstub_bindgen_macro::expose_c_stubs;

use std::fs::File;
use std::mem;
use std::alloc;

#[derive(Debug)]
pub struct Runtime {
    pub(crate) string_table: StringTable,
    pub(crate) vtable: vtable::Vtable,
    pub(crate) env: Environment,
}

#[expose_c_stubs(rt_runtime)]
impl Runtime {
    // TODO: ERRH
    pub fn init(&mut self, vtable_ptr: *const vtable::Entry) {
        let init_st = StringTable::deserialize(File::open("stringtable.bincode").unwrap());
        let init_env: Environment = bincode::deserialize_from(File::open("environment.bincode").unwrap()).unwrap();
        let vtable = vtable::Vtable::from_static(vtable_ptr, init_env.type_tree.types.len());

        let new = Runtime {
            string_table: init_st,
            vtable: vtable,
            env: init_env,
        };

        // this fn should only be called by init on a zeroed-out Runtime, which we need to ignore
        mem::forget(mem::replace(self, new));
    }

    // TODO: change arg when better prim support in macro
    pub fn alloc_datum(&self, ty: u32) -> *mut Datum {
        let ty = TypeId::new(ty as usize);
        let ventry = &self.vtable[ty];
        // TODO: revisit alignment
        let layout = alloc::Layout::from_size_align(ventry.size as usize, 8).unwrap();
        unsafe {
            // TODO: init vars instead of zeroing
            let ptr = alloc::alloc_zeroed(layout) as *mut Datum;
            (*ptr).ty = ty;

            ptr
        }
    }
}

impl Runtime {
    // TODO: fix lifetimes
    pub fn new_datum(&self, ty: TypeId) -> &mut Datum {
        unsafe {self.alloc_datum(ty.index() as u32).as_mut().unwrap()}
    }
}
