use crate::string_table::StringTable;
use crate::environment::Environment;
use crate::vtable;

use cstub_bindgen_macro::expose_c_stubs;

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
    pub fn init(&mut self, vtable_ptr: *const vtable::Entry) {
        let init_st = StringTable::deserialize(File::open("stringtable.bincode").unwrap());
        let init_env: Environment = bincode::deserialize_from(File::open("environment.bincode").unwrap()).unwrap();
        let vtable = vtable::Vtable::from_static(vtable_ptr, init_env.type_tree.types.len());
        println!("VTABLE: {:?}", vtable);

        let new = Runtime {
            string_table: init_st,
            vtable: vtable,
            env: init_env,
        };

        // this fn should only be called by init on a zeroed-out Runtime, which we need to ignore
        mem::forget(mem::replace(self, new));
    }
}
