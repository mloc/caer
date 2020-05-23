use cstub_bindgen_macro::expose_c_stubs;

use crate::string_table::StringTable;
use crate::proc_spec::ProcSpec;
use crate::environment::Environment;

use std::fs::File;
use std::mem;

#[derive(Debug)]
pub struct Runtime {
    pub(crate) string_table: StringTable,
    pub(crate) env: Environment,
}

#[expose_c_stubs(rt_runtime)]
impl Runtime {
    // TODO: ERRH
    pub fn init(&mut self) {
        let init_st = StringTable::deserialize(File::open("stringtable.bincode").unwrap());
        let init_env = bincode::deserialize_from(File::open("environment.bincode").unwrap()).unwrap();

        let new = Runtime {
            string_table: init_st,
            env: init_env,
        };

        // this fn should only be called by init on a zeroed-out Runtime, which we need to ignore
        mem::forget(mem::replace(self, new));
    }
}
