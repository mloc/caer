use cstub_bindgen_macro::expose_c_stubs;

use crate::string_table::StringTable;
use crate::proc_spec::ProcSpec;
use crate::environment::Environment;

use std::fs::File;

#[derive(Debug)]
pub struct Runtime {
    pub(crate) string_table: StringTable,
    pub(crate) env: Environment,
}

#[expose_c_stubs(rt_runtime)]
impl Runtime {
    // TODO: ERRH
    pub fn init(init_st_bytes: *const u8, init_st_n: i64, init_env_bytes: *const u8, init_env_n: i64) -> Self {
        let init_st = StringTable::deserialize(File::open("stringtable.bincode").unwrap());
        let init_env = bincode::deserialize_from(File::open("environment.bincode").unwrap()).unwrap();

        Runtime {
            string_table: init_st,
            env: init_env,
        }
    }
}
