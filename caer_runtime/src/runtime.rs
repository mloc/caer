use cstub_bindgen_macro::expose_c_stubs;

use crate::string_table;
use crate::proc_spec::ProcSpec;

#[derive(Debug)]
pub struct Runtime {
    pub(crate) string_table: string_table::StringTable,
    pub(crate) proc_specs: Vec<ProcSpec>,
}

#[expose_c_stubs(rt_runtime)]
impl Runtime {
    pub fn init(init_st_bytes: *const u8, init_st_n: i64) -> Self {
        let init_st = unsafe { std::slice::from_raw_parts(init_st_bytes, init_st_n as usize) };
        Runtime {
            string_table: string_table::StringTable::deserialize(init_st),
            proc_specs: vec![],
        }
    }
}
