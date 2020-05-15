use crate::string_table;
use cstub_bindgen_macro::expose_c_stubs;

#[derive(Debug)]
pub struct Runtime {
    pub(crate) string_table: string_table::StringTable,
}

#[expose_c_stubs(rt_runtime)]
impl Runtime {
    pub fn new() -> Self {
        Runtime {
            string_table: string_table::StringTable::new(),
        }
    }
}
