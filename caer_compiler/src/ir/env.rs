use std::collections::HashMap;
use caer_runtime::string_table::{StringTable, StringId};
use caer_runtime;
use super::cfg::*;

// this should be renamed
#[derive(Debug)]
pub struct Environment {
    // is this bad? yes
    // but it's easy
    // TODO: do something less tightly coupled
    pub string_table: StringTable,
    pub rt_env: caer_runtime::environment::Environment,
    pub procs: HashMap<StringId, Proc>,
}

impl Environment {
    pub fn new(ot: &dreammaker::objtree::ObjectTree) -> Self {
        let mut string_table = StringTable::new();
        // tech debt, TODO: move TT init out of here
        let tt = caer_runtime::type_tree::TypeTree::from_objtree(ot, &mut string_table);
        Self {
            string_table: string_table,
            rt_env: caer_runtime::environment::Environment::new(tt),
            procs: HashMap::new(),
        }
    }
}

