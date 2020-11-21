use super::cfg::*;
use crate::string::StringTable;
use caer_types::id::{FuncId, StringId};
use std::collections::HashMap;
use caer_types::proc::ProcSpec;
use caer_types::type_tree::TypeTree;
use index_vec::IndexVec;
use std::borrow::Cow;

// this should be renamed; ir-something
// this is the output of the frontend
#[derive(Debug)]
pub struct Env {
    pub string_table: StringTable,
    pub proc_specs: IndexVec<FuncId, ProcSpec>,
    pub type_tree: TypeTree,
    pub funcs: HashMap<FuncId, Function>,
    next_func_id: FuncId,
}

impl Env {
    pub fn new() -> Self {
        let string_table = StringTable::new();
        Self {
            string_table,
            proc_specs: IndexVec::new(),
            type_tree: TypeTree::new(),
            funcs: HashMap::new(),
            next_func_id: 0.into(),
        }
    }

    // proc/func creation is problematic & ugly
    // we hand out IDs and just sorta trust that they'll be given back
    pub fn add_proc(&mut self, name: StringId) -> FuncId {
        let spec = ProcSpec {
            name,
            params: vec![],
            names: vec![],
        };
        let id = self.next_func_id;
        self.next_func_id += 1;
        self.proc_specs.push(spec);
        id
    }

    pub fn new_func(&mut self) -> Function {
        let id = self.next_func_id;
        self.next_func_id += 1;
        Function::new(id)
    }

    pub fn add_func(&mut self, func: Function) {
        assert!(!self.funcs.contains_key(&func.id));
        self.funcs.insert(func.id, func);
    }

    // TODO: ERRH
    pub fn get_proc(&self, id: FuncId) -> &ProcSpec {
        self.proc_specs.get(id).unwrap()
    }

    // TODO: ERRH
    pub fn get_proc_mut(&mut self, id: FuncId) -> &mut ProcSpec {
        self.proc_specs.get_mut(id).unwrap()
    }

    pub fn intern_string<'s>(&mut self, s: impl Into<Cow<'s, str>>) -> StringId {
        self.string_table.put(s)
    }

    pub fn intern_string_ro(&self, s: impl AsRef<str>) -> StringId {
        self.string_table.lookup(s.as_ref()).unwrap()
    }
}
