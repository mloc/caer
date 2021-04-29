use std::borrow::Cow;
use std::collections::HashMap;

use caer_types::id::{FuncId, StringId};
use caer_types::type_tree::TypeTree;

use super::cfg::*;
use crate::string::StringTable;

// this should be renamed; ir-something
// this is the output of the frontend
#[derive(Debug)]
pub struct Env {
    pub string_table: StringTable,
    pub type_tree: TypeTree,
    pub funcs: HashMap<FuncId, Function>,
    next_func_id: FuncId,
}

impl Env {
    pub fn new() -> Self {
        let string_table = StringTable::new();
        Self {
            string_table,
            type_tree: TypeTree::new(),
            funcs: HashMap::new(),
            next_func_id: 0.into(),
        }
    }

    pub fn new_func(&mut self) -> Function {
        let id = self.next_func_id;
        self.next_func_id += 1;
        Function::new(id)
    }

    pub fn assimilate_func(&mut self, func: Function) {
        assert!(!self.funcs.contains_key(&func.id));
        self.funcs.insert(func.id, func);
    }

    pub fn intern_string<'s>(&mut self, s: impl Into<Cow<'s, str>>) -> StringId {
        self.string_table.put(s)
    }

    pub fn intern_string_ro(&self, s: impl AsRef<str>) -> StringId {
        self.string_table.lookup(s.as_ref()).unwrap()
    }
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}
