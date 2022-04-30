use caer_types::id::{FuncId, InstanceTypeId, StringId};
use caer_types::instance::InstanceTypes;
use caer_types::type_tree::{Specialization, TypeTree};
use index_vec::IndexVec;

use super::cfg::*;
use crate::string::StringTable;
use crate::type_repo::TypeRepo;

// this should be renamed; ir-something
// this is the output of the frontend
#[derive(Debug)]
pub struct Module {
    pub string_table: StringTable,
    pub type_tree: TypeTree,
    pub instances: InstanceTypes,

    pub funcs: IndexVec<FuncId, Function>,
    next_func_id: FuncId,
    pub types: TypeRepo,
}

impl Module {
    pub fn new(strings: StringTable, type_tree: TypeTree, instances: InstanceTypes) -> Self {
        Self {
            string_table: strings,
            type_tree,
            instances,
            funcs: IndexVec::new(),
            next_func_id: 0.into(),
            types: TypeRepo::new(),
        }
    }

    pub fn new_func(&mut self) -> Function {
        let id = self.next_func_id;
        self.next_func_id += 1;
        Function::new(id)
    }

    pub fn assimilate_func(&mut self, func: Function) {
        assert_eq!(func.id, self.funcs.next_idx());
        self.funcs.push(func);
    }

    pub fn intern_string(&mut self, s: impl Into<String> + AsRef<str>) -> StringId {
        self.string_table.put(s)
    }

    pub fn intern_string_ro(&self, s: impl AsRef<str>) -> StringId {
        self.string_table.lookup(s.as_ref()).unwrap()
    }

    pub fn get_type_spec(&self, ty: InstanceTypeId) -> Specialization {
        self.instances
            .lookup_instance(ty)
            .unwrap()
            .pty
            .specialization
    }
}
