use super::cfg::*;
use crate::string::StringTable;
use caer_types::id::{ProcId, StringId};
use caer_types::proc::ProcSpec;
use caer_types::type_tree::TypeTree;
use index_vec::IndexVec;
use std::borrow::Cow;

// this should be renamed; ir-something
// this is the output of the frontend
#[derive(Debug)]
pub struct Env {
    pub string_table: StringTable,
    pub proc_specs: IndexVec<ProcId, ProcSpec>,
    pub type_tree: TypeTree,
    pub procs: IndexVec<ProcId, Proc>,
}

impl Env {
    pub fn new() -> Self {
        let string_table = StringTable::new();
        Self {
            string_table,
            proc_specs: IndexVec::new(),
            type_tree: TypeTree::new(),
            procs: IndexVec::new(),
        }
    }

    pub fn add_proc(&mut self, name: StringId) -> ProcId {
        let spec = ProcSpec {
            name,
            params: vec![],
            names: vec![],
        };
        let id = ProcId::new(self.proc_specs.len());
        self.proc_specs.push(spec);
        id
    }

    // TODO: ERRH
    pub fn get_proc(&self, id: ProcId) -> &ProcSpec {
        self.proc_specs.get(id).unwrap()
    }

    // TODO: ERRH
    pub fn get_proc_mut(&mut self, id: ProcId) -> &mut ProcSpec {
        self.proc_specs.get_mut(id).unwrap()
    }

    pub fn intern_string<'s>(&mut self, s: impl Into<Cow<'s, str>>) -> StringId {
        self.string_table.put(s)
    }

    pub fn intern_string_ro(&self, s: impl AsRef<str>) -> StringId {
        self.string_table.lookup(s.as_ref()).unwrap()
    }
}
