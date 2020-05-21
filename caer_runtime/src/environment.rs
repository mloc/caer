// this is gonna be too monolithic, split it up;

use indexed_vec::{IndexVec, newtype_index, Idx};
use serde::{Serialize, Deserialize};

use crate::proc_spec::ProcSpec;
use crate::string_table::StringId;

newtype_index!(ProcId {pub idx});

// TODO: move stringtable into here, at least static
#[derive(Debug, Serialize, Deserialize)]
pub struct Environment {
    pub(crate) proc_specs: IndexVec<ProcId, ProcSpec>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            proc_specs: IndexVec::new(),
        }
    }

    pub fn add_proc(&mut self, name: StringId) -> ProcId {
        let spec = ProcSpec {
            name: name,
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
}
