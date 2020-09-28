// this is gonna be too monolithic, split it up;

use index_vec::IndexVec;
use serde::{Serialize, Deserialize};

use caer_types::proc::ProcSpec;
use caer_types::id::{StringId, FuncId};
use caer_types::rt_env::RtEnv;
use caer_types::type_tree::TypeTree;

// TODO: move stringtable into here, at least intern
#[derive(Debug, Serialize, Deserialize)]
pub struct Environment {
    // move into typetree?
    pub(crate) proc_specs: IndexVec<FuncId, ProcSpec>,
    pub type_tree: TypeTree,
}

impl Environment {
    pub fn from_rt_env(rt_env: RtEnv) -> Self {
        Self {
            proc_specs: rt_env.proc_specs,
            type_tree: rt_env.type_tree,
        }
    }

    pub fn add_proc(&mut self, name: StringId) -> FuncId {
        let spec = ProcSpec {
            name,
            params: vec![],
            names: vec![],
        };
        let id = FuncId::new(self.proc_specs.len());
        self.proc_specs.push(spec);
        id
    }

    // TODO: ERRH
    pub fn get_proc(&self, id: FuncId) -> &ProcSpec {
        self.proc_specs.get(id).unwrap()
    }

    // TODO: ERRH
    pub fn get_proc_mut(&mut self, id: FuncId) -> &mut ProcSpec {
        self.proc_specs.get_mut(id).unwrap()
    }
}
