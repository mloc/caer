// this is gonna be too monolithic, split it up;

use caer_types::func::FuncInfo;
use caer_types::id::FuncId;
use caer_types::rt_env::RtEnv;
use caer_types::type_tree::TypeTree;
use index_vec::IndexVec;
use serde::{Deserialize, Serialize};

// TODO: move stringtable into here, at least intern
#[derive(Debug, Serialize, Deserialize)]
pub struct Environment {
    // move into typetree?
    pub(crate) func_specs: IndexVec<FuncId, FuncInfo>,
    pub type_tree: TypeTree,
}

impl Environment {
    pub fn from_rt_env(rt_env: RtEnv) -> Self {
        Self {
            func_specs: rt_env.func_specs,
            type_tree: rt_env.type_tree,
        }
    }

    /*pub fn add_proc(&mut self, name: StringId) -> FuncId {
        let spec = FuncInfo {
            name,
            params: vec![],
            names: vec![],
        };
        let id = FuncId::new(self.proc_specs.len());
        self.proc_specs.push(spec);
        id
    }*/

    // TODO: ERRH
    pub fn get_proc(&self, id: FuncId) -> &FuncInfo {
        self.func_specs.get(id).unwrap()
    }

    // TODO: ERRH
    pub fn get_proc_mut(&mut self, id: FuncId) -> &mut FuncInfo {
        self.func_specs.get_mut(id).unwrap()
    }
}
