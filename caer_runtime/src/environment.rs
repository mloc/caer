// this is gonna be too monolithic, split it up;

use caer_types::func::FuncInfo;
use caer_types::id::{FuncId, InstanceTypeId, PathTypeId, TypeId};
use caer_types::instance::InstanceTypes;
use caer_types::rt_env::RtEnvBundle;
use caer_types::string::FrozenStringTable;
use caer_types::ty::{RefType, Type};
use caer_types::type_tree::{PathType, Specialization, TypeTree};
use index_vec::IndexVec;
use serde::{Deserialize, Serialize};

// TODO: move stringtable into here, at least intern
#[derive(Debug, Serialize, Deserialize)]
pub struct Environment {
    pub(crate) string_table: FrozenStringTable,
    // move into typetree?
    pub(crate) func_specs: IndexVec<FuncId, FuncInfo>,
    pub type_tree: TypeTree,
    pub instances: InstanceTypes,
    types: IndexVec<TypeId, Type>,
}

impl Environment {
    pub fn from_rt_env(rt_env: RtEnvBundle) -> Self {
        Self {
            string_table: rt_env.string_table,
            func_specs: rt_env.func_specs,
            type_tree: rt_env.type_tree,
            instances: rt_env.instances,
            types: rt_env.types,
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

    pub fn get_type(&self, id: TypeId) -> Option<&Type> {
        self.types.get(id)
    }

    pub fn get_type_spec(&self, id: InstanceTypeId) -> Specialization {
        // TODO: ERRH
        self.instances
            .lookup_instance(id)
            .unwrap()
            .pty
            .specialization
    }

    pub fn get_path_type(&self, id: PathTypeId) -> Option<&PathType> {
        self.type_tree.get_pty(id)
    }

    pub fn resolve_type_to_path(&self, id: TypeId) -> Option<&PathType> {
        let ty = self.get_type(id)?;
        match ty {
            Type::Ref(RefType::Exact(pty_id) | RefType::Subtype(pty_id)) => {
                self.get_path_type(*pty_id)
            },
            _ => None,
        }
    }
}
