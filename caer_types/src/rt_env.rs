use index_vec::IndexVec;
use serde::{Deserialize, Serialize};

use crate::func::FuncInfo;
use crate::id::{FuncId, TypeId};
use crate::instance::InstanceTypes;
use crate::ty;
use crate::type_tree::TypeTree;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RtEnvBundle {
    pub type_tree: TypeTree,
    pub instances: InstanceTypes,
    pub func_specs: IndexVec<FuncId, FuncInfo>,
    pub types: IndexVec<TypeId, ty::Type>,
}
