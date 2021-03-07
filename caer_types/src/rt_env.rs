use crate::id::FuncId;
use crate::func::FuncInfo;
use crate::type_tree::TypeTree;
use index_vec::IndexVec;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RtEnv {
    pub type_tree: TypeTree,
    pub func_specs: IndexVec<FuncId, FuncInfo>,
}
