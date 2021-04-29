use index_vec::IndexVec;
use serde::{Deserialize, Serialize};

use crate::func::FuncInfo;
use crate::id::FuncId;
use crate::type_tree::TypeTree;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RtEnv {
    pub type_tree: TypeTree,
    pub func_specs: IndexVec<FuncId, FuncInfo>,
}
