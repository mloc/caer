use crate::id::FuncId;
use crate::proc::ProcSpec;
use crate::type_tree::TypeTree;
use index_vec::IndexVec;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RtEnv {
    pub type_tree: TypeTree,
    pub proc_specs: IndexVec<FuncId, ProcSpec>,
}
