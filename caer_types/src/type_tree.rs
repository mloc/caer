use std::collections::HashMap;

use index_vec::IndexVec;
use serde::{Deserialize, Serialize};

use crate::id::{FuncId, StringId, TypeId};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeTree {
    pub types: IndexVec<TypeId, DType>,
    pub type_by_path_str: HashMap<StringId, TypeId>,
    pub type_by_node_id: HashMap<u64, TypeId>,
}

impl TypeTree {
    pub fn new() -> Self {
        Self {
            types: IndexVec::new(),
            type_by_path_str: HashMap::new(),
            type_by_node_id: HashMap::new(),
        }
    }

    pub fn global_type(&self) -> &DType {
        &self.types[TypeId::new(0)]
    }
}

impl Default for TypeTree {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Specialization {
    Datum,
    List,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DType {
    pub id: TypeId,
    pub path_str: StringId,
    pub path_string: String,
    pub path: Vec<StringId>,
    pub type_path: Vec<StringId>,
    pub parent: Option<TypeId>,
    pub specialization: Specialization,

    pub vars: Vec<StringId>,
    pub var_lookup: HashMap<StringId, VarInfo>,

    pub procs: Vec<StringId>,
    pub proc_lookup: HashMap<StringId, ProcInfo>,
}

impl DType {
    pub fn iter_procs(&self) -> impl Iterator<Item = &ProcInfo> + '_ {
        self.procs.iter().map(move |id| &self.proc_lookup[id])
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VarInfo {
    pub name: StringId,
    // TODO: just a dty for now; could maybe be a fully-blown prim or complex type
    pub assoc_dty: Option<TypeId>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcInfo {
    pub name: StringId,
    pub top_proc: FuncId,
}
