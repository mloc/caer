use serde::{Serialize, Deserialize};
use index_vec::IndexVec;
use std::collections::HashMap;
use crate::id::{TypeId, StringId, ProcId};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Specialization {
    Datum,
    List,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DType {
    pub id: TypeId,
    pub path_str: StringId,
    pub path: Vec<StringId>,
    pub type_path: Vec<StringId>,
    pub parent: Option<TypeId>,
    pub specialization: Specialization,

    pub vars: Vec<StringId>,
    pub var_lookup: HashMap<StringId, VarInfo>,

    pub procs: Vec<StringId>,
    pub proc_lookup: HashMap<StringId, ProcInfo>,
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
    pub top_proc: ProcId,
}