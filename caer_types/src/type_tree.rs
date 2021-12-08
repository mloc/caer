use std::collections::HashMap;

use index_vec::IndexVec;
use serde::{Deserialize, Serialize};

use crate::id::{FuncId, PathTypeId, StringId};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeTree {
    types: IndexVec<PathTypeId, PathType>,
    type_by_path_str: HashMap<StringId, PathTypeId>,
}

impl TypeTree {
    pub fn from_parts(types: IndexVec<PathTypeId, PathType>) -> Self {
        let mut type_by_path_str = HashMap::new();

        for (id, pt) in types.iter_enumerated() {
            type_by_path_str.insert(pt.path_str, id);
        }

        Self {
            types,
            type_by_path_str,
        }
    }

    pub fn global_type(&self) -> &PathType {
        &self.types[PathTypeId::new(0)]
    }

    pub fn get_pty(&self, pty_id: PathTypeId) -> Option<&PathType> {
        self.types.get(pty_id)
    }

    pub fn lookup_path(&self, path: StringId) -> Option<PathTypeId> {
        self.type_by_path_str.get(&path).copied()
    }

    pub fn len(&self) -> usize {
        self.types.len()
    }

    pub fn is_empty(&self) -> bool {
        self.types.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &PathType> {
        self.types.iter()
    }

    pub fn iter_enumerated(&self) -> impl Iterator<Item = (PathTypeId, &PathType)> {
        self.types.iter_enumerated()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[repr(u8)]
pub enum Specialization {
    Val,
    Datum,
    List,
    String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PathType {
    pub id: PathTypeId,
    pub path_str: StringId,
    pub path_string: String,
    pub path: Vec<StringId>,
    pub type_path: Vec<StringId>,
    pub parent: Option<PathTypeId>,
    pub specialization: Specialization,

    pub vars: Vec<StringId>,
    pub var_lookup: HashMap<StringId, VarInfo>,

    pub procs: Vec<StringId>,
    pub proc_lookup: HashMap<StringId, ProcInfo>,
    // ... generics
}

impl PathType {
    pub fn empty() -> Self {
        Self {
            id: 0.into(),
            path_str: 0.into(),
            path_string: "".into(),
            path: Vec::new(),
            type_path: Vec::new(),
            parent: None,
            specialization: Specialization::Datum,
            vars: Vec::new(),
            var_lookup: HashMap::new(),
            procs: Vec::new(),
            proc_lookup: HashMap::new(),
        }
    }

    pub fn iter_procs(&self) -> impl Iterator<Item = &ProcInfo> + '_ {
        self.procs.iter().map(move |id| &self.proc_lookup[id])
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VarInfo {
    pub name: StringId,
    // TODO: just a dty for now; could maybe be a fully-blown prim or complex type
    pub assoc_dty: Option<PathTypeId>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcInfo {
    pub name: StringId,
    pub top_proc: FuncId,
}
