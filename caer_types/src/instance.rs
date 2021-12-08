use index_vec::IndexVec;
use serde::{Deserialize, Serialize};

use crate::id::{InstanceTypeId, PathTypeId};
use crate::type_tree::PathType;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstanceTypes {
    types: IndexVec<InstanceTypeId, InstanceType>,
}

impl InstanceTypes {
    pub fn new(types: IndexVec<InstanceTypeId, InstanceType>) -> Self {
        Self { types }
    }

    // The return type here is.. not great, but it's an interface.
    // Eventually will probably be something something enum something generics.
    pub fn lookup_instance(&self, ity_id: InstanceTypeId) -> Option<&InstanceType> {
        self.types.get(ity_id)
    }

    pub fn iter(&self) -> impl Iterator<Item = &InstanceType> {
        self.types.iter()
    }

    pub fn iter_enumerated(&self) -> impl Iterator<Item = (InstanceTypeId, &InstanceType)> {
        self.types.iter_enumerated()
    }
}

/// Represents a concrete instance of a PathType.
///
/// For now, there's a 1-1 mapping between this and PathType. In the future, this will encode
/// generic parameter values and other fun instance info.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstanceType {
    pub id: InstanceTypeId,
    pub path_type: PathTypeId,
    // To save on lifetime shenanigans, this is just cloned in for now.
    pub pty: PathType,
    // ... generic args
}
