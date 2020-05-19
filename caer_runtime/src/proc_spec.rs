use serde::{Serialize, Deserialize};

use crate::string_table::StringId;
use crate::environment::ProcId;

#[derive(Debug, Serialize, Deserialize)]
pub struct ProcSpec {
    pub name: StringId,
    //ty_path: u32,

    pub params: Vec<StringId>,
    // name + index into params list. sorted by name string id, used to resolve keyword args
    // we assume no two params have the same name. TODO: check this assumption
    pub names: Vec<(StringId, u32)>,
}
