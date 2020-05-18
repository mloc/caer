use serde::{Serialize, Deserialize};

use crate::string_table::StringId;

#[derive(Debug, Serialize, Deserialize)]
pub struct ProcSpec {
    pub name: StringId,
    //ty_path: u32,

    pub params: Vec<StringId>,
    // name + index into args list. sorted by name string id, used to resolve keyword args
    pub names: Vec<(StringId, u32)>,
}
