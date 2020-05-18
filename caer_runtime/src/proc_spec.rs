use serde::{Serialize, Deserialize};

use crate::string::DmString;

#[derive(Debug, Serialize, Deserialize)]
pub struct ProcSpec {
    pub name: DmString,
    //ty_path: u32,

    pub params: Vec<DmString>,
    // name + index into args list. sorted by name string id, used to resolve keyword args
    pub names: Vec<(DmString, u32)>,
}
