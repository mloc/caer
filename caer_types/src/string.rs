use std::collections::HashMap;

use index_vec::IndexVec;
use serde::{Deserialize, Serialize};

use crate::id::StringId;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FrozenStringTable {
    strings: IndexVec<StringId, String>,
    ids: HashMap<String, StringId>,
}

impl FrozenStringTable {
    pub fn from_strings(strings: IndexVec<StringId, String>) -> Self {
        let ids = strings
            .iter_enumerated()
            .map(|(id, string)| (string.clone(), id))
            .collect();

        Self { strings, ids }
    }

    pub fn get(&self, id: StringId) -> &str {
        &self.strings[id]
    }

    pub fn lookup(&self, s: impl AsRef<str>) -> Option<StringId> {
        self.ids.get(s.as_ref()).copied()
    }
}
