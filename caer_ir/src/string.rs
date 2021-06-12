// string interner for IR
// has serializer for format compatible with runtime stringtable

use std::collections::HashMap;
use std::io::Write;

use caer_types::id::StringId;
use index_vec::IndexVec;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StringTable {
    strings: IndexVec<StringId, String>,
    ids: HashMap<String, StringId>,
}

impl StringTable {
    pub fn new() -> Self {
        let mut strings = IndexVec::new();
        strings.push("".into());
        Self {
            strings,
            ids: HashMap::new(),
        }
    }

    pub fn get(&self, id: StringId) -> &str {
        &self.strings[id]
    }

    pub fn lookup(&self, s: impl AsRef<str>) -> Option<StringId> {
        self.ids.get(s.as_ref()).copied()
    }

    pub fn put(&mut self, s: impl Into<String> + AsRef<str>) -> StringId {
        let s_ref = s.as_ref();
        if s_ref.is_empty() {
            return StringId::new(0);
        }

        if let Some(id) = self.ids.get(s_ref) {
            return *id;
        }

        let s_owned = s.into();

        let id = self.strings.next_idx();

        self.strings.insert(id, s_owned.clone());
        self.ids.insert(s_owned, id);

        id
    }

    // TODO: ERRH
    pub fn serialize_runtime(&self, writer: impl Write) {
        let flat: Vec<(StringId, &String)> = self.strings.iter_enumerated().collect();
        bincode::serialize_into(writer, &flat).unwrap();
    }

    pub fn iter(&self) -> impl Iterator<Item = (StringId, &str)> + ExactSizeIterator + '_ {
        self.strings
            .indices()
            .map(move |id| (id, self.strings[id].as_str()))
    }
}

impl Default for StringTable {
    fn default() -> Self {
        Self::new()
    }
}
