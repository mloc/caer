use std::io::Read;
use std::borrow::Cow;
use std::collections::HashMap;

use caer_types::id::StringId;

// THOUGHTS
// 64 bits is maybe too big. for the sake of not overthinking stuff, I've left it as is, but it
//    bloats *all* values. using 32 bits might require some kind of reallocation.
// TODO: no GC currently, leaks memory like mad
// TODO: when we GC, don't GC 'static' compiler-created strings
// TODO: don't hold backrefs for large strings
// TODO: optimization for small strings, <=7 bytes can be stuffed in the val, like servo?

//static UPPER_REGION: u64 = 1 << 31;
//static CUTOFF: usize = 64;

// not using newtype_idx because we want 64 bits, and might do some other hacking later.

#[derive(Debug)]
pub struct StringTable {
    // optimized btreemap might be faster here
    strings: HashMap<StringId, String>,
    ids: HashMap<String, StringId>,

    next_id: usize,
}

impl StringTable {
    pub fn new() -> Self {
        Self {
            strings: HashMap::new(),
            ids: HashMap::new(),
            next_id: 1,
        }
    }

    pub fn get(&self, id: StringId) -> &str {
        if id.index() == 0 {
            return ""
        }
        &self.strings[&id]
    }

    pub fn lookup(&self, s: impl AsRef<str>) -> Option<StringId> {
        self.ids.get(s.as_ref()).copied()
    }

    pub fn put<'a>(&mut self, s: impl Into<Cow<'a, str>>) -> StringId {
        let s = s.into();
        if s.is_empty() {
            return StringId::new(0)
        }

        if let Some(id) = self.ids.get(s.as_ref()) {
            return *id
        }

        let s_owned = s.into_owned();

        let id = StringId::new(self.next_id);
        self.next_id += 1;

        self.strings.insert(id, s_owned.clone());
        self.ids.insert(s_owned, id);

        id
    }

    // mightn't belong on stringtable
    pub fn concat(&mut self, l: StringId, r: StringId) -> StringId {
        self.put(format!("{}{}", self.get(l), self.get(r)))
    }

    // TODO: ERRH
    pub fn deserialize(reader: impl Read) -> StringTable {
        let flat: Vec<(StringId, String)> = bincode::deserialize_from(reader).unwrap();

        let mut strings = HashMap::new();
        let mut ids = HashMap::new();
        let mut next_id = 0;

        for (id, s) in flat {
            strings.insert(id, s.clone());
            ids.insert(s, id);
            if id.index() + 1 > next_id {
                next_id = id.index() + 1;
            }
        }

        StringTable {
            strings,
            ids,
            next_id,
        }
    }
}

impl Default for StringTable {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use crate::string_table::StringTable;

    #[test]
    fn identity() {
        let mut table = StringTable::new();
        let s: String = "Hello".into();
        let id = table.put(s.clone());
        assert_eq!(&s, table.get(id));
    }

    #[test]
    fn dedup() {
        let mut table = StringTable::new();
        let id1 = table.put("Hello");
        let id2 = table.put("Hello");
        assert_eq!(id1, id2);

        let id3 = table.put("hello");
        assert_ne!(id1, id3);
    }

    /*#[test]
    fn serde() {
        let mut src_table = StringTable::new();
        src_table.put("hello");
        src_table.put("world");

        let mut buf = vec![];
        src_table.serialize(&mut buf);
        let dest_table = StringTable::deserialize(buf.as_slice());

        println!("{:?}", dest_table);

        assert_eq!(src_table.strings, dest_table.strings);
        assert_eq!(src_table.ids, dest_table.ids);
        assert_eq!(src_table.next_id, dest_table.next_id);
    }*/
}
