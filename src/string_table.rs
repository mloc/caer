use std::cell::RefCell;
use std::borrow::Cow;
use std::collections::HashMap;

// THOUGHTS
// 64 bits is maybe too big. for the sake of not overthinking stuff, I've left it as is, but it
//    bloats *all* values. using 32 bits might require some kind of reallocation.
// TODO: no GC currently, leaks memory like mad
// TODO: don't hold backrefs for large strings
//static UPPER_REGION: u64 = 1 << 31;
//static CUTOFF: usize = 64;

#[derive(Debug)]
pub struct StringTable {
    // optimized btreemap might be faster here
    strings: HashMap<u64, String>,
    ids: HashMap<String, u64>,

    next_id: u64,
}

impl StringTable {
    pub fn new() -> Self {
        Self {
            strings: HashMap::new(),
            ids: HashMap::new(),
            next_id: 1,
        }
    }

    pub fn get(&self, id: u64) -> &str {
        if id == 0 {
            return ""
        }
        &self.strings[&id]
    }

    pub fn put<'a, S: Into<Cow<'a, str>>>(&mut self, s: S) -> u64 {
        let s = s.into();
        if s.is_empty() {
            return 0
        }

        if let Some(id) = self.ids.get(s.as_ref()) {
            return *id
        }

        let s_owned = s.into_owned();

        let id = self.next_id;
        self.next_id += 1;

        self.strings.insert(id, s_owned.clone());
        self.ids.insert(s_owned, id);

        id
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
        let id1 = table.put("Hello".into());
        let id2 = table.put("Hello".into());
        assert_eq!(id1, id2);

        let id3 = table.put("hello".into());
        assert_ne!(id1, id3);
    }
}
