use crate::val::{rt_val_drop, Val};
use std::collections::HashMap;

#[derive(Debug)]
pub struct List {
    vec: Vec<Val>,
    map: Option<HashMap<Val, AssocValue>>,
}

#[derive(Debug)]
struct AssocValue {
    key_count: usize,
    // TODO: use null as empty val? some other variant?
    val: Option<Val>,
}

impl AssocValue {
    fn inc_count(&mut self) {
        self.key_count += 1;
    }

    /// returns true if key count is reduced to zero
    fn dec_count(&mut self) -> bool {
        assert_ne!(self.key_count, 0);
        if self.key_count > 0 {
            self.key_count -= 1;
        }
        self.key_count == 0
    }

    fn update_val(&mut self, new_val: Val) {
        if let Some(old_val) = self.val.replace(new_val) {
            rt_val_drop(old_val);
        }
    }

    fn remove_val(&mut self) {
        if let Some(old_val) = self.val.take() {
            rt_val_drop(old_val);
        }
    }

    /// Returns null if there's no value assigned.
    // TODO: pass up for RTEs, maybe just return the opt
    fn get_val(&self) -> Val {
        match self.val {
            Some(v) => v,
            None => Val::Null,
        }
    }
}

impl Default for AssocValue {
    fn default() -> Self {
        Self {
            key_count: 0,
            val: None,
        }
    }
}

impl List {
    fn new() -> Self {
        Self {
            vec: Vec::new(),
            map: None,
        }
    }

    /// Initialize the assoc map and populate with empty assoc values
    fn init_map(&mut self) {
        assert!(self.map.is_none());
        let mut map: HashMap<Val, AssocValue> = HashMap::new();

        for k in self.vec.iter() {
            map.entry(*k).or_default().inc_count();
        }

        self.map = Some(map)
    }

    fn ensure_map(&mut self) {
        if self.map.is_none() {
            self.init_map()
        }
    }

    fn push(&mut self, v: Val) {
        self.vec.push(v);
        if let Some(map) = self.map.as_mut() {
            map.entry(v).or_default().inc_count();
        }
    }

    fn len(&self) -> usize {
        self.vec.len()
    }

    fn insert(&mut self, idx: u32, v: Val) {
        if idx == 0 || (idx as usize) == self.vec.len() + 1 {
            return self.push(v);
        }
        if (idx as usize) > self.vec.len() {
            panic!("RTE OOB");
        }
        self.vec.insert((idx as usize) - 1, v);
        if let Some(map) = self.map.as_mut() {
            map.entry(v).or_default().inc_count();
        }
    }

    // 1-based index
    fn index_int(&self, idx: u32) -> Val {
        match self.vec.get((idx - 1) as usize) {
            Some(v) => *v,
            None => panic!("RTE OOB"),
        }
    }

    fn update_index_int(&mut self, idx: u32, new_val: Val) {
        self.ensure_map();

        let old_val = match self.vec.get_mut(idx as usize) {
            Some(v) => v,
            None => panic!("RTE OOB"),
        };

        // DQ002: updating a list by index will wipe out assoc values
        let map = self.map.as_mut().unwrap();
        if map.get_mut(old_val).unwrap().dec_count() {
            map.remove(old_val);
        } else {
            // TODO: opt out second lookup
            map.get_mut(old_val).unwrap().remove_val();
        }

        rt_val_drop(*old_val);
        *old_val = new_val;

        map.entry(new_val).or_default().inc_count();
    }

    fn index_assoc(&self, key: &Val) -> Val {
        // some lists do an RTE on bad assoc index, some don't
        // default to null; user lists do this
        // TODO: support for crashing lists
        if let Some(map) = &self.map {
            if let Some(assoc) = map.get(key) {
                assoc.get_val()
            } else {
                Val::Null
            }
        } else {
            Val::Null
        }
    }

    fn update_index_assoc(&mut self, key: &Val, new_val: Val) {
        self.ensure_map();
        if !self.map.as_mut().unwrap().contains_key(key) {
            self.push(*key)
        }
        self.map.as_mut().unwrap().get_mut(key).unwrap().update_val(new_val);
    }
}

#[cfg(test)]
mod tests {
    use super::List;
    use crate::val::Val;
    use crate::string_table::StringId;

    #[test]
    fn basic() {
        let mut l = List::new();
        l.push(Val::Float(1f32.into()));
        l.push(Val::Float(2f32.into()));
        l.insert(2, Val::Float(3f32.into()));

        assert_eq!(l.index_int(1), Val::Float(1f32.into()));
        assert_eq!(l.index_int(2), Val::Float(3f32.into()));
        assert_eq!(l.index_int(3), Val::Float(2f32.into()));
    }

    #[test]
    fn assoc() {
        let mut l = List::new();
        l.update_index_assoc(&Val::Float(1f32.into()), Val::Float(10f32.into()));
        assert_eq!(l.index_assoc(&Val::Float(1f32.into())), Val::Float(10f32.into()));

        l.update_index_assoc(&Val::Float(1f32.into()), Val::Float(12f32.into()));
        assert_eq!(l.index_assoc(&Val::Float(1f32.into())), Val::Float(12f32.into()));

        l.update_index_assoc(&Val::Float(2f32.into()), Val::Float(14f32.into()));
        assert_eq!(l.index_assoc(&Val::Float(1f32.into())), Val::Float(12f32.into()));
        assert_eq!(l.index_assoc(&Val::Float(2f32.into())), Val::Float(14f32.into()));
    }

    #[test]
    fn assoc_dq002() {
        /*
        ```
        var/list/l = list("a", "a")
        l["a"] = "foo"
        // "a" is now associated with "foo"
        l[1] = "a"
        ```
        */
        let a_str = Val::String(StringId::new(1));
        let foo_str = Val::String(StringId::new(2));

        let mut l = List::new();
        l.push(a_str);
        l.push(a_str);

        l.update_index_assoc(&a_str, foo_str);
        assert_eq!(l.index_assoc(&a_str), foo_str);

        assert_eq!(l.index_int(1), a_str);
        l.update_index_int(1, a_str);
        assert_eq!(l.index_int(1), a_str);

        assert_eq!(l.index_assoc(&a_str), Val::Null);
    }

    #[test]
    fn assoc_append() {
        let mut l = List::new();
        l.push(Val::Float(1f32.into()));
        assert_eq!(l.len(), 1);
        l.update_index_assoc(&Val::Float(2f32.into()), Val::Float(20f32.into()));
        assert_eq!(l.len(), 2);
        l.push(Val::Float(2f32.into()));
        assert_eq!(l.len(), 3);
        assert_eq!(l.index_int(2), Val::Float(2f32.into()));
    }

    #[test]
    fn assoc_no_append() {
        let mut l = List::new();
        l.push(Val::Float(1f32.into()));
        assert_eq!(l.len(), 1);
        l.push(Val::Float(2f32.into()));
        assert_eq!(l.len(), 2);
        l.update_index_assoc(&Val::Float(2f32.into()), Val::Float(20f32.into()));
        assert_eq!(l.len(), 2);
        assert_eq!(l.index_int(2), Val::Float(2f32.into()));
    }
}
