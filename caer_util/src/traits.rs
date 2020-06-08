use index_vec::{IndexVec, Idx};
use std::collections::HashMap;
use std::hash::Hash;

pub trait Map<K, V> {
    fn map_get(&self, index: &K) -> Option<&V>;

    fn map_contains_key(&self, key: &K) -> bool {
        self.map_get(key).is_some()
    }
}

impl<I: Idx, V> Map<I, V> for &IndexVec<I, V> {
    fn map_get(&self, index: &I) -> Option<&V> {
        self.get(*index)
    }

    fn map_contains_key(&self, key: &I) -> bool {
        *key <= self.last_idx()
    }
}

impl<K: Hash + Eq, V> Map<K, V> for &HashMap<K, V> {
    fn map_get(&self, index: &K) -> Option<&V> {
        self.get(index)
    }

    fn map_contains_key(&self, key: &K) -> bool {
        self.contains_key(key)
    }
}
