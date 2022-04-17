use std::any;
use std::collections::HashMap;

use crate::layout::BasicType;
use crate::traits::PinionData;
use crate::PinionStruct;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LayoutId {
    tyid: any::TypeId,
}

impl LayoutId {
    fn of<T: PinionData>() -> Self {
        Self {
            tyid: any::TypeId::of::<T::Static>(),
        }
    }
}

pub struct LayoutCtx {
    layouts: HashMap<LayoutId, BasicType>,
}

impl LayoutCtx {
    pub fn new() -> Self {
        Self {
            layouts: HashMap::new(),
        }
    }

    pub fn populate<T: PinionData>(&mut self) -> LayoutId {
        let id = LayoutId::of::<T>();
        if !self.layouts.contains_key(&id) {
            let layout = T::get_layout(self);
            self.layouts.insert(id, layout);
        }
        id
    }

    pub fn get(&self, id: LayoutId) -> Option<&BasicType> {
        self.layouts.get(&id)
    }

    pub fn get_ty<T: PinionData>(&self) -> Option<&BasicType> {
        self.layouts.get(&LayoutId::of::<T>())
    }

    pub fn get_gep_indices<T: PinionStruct>(&mut self, path: &[&'static str]) -> Vec<u64> {
        let mut indices = vec![0];

        let mut cur_layout = self.populate::<T>();
        for part in path {
            let (index, new_layout) = match self.get(cur_layout).unwrap() {
                BasicType::Struct(sl) => sl.lookup_field(part).unwrap(),
                _ => panic!(),
            };

            cur_layout = new_layout;
            indices.push(index as u64)
        }

        indices
    }
}

impl Default for LayoutCtx {
    fn default() -> Self {
        Self::new()
    }
}
