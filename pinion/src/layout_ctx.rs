use std::any;
use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::layout::Layout;
use crate::traits::PinionData;
use crate::PinionStruct;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LayoutId {
    tyid: any::TypeId,
}

impl LayoutId {
    fn of<T: PinionData>() -> Self {
        unsafe { Self::of_unchecked::<T::Static>() }
    }

    unsafe fn of_unchecked<T: 'static>() -> Self {
        Self {
            tyid: any::TypeId::of::<T>(),
        }
    }
}

#[derive(Debug, Default)]
pub struct LayoutCtx {
    layouts: HashMap<LayoutId, Rc<Layout>>,
    ids: HashSet<LayoutId>,
}

impl LayoutCtx {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn populate<T: PinionData>(&mut self) -> LayoutId {
        unsafe { self.unchecked_populate_fn::<T::Static, _>(|lctx| T::get_layout(lctx)) }
    }

    pub fn get(&self, id: LayoutId) -> Option<Rc<Layout>> {
        self.layouts.get(&id).cloned()
    }

    pub fn get_ty<T: PinionData>(&self) -> Option<Rc<Layout>> {
        self.layouts.get(&LayoutId::of::<T>()).cloned()
    }

    pub fn get_gep_indices<T: PinionStruct>(&mut self, path: &[&'static str]) -> Vec<u64> {
        let mut indices = vec![0];

        let mut cur_layout = self.populate::<T>();
        for part in path {
            let (index, new_layout) = match self.get(cur_layout).unwrap().borrow() {
                Layout::Struct(sl) => sl.lookup_field(part).unwrap(),
                _ => panic!(),
            };

            cur_layout = new_layout;
            indices.push(index as u64)
        }

        indices
    }

    pub unsafe fn unchecked_populate_fn<T: 'static, F: FnOnce(&mut Self) -> Layout>(
        &mut self, f: F,
    ) -> LayoutId {
        let id = LayoutId::of_unchecked::<T>();
        if !self.ids.contains(&id) {
            self.ids.insert(id);
            let layout = f(self);
            self.layouts.insert(id, Rc::new(layout));
        }
        id
    }
}
