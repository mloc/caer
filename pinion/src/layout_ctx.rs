use std::any;
use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;
use std::rc::Rc;

use crate::layout::Layout;
use crate::traits::PinionData;
use crate::PinionStruct;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LayoutId<'ctx> {
    tyid: any::TypeId,
    phantom: PhantomData<&'ctx ()>,
}

impl<'ctx> LayoutId<'ctx> {
    fn of<T: PinionData>() -> Self {
        unsafe { Self::of_unchecked::<T::Static>() }
    }

    unsafe fn of_unchecked<T: 'static>() -> Self {
        Self {
            tyid: any::TypeId::of::<T>(),
            phantom: PhantomData::<&'ctx ()>,
        }
    }
}

#[derive(Debug, Default)]
pub struct LayoutCtx<'ctx> {
    layouts: HashMap<LayoutId<'ctx>, Rc<Layout<'ctx>>>,
    ids: HashSet<LayoutId<'ctx>>,
    phantom: PhantomData<&'ctx ()>,
}

// Invariant: if we hand out a LayoutId<'ctx>, we must hold onto the correspondong layout for at
// least 'ctx
impl<'ctx> LayoutCtx<'ctx> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn populate<'a, T: PinionData>(&'a mut self) -> LayoutId<'ctx> {
        unsafe { self.unchecked_populate_fn::<T::Static, _>(|lctx| T::get_layout(lctx)) }
    }

    pub fn get(&self, id: LayoutId) -> Option<Rc<Layout<'ctx>>> {
        self.layouts.get(&id).cloned()
    }

    pub fn get_ty<T: PinionData>(&self) -> Option<Rc<Layout<'ctx>>> {
        self.layouts.get(&LayoutId::of::<T>()).cloned()
    }

    pub fn get_gep_indices<T: PinionStruct>(&'ctx mut self, path: &[&'static str]) -> Vec<u64> {
        let mut indices = vec![0];

        let mut cur_layout = self.populate::<T>();
        for part in path {
            let layout = self.get(cur_layout).unwrap();
            let (index, new_layout) = match layout.borrow() {
                Layout::Struct(sl) => sl.lookup_field(part).unwrap(),
                _ => panic!(),
            };

            cur_layout = new_layout;
            indices.push(index as u64)
        }

        indices
    }

    pub unsafe fn unchecked_populate_fn<'a, T: 'static, F: FnOnce(&mut Self) -> Layout<'ctx>>(
        &mut self, f: F,
    ) -> LayoutId<'ctx>
where {
        let id = LayoutId::<'ctx>::of_unchecked::<T>();
        if !self.ids.contains(&id) {
            self.ids.insert(id);
            let layout: Layout<'ctx> = {
                let s: &mut LayoutCtx<'ctx> = self;
                let layout: Layout<'ctx> = f(s);
                layout
            };
            self.layouts.insert(id, Rc::new(layout));
        }
        id
    }
}
