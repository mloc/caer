// TODO: maybe don't throw exceptions in methods, use result+unwrapper?

use std::collections::{hash_map, HashMap};
use std::ffi::c_void;
use std::ptr::NonNull;
use std::slice;

use caer_types::id::StringId;
use caer_types::type_tree::Specialization;
use pinion::pinion_export;

use crate::arg_pack::ProcPack;
use crate::heap_object::HeapHeader;
use crate::rtti::RttiRef;
use crate::runtime::Runtime;
use crate::string::RtString;
use crate::val::Val;
use crate::vtable::{Entry, ProcPtr};

#[derive(Debug, Clone)]
#[repr(C)]
pub struct List {
    heap_header: HeapHeader,
    vec: Vec<Val>,
    map: Option<HashMap<Val, AssocValue>>,
}

#[derive(Debug, Clone, Default)]
struct AssocValue {
    key_count: usize,
    // TODO: use null as empty val? some other variant?
    val: Option<Val>,
}

impl AssocValue {
    fn inc_count(&mut self) {
        self.key_count += 1;
    }

    /// Returns true if key count is reduced to zero
    fn dec_count(&mut self) -> bool {
        assert_ne!(self.key_count, 0);
        if self.key_count > 0 {
            self.key_count -= 1;
        }
        self.key_count == 0
    }

    fn update_val(&mut self, new_val: Val) {
        if let Some(_old_val) = self.val.replace(new_val) {
            //rt_val_drop(&old_val);
        }
    }

    fn remove_val(&mut self) {
        if let Some(_old_val) = self.val.take() {
            //rt_val_drop(&old_val);
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

impl List {
    pub fn new() -> Self {
        Self {
            heap_header: HeapHeader::list(),
            vec: Default::default(),
            map: Default::default(),
        }
    }

    pub fn on_gc_heap(rt: &mut Runtime, list: List) -> &mut Self {
        let ptr = rt.alloc_list();

        unsafe {
            let lref = &mut *ptr.as_ptr();
            *lref = list;
            lref
        }
    }

    fn iter(&self) -> impl Iterator<Item = &Val> {
        self.vec.iter()
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

    fn swap(&mut self, idx_a: u32, idx_b: u32) {
        if (idx_a as usize) > self.vec.len() {
            panic!("RTE OOB");
        }
        if (idx_b as usize) > self.vec.len() {
            panic!("RTE OOB");
        }

        if idx_a != idx_b {
            self.vec.swap((idx_a - 1) as usize, (idx_b - 1) as usize);
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

        //rt_val_drop(old_val);
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
        self.map
            .as_mut()
            .unwrap()
            .get_mut(key)
            .unwrap()
            .update_val(new_val);
    }

    pub fn var_get(&mut self, _var: &RtString) -> Val {
        unimplemented!("list var get")
    }

    pub fn var_set(&mut self, _var: &RtString, _val: Val) {
        unimplemented!("list var set")
    }

    extern "C" fn proc_add(args: *const ProcPack, rt: &mut Runtime, out: *mut Val) {
        let args = unsafe { &*args };
        let (mut list_ptr, _) = ensure_list(args.src, rt);
        let list = unsafe { list_ptr.as_mut() };

        assert_eq!(args.named.as_slice().len(), 0);
        // TODO: copy assoc too
        for arg in args.unnamed.as_slice().iter() {
            match arg {
                Val::Ref(rr) => {
                    let ty_id = rr.vptr.id;
                    // TODO: maybe put spec in vtable?
                    let spec = rt.env.get_type_spec(ty_id);
                    if spec != Specialization::List {
                        list.push(*arg);
                        continue;
                    }

                    let sublist_ptr = rr.ptr.cast();
                    if sublist_ptr.as_ptr() == (list as *mut List) {
                        // if same list, we need to clone the elems first
                        // no need to handle assoc when copying elf
                        for elem in list.iter().cloned().collect::<Vec<_>>() {
                            list.push(elem);
                        }
                    } else {
                        // not same list, can just iter
                        let sublist = unsafe { sublist_ptr.as_ref() };
                        for elem in sublist.iter() {
                            list.push(*elem);
                        }
                        if sublist.map.is_some() {
                            // could be sped up
                            for elem in sublist.iter() {
                                list.update_index_assoc(elem, sublist.index_assoc(elem));
                            }
                        }
                    };
                },
                o => list.push(*o),
            }
        }
        unsafe { *out = Val::Null };
    }

    extern "C" fn proc_copy(args: *const ProcPack, rt: &mut Runtime, out: *mut Val) {
        let args = unsafe { &*args };
        let (mut list_ptr, vptr) = ensure_list(args.src, rt);
        let list = unsafe { list_ptr.as_mut() };

        let start = args.get(0).and_then(|v| v.try_cast_int()).unwrap_or(1);
        let end = args.get(1).and_then(|v| v.try_cast_int()).unwrap_or(0);
        assert!(start >= 0);
        assert!(end >= 0);

        if start == 1 && end == 0 {
            // easy case, clone the entire list
            let newlist_ptr =
                NonNull::new(List::on_gc_heap(rt, list.clone()) as *mut List).unwrap();
            unsafe { *out = Val::Ref(RttiRef::new(vptr, newlist_ptr.cast())) };
            return;
        }

        let newlist = List::on_gc_heap(rt, List::new());

        let range = ((start - 1) as usize)..{
            if end == 0 {
                list.len()
            } else {
                (end - 1) as usize
            }
        };

        // hack, probably does too much with internal repr..
        // TODO: optimize, wrt assoc
        newlist.vec.extend(list.vec[range].iter().cloned());
        if let Some(list_map) = &list.map {
            newlist.ensure_map();
            for (key, av) in newlist.map.as_mut().unwrap().iter_mut() {
                av.val = list_map[key].val;
            }
        }

        let newlist_ptr = NonNull::new(newlist as *mut List).unwrap();
        unsafe { *out = Val::Ref(RttiRef::new(vptr, newlist_ptr.cast())) };
    }

    extern "C" fn proc_cut(args: *const ProcPack, rt: &mut Runtime, out: *mut Val) {
        let args = unsafe { &*args };
        let (mut list_ptr, _) = ensure_list(args.src, rt);
        let list = unsafe { list_ptr.as_mut() };

        let start = args.get(0).and_then(|v| v.try_cast_int()).unwrap_or(1);
        let end = args.get(1).and_then(|v| v.try_cast_int()).unwrap_or(0);
        assert!(start >= 0);
        assert!(end >= 0);

        let range = ((start - 1) as usize)..{
            if end == 0 {
                list.len()
            } else {
                (end - 1) as usize
            }
        };

        let drain_iter = list.vec.drain(range);
        if let Some(list_map) = &mut list.map {
            for elem in drain_iter {
                if list_map.get_mut(&elem).unwrap().dec_count() {
                    list_map.remove(&elem);
                }
            }
        }

        unsafe { *out = Val::Null };
    }

    pub fn gc_iter(&self) -> GcIterator {
        GcIterator {
            state: GcIterState::Array(self.vec.iter()),
            list: self,
        }
    }
}

impl Drop for List {
    fn drop(&mut self) {
        println!("AAAAAAAAAAAAAAAAAAA list dropped!");
    }
}

#[pinion_export]
pub unsafe fn rt_list_var_get(list_ptr: NonNull<c_void>, var: StringId, out: *mut Val) {
    todo!();
    /*let list: &mut List = list_ptr.cast().as_mut();
     *out = list.var_get(var)*/
}

#[pinion_export]
pub unsafe fn rt_list_var_set(list_ptr: NonNull<c_void>, var: StringId, val: *const Val) {
    todo!();
    /*let list: &mut List = list_ptr.cast().as_mut();
    list.var_set(var, *val)*/
}

#[pinion_export]
pub unsafe fn rt_list_proc_lookup(proc: StringId, _rt: *mut Runtime) -> ProcPtr {
    todo!();
    /*match proc.as_str() {
        "Add" => ProcPtr(List::proc_add),
        "Copy" => ProcPtr(List::proc_copy),
        "Cut" => ProcPtr(List::proc_cut),
        s => panic!("RTE bad proc for list: {:?}", s),
    }*/
}

// TODO: move into val? could be ensure_datum_spec if that opts ok
// TODO: Runtime error handling
fn ensure_list(val: Val, rt: &Runtime) -> (NonNull<List>, &'static Entry) {
    match val {
        Val::Ref(rr) => {
            if rt.env.get_type_spec(rr.vptr.id) != Specialization::List {
                panic!("RTE ref but not list")
            }
            (rr.ptr.cast(), rr.vptr)
        },
        _ => panic!("RTE not list"),
    }
}

enum GcIterState<'a> {
    Array(slice::Iter<'a, Val>),
    Assoc(hash_map::Iter<'a, Val, AssocValue>),
    Done,
}

pub struct GcIterator<'a> {
    list: &'a List,
    state: GcIterState<'a>,
}

impl<'a> Iterator for GcIterator<'a> {
    type Item = &'a Val;

    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.state {
            GcIterState::Array(ref mut iter) => match iter.next() {
                None => match &self.list.map {
                    Some(map) => {
                        let mut map_iter = map.iter();
                        let next = Self::grind_assoc(&mut map_iter);
                        self.state = GcIterState::Assoc(map_iter);
                        next
                    },
                    None => {
                        self.state = GcIterState::Done;
                        None
                    },
                },
                o => o,
            },
            GcIterState::Assoc(ref mut iter) => match Self::grind_assoc(iter) {
                Some(v) => Some(v),
                None => {
                    self.state = GcIterState::Done;
                    None
                },
            },
            GcIterState::Done => None,
        }
    }
}

impl<'a> GcIterator<'a> {
    fn grind_assoc(assoc_iter: &mut hash_map::Iter<'a, Val, AssocValue>) -> Option<&'a Val> {
        loop {
            match assoc_iter.next() {
                None => return None,
                Some((_, assoc)) => {
                    if let Some(ref val) = assoc.val {
                        return Some(val);
                    }
                },
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use super::List;
    use crate::val::Val;

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
        assert_eq!(
            l.index_assoc(&Val::Float(1f32.into())),
            Val::Float(10f32.into())
        );

        l.update_index_assoc(&Val::Float(1f32.into()), Val::Float(12f32.into()));
        assert_eq!(
            l.index_assoc(&Val::Float(1f32.into())),
            Val::Float(12f32.into())
        );

        l.update_index_assoc(&Val::Float(2f32.into()), Val::Float(14f32.into()));
        assert_eq!(
            l.index_assoc(&Val::Float(1f32.into())),
            Val::Float(12f32.into())
        );
        assert_eq!(
            l.index_assoc(&Val::Float(2f32.into())),
            Val::Float(14f32.into())
        );
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
        /*let a_str = Val::String(StringId::new(1));
        let foo_str = Val::String(StringId::new(2));

        let mut l = List::new();
        l.push(a_str);
        l.push(a_str);

        l.update_index_assoc(&a_str, foo_str);
        assert_eq!(l.index_assoc(&a_str), foo_str);

        assert_eq!(l.index_int(1), a_str);
        l.update_index_int(1, a_str);
        assert_eq!(l.index_int(1), a_str);

        assert_eq!(l.index_assoc(&a_str), Val::Null);*/
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
