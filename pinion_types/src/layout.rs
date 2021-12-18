use std::collections::HashMap;
use std::fmt::Debug;
use std::ops::Deref;
use std::sync::{RwLock, RwLockReadGuard};

use crate::types::Primitive;

#[derive(Debug, Clone)]
pub enum BasicType {
    Struct(StructLayout),
    Primitive(Primitive),
    Pointer(Pointer),
    FuncPtr,
    OpaqueStruct,
}

#[derive(Debug, Clone)]
pub struct StructLayout {
    pub fields: Vec<&'static CycleCell>,
    // hashmap here is not the best! but it's fiiiine. it's all compile-time
    name_lookup: HashMap<&'static str, usize>,
}

impl StructLayout {
    pub fn new(fields: &[(&'static str, &'static CycleCell)]) -> Self {
        Self {
            fields: fields.iter().map(|(_, ty)| *ty).collect(),
            name_lookup: fields
                .iter()
                .enumerate()
                .map(|(i, (name, _))| (*name, i))
                .collect(),
        }
    }

    pub fn lookup_field(&self, field: &'static str) -> Option<(usize, &'static CycleCell)> {
        self.name_lookup
            .get(field)
            .map(|idx| (*idx, self.fields[*idx]))
    }
}

#[derive(Debug, Clone)]
pub struct Pointer {
    element: &'static CycleCell,
    pub gc: bool,
}

impl Pointer {
    pub fn new(element: &'static CycleCell, gc: bool) -> Self {
        Self { element, gc }
    }

    pub fn get(&self) -> CycleCellRef {
        self.element.get()
    }
}

#[derive(Debug)]
pub struct CycleCell {
    container: RwLock<Option<BasicType>>,
}

impl CycleCell {
    pub fn new_empty() -> Self {
        Self {
            container: RwLock::new(None),
        }
    }

    pub fn new_init(init: BasicType) -> Self {
        Self {
            container: RwLock::new(Some(init)),
        }
    }

    pub fn update(&self, ty: BasicType) {
        *self.container.write().unwrap() = Some(ty)
    }

    pub fn get(&'static self) -> CycleCellRef {
        CycleCellRef {
            inner: self.container.read().unwrap(),
        }
    }
}

pub struct CycleCellRef {
    inner: RwLockReadGuard<'static, Option<BasicType>>,
}

impl Deref for CycleCellRef {
    type Target = BasicType;

    fn deref(&self) -> &Self::Target {
        let y = self.inner.as_ref().unwrap();
        y
    }
}
