use std::collections::HashMap;
use std::fmt::Debug;

use crate::layout_ctx::LayoutId;
use crate::types::Primitive;

#[derive(Debug, Clone)]
pub enum BasicType {
    Struct(StructLayout),
    Primitive(Primitive),
    Pointer(Pointer),
    Enum(Enum),
    FuncPtr,
    OpaqueStruct(Option<u32>),
}

#[derive(Debug, Clone)]
pub struct StructLayout {
    pub fields: Vec<LayoutId>,
    // hashmap here is not the best! but it's fiiiine. it's all compile-time
    name_lookup: HashMap<&'static str, usize>,
}

impl StructLayout {
    pub fn new(fields: &[(&'static str, LayoutId)]) -> Self {
        Self {
            fields: fields.iter().map(|(_, ty)| *ty).collect(),
            name_lookup: fields
                .iter()
                .enumerate()
                .map(|(i, (name, _))| (*name, i))
                .collect(),
        }
    }

    pub fn lookup_field(&self, field: &'static str) -> Option<(usize, LayoutId)> {
        self.name_lookup
            .get(field)
            .map(|idx| (*idx, self.fields[*idx]))
    }
}

#[derive(Debug, Clone)]
pub struct Pointer {
    element: LayoutId,
    pub gc: bool,
}

impl Pointer {
    pub fn new(element: LayoutId, gc: bool) -> Self {
        Self { element, gc }
    }

    pub fn get(&self) -> LayoutId {
        self.element
    }
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub disc_width: i32,
    pub discs: Vec<u64>,
    // Maps disc value to layout. No entry if unit field
    pub field_layouts: HashMap<u64, LayoutId>,
}

#[derive(Debug, Clone)]
pub struct Func {
    pub param_tys: Vec<LayoutId>,
    pub return_ty: Option<LayoutId>,
}
