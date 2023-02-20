use std::collections::HashMap;
use std::fmt::Debug;

use crate::layout_ctx::LayoutId;
use crate::types::Primitive;

#[derive(Debug, Clone)]
pub enum Layout {
    Struct(StructLayout),
    Primitive(Primitive),
    Pointer(Pointer),
    Enum(Enum),
    FuncPtr(Func),
    OpaqueStruct(OpaqueLayout),
    Union(Union),
    TaggedUnion(TaggedUnion),
    // Size 0
    Unit,
    // No size
    Unsized,
}

#[derive(Debug, Clone)]
pub struct StructLayout {
    pub name: Option<&'static str>,
    pub fields: Vec<LayoutId>,
    // hashmap here is not the best! but it's fiiiine. it's all compile-time
    pub name_lookup: HashMap<&'static str, usize>,
}

impl StructLayout {
    pub fn new(name: Option<&'static str>, fields: &[(&'static str, LayoutId)]) -> Self {
        Self {
            name,
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
    pub element: LayoutId,
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
    pub name: Option<&'static str>,
    pub disc_layout: LayoutId,
    pub disc_values: Vec<u64>,
    pub disc_values_reverse: HashMap<u64, usize>,
}

#[derive(Debug, Clone)]
pub struct Union {
    pub name: Option<&'static str>,
    pub layouts: Vec<LayoutId>,

    // Used for validation, filled in by rust
    pub size: usize,
    pub alignment: usize,
}

#[derive(Debug, Clone)]
pub struct TaggedUnion {
    pub name: Option<&'static str>,

    // Must be an Enum layout
    pub tag_layout: LayoutId,
    // Must be a Union layout
    pub union_layout: LayoutId,
}

#[derive(Debug, Clone)]
pub struct OpaqueLayout {
    pub name: Option<&'static str>,
    pub size: Option<u32>,
}

#[derive(Debug, Clone)]
pub struct Func {
    pub name: &'static str,
    pub param_tys: Vec<LayoutId>,
    pub return_ty: Option<LayoutId>,
}
