use std::collections::HashMap;

use crate::types::Primitive;

// Built by generated traits, used by things like GEP machinery
// Some of this is probably.. needlessly heavy. Could do with being more memoization-friendly
// TODO: use oncecells?

#[derive(Debug, Clone)]
pub enum BasicType {
    Struct(StructLayout),
    Primitive(Primitive),
    Pointer(Pointer),
}

#[derive(Debug, Clone)]
pub struct StructLayout {
    pub fields: Vec<BasicType>,
    // hashmap here is not the best! but it's fiiiine. it's all compile-time
    name_lookup: HashMap<&'static str, usize>,
}

impl StructLayout {
    pub fn new(fields: &[(&'static str, BasicType)]) -> Self {
        Self {
            fields: fields.iter().map(|(_, ty)| ty.clone()).collect(),
            name_lookup: fields
                .iter()
                .enumerate()
                .map(|(i, (name, _))| (*name, i))
                .collect(),
        }
    }

    pub fn lookup_field(&self, field: &'static str) -> Option<&BasicType> {
        self.name_lookup.get(&field).map(|idx| &self.fields[*idx])
    }
}

#[derive(Debug, Clone)]
pub struct Pointer {
    pub element: Box<BasicType>,
    pub gc: bool,
}

impl Pointer {
    pub fn new(element: BasicType, gc: bool) -> Self {
        Self {
            element: element.into(),
            gc,
        }
    }
}
