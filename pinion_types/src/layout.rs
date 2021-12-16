use crate::types::Primitive;

// Built by generated traits, used by things like GEP machinery
// Some of this is probably.. needlessly heavy. Could do with being more memoization-friendly

#[derive(Debug, Clone)]
pub enum BasicType {
    Struct(StructLayout),
    Primitive(Primitive),
    Pointer(Pointer),
}

#[derive(Debug, Clone)]
pub struct StructLayout {
    pub fields: Vec<BasicType>,
}

impl StructLayout {
    pub fn new(fields: &[BasicType]) -> Self {
        Self {
            fields: fields.into(),
        }
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
