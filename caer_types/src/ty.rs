use std::collections::BTreeSet;

use serde::{Deserialize, Serialize};

use crate::id::{PathTypeId, TypeId};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Serialize, Deserialize)]
pub enum Type {
    Null,

    Float,

    // Object ref
    Ref(RefType),

    // Val types. Any and OneOf require a soft-val layout
    Any,
    // btreeset is bad.
    OneOf(BTreeSet<TypeId>),
    /*Proc {
        args: Vec<PathTypeId>,
        var_args: Option<Box<PathTypeId>>,
        ret: Box<PathTypeId>,
    },*/
}

impl Type {
    pub fn get_layout(&self) -> Layout<'_> {
        match self {
            Type::Null => Layout::Scalar(ScalarLayout::Null),
            Type::Float => Layout::Scalar(ScalarLayout::Float),

            Type::Any | Type::OneOf(_) => Layout::Val,

            Type::Ref(ref_type) => ref_type.get_layout(),
        }
    }
}

impl From<RefType> for Type {
    fn from(rt: RefType) -> Self {
        Self::Ref(rt)
    }
}

// Eventually this will contain: generic args, no special casing for list/string, no special casing
// for Any?
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Serialize, Deserialize)]
pub enum RefType {
    Any,
    String,
    List(TypeId),
    Exact(PathTypeId),
    Subtype(PathTypeId),
}

impl RefType {
    pub fn get_layout(&self) -> Layout<'_> {
        match self {
            RefType::String | RefType::Exact(_) | RefType::List(_) => Layout::HardRef(self),
            _ => Layout::SoftRef,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Layout<'a> {
    Val,
    SoftRef,
    HardRef(&'a RefType),
    Scalar(ScalarLayout),
}

impl Layout<'_> {
    // BAD. TODO: remove, x64 specific
    pub fn store_size(self) -> u64 {
        match self {
            Layout::Val => 24,             // tag, (scalar OR softref)
            Layout::SoftRef => 16,         // vptr, ptr
            Layout::HardRef(_) => 8,       // ptr
            Layout::Scalar(_) => panic!(), // padded float, etc
        }
    }

    pub fn is_val(self) -> bool {
        matches!(self, Layout::Val)
    }
}

// meh, helps codegen deal with layouts
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScalarLayout {
    Null,
    Float,
}
