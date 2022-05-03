use std::collections::HashMap;

use caer_types::id::PathTypeId;
use dreammaker::ast::PathOp;
use dreammaker::objtree::{self, NodeIndex, TypeRef};

/// Encapsulated objtree + bookkeeping info for mapping nodes to ptys
pub struct ObjtreeWrapper<'a> {
    objtree: &'a objtree::ObjectTree,
    mapping: HashMap<NodeIndex, PathTypeId>,
}

impl<'a> ObjtreeWrapper<'a> {
    pub fn from_mapping(
        objtree: &'a objtree::ObjectTree, mapping: HashMap<NodeIndex, PathTypeId>,
    ) -> Self {
        Self { objtree, mapping }
    }

    // Nasty copypasta. TODO: native navigation?
    pub fn navigate_path<S>(&self, pieces: &'a [(PathOp, S)]) -> Option<TypeRef>
    where
        S: AsRef<str>,
    {
        self.objtree.root().navigate_path(pieces).map(|r| r.ty())
    }

    pub fn resolve_path<I>(&self, path_components: I) -> Option<TypeRef>
    where
        I: IntoIterator,
        I::Item: AsRef<str>,
    {
        self.objtree.type_by_path(path_components)
    }

    pub fn lookup_type(&self, ty: TypeRef) -> Option<PathTypeId> {
        self.mapping.get(&ty.index()).copied()
    }

    pub fn type_by_path<I>(&self, path: I) -> Option<PathTypeId>
    where
        I: IntoIterator,
        I::Item: AsRef<str>,
    {
        self.objtree
            .type_by_path(path)
            .map(|ty| self.mapping[&ty.index()])
    }
}
