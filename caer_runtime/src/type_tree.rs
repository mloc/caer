use crate::string_table::{StringId, StringTable};
use serde::{Serialize, Deserialize};
use indexed_vec::{IndexVec, newtype_index, Idx};

// probably should do this in compiler to not pull dreammaker into runtime?
use dreammaker::objtree;

// TODO: move all ids into their own mod?
newtype_index!(TypeId {pub idx});

#[derive(Debug, Serialize, Deserialize)]
pub struct TypeTree {
    pub types: IndexVec<TypeId, DType>,
}

impl TypeTree {
    pub fn from_objtree(objtree: &objtree::ObjectTree, st: &mut StringTable) -> Self {
        let mut tt = Self {
            types: IndexVec::new(),
        };
        tt.populate_from_objtree(objtree, st);
        tt
    }

    // TODO: move to builder struct
    fn convert_vars<'a>(ty: objtree::TypeRef<'a>, st: &mut StringTable) -> Vec<StringId> {
        let mut vars = Vec::new();
        for (name, _) in ty.vars.iter() {
            vars.push(st.put(name));
        }

        vars
    }

    // threading stringtable around here is meh. TODO: move to builder struct
    // this whole fn is meh
    fn populate_from_objtree(&mut self, objtree: &objtree::ObjectTree, st: &mut StringTable) {
        let mut open = Vec::new();
        for ty in objtree.root().children() {
            let id = TypeId::new(self.types.len());
            let mut dty = DType {
                id: id,
                path: path_to_pvec(&ty.path, st),
                parent: None,
                vars: TypeTree::convert_vars(ty, st),
            };
            dty.vars.sort_unstable();
            self.types.push(dty);
            open.push((id, ty));
        }

        while let Some((parent_id, parent_ty)) = open.pop() {
            for child_ty in parent_ty.children() {
                // ew
                let mut child_vars = TypeTree::convert_vars(child_ty, st);
                child_vars.extend_from_slice(&self.types[parent_id].vars);
                child_vars.sort_unstable();

                let child_id = TypeId::new(self.types.len());
                let dty = DType {
                    id: child_id,
                    path: path_to_pvec(&child_ty.path, st),
                    parent: Some(parent_id),
                    vars: child_vars,
                };
                assert_eq!(dty.path.len(), self.types[parent_id].path.len() + 1);

                self.types.push(dty);
                open.push((child_id, child_ty));
            }
        }
    }
}

// TODO: move into environment when env contains intern stringtable?
pub fn path_to_pvec(path_str: &str, st: &mut StringTable) -> Vec<StringId> {
    assert!(path_str.starts_with("/"));
    let mut path_vec = Vec::new();

    for seg in path_str.split("/").skip(1) {
        assert_ne!(seg, "");
        path_vec.push(st.put(seg));
    }

    path_vec
}

// arguably reinventing the wheel wrt. spacemandmm's Type type, but this is more tightly integrated
#[derive(Debug, Serialize, Deserialize)]
pub struct DType {
    pub id: TypeId,
    pub path: Vec<StringId>,
    pub parent: Option<TypeId>,
    // TODO: more detail
    pub vars: Vec<StringId>,
}
