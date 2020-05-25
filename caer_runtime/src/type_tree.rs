use crate::string_table::{StringId, StringTable};
use serde::{Serialize, Deserialize};
use indexed_vec::{IndexVec, newtype_index, Idx};
use std::collections::{HashMap, HashSet};

// probably should do this in compiler to not pull dreammaker into runtime?
use dreammaker::objtree;

// TODO: move all ids into their own mod?
newtype_index!(TypeId {pub idx});

#[derive(Debug, Serialize, Deserialize)]
pub struct TypeTree {
    pub types: IndexVec<TypeId, DType>,
    pub type_by_path_str: HashMap<StringId, TypeId>,
    pub type_by_node_id: HashMap<u64, TypeId>,
}

impl TypeTree {
    pub fn from_objtree(objtree: &objtree::ObjectTree, st: &mut StringTable) -> Self {
        let mut tt = Self {
            types: IndexVec::new(),
            type_by_path_str: HashMap::new(),
            type_by_node_id: HashMap::new(),
        };
        tt.populate_from_objtree(objtree, st);
        tt
    }

    // TODO: move to builder struct
    fn convert_vars<'a>(prev: &[StringId], ty: objtree::TypeRef<'a>, st: &mut StringTable) -> Vec<StringId> {
        let prev_lookup: HashSet<_> = prev.iter().collect();
        let mut vars = Vec::new();
        let pty = ty.parent_type_without_root();
        for (name, _) in ty.vars.iter() {
            if name == "parent_type" || name == "vars" || name == "type" {
                continue;
            }
            let var_id = st.put(name);
            if prev_lookup.contains(&var_id) {
                continue;
            }
            vars.push(var_id);
        }

        vars
    }

    // threading stringtable around here is meh. TODO: move to builder struct
    // this whole fn is meh
    fn populate_from_objtree(&mut self, objtree: &objtree::ObjectTree, st: &mut StringTable) {
        for ty_ref in objtree.iter_types() {
            if ty_ref.index().index() == 0 {
                continue;
            }
            self.populate_ty(ty_ref, st);
        }
    }

    fn populate_ty<'a>(&mut self, oty: objtree::TypeRef<'a>, st: &mut StringTable) -> TypeId {
        if let Some(id) = self.type_by_node_id.get(&(oty.index().index() as u64)) {
            return *id;
        }

        let parent_ty = oty.parent_type_without_root().map(|tyr| self.populate_ty(tyr, st));

        let mut vars = match parent_ty {
            Some(ty_id) => self.types[ty_id].vars.clone(),
            None => Vec::new(),
        };
        vars.extend_from_slice(&TypeTree::convert_vars(&vars, oty, st));

        let id = TypeId::new(self.types.len());
        let dty = DType {
            id: id,
            path: path_to_pvec(&oty.path, st),
            path_str: st.put(&oty.path),
            parent: parent_ty,
            vars: vars,
        };

        self.type_by_node_id.insert(oty.index().index() as u64, id);
        self.type_by_path_str.insert(dty.path_str, id);
        self.types.push(dty);
        id
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
    pub path_str: StringId,
    pub path: Vec<StringId>,
    pub parent: Option<TypeId>,
    // TODO: more detail
    pub vars: Vec<StringId>,
}
