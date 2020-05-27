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

    // threading stringtable around here is meh. TODO: move to builder struct
    // this whole fn is meh
    fn populate_from_objtree(&mut self, objtree: &objtree::ObjectTree, st: &mut StringTable) {
        let mut var_queue = Vec::new();
        for ty_ref in objtree.iter_types() {
            if ty_ref.index().index() == 0 {
                continue;
            }
            self.populate_ty(ty_ref, st, &mut var_queue);
        }
        self.populate_ty(objtree.root(), st, &mut var_queue);
        for (id, oty) in var_queue {
            self.populate_ty_vars(id, oty, objtree, st);
        }
    }

    fn populate_ty<'ot>(&mut self, oty: objtree::TypeRef<'ot>, st: &mut StringTable, var_queue: &mut Vec<(TypeId, objtree::TypeRef<'ot>)>) -> TypeId {
        if let Some(id) = self.type_by_node_id.get(&(oty.index().index() as u64)) {
            return *id;
        }

        let parent_ty = oty.parent_type_without_root().map(|tyr| self.populate_ty(tyr, st, var_queue));

        let path = path_to_pvec(oty, st);
        let mut type_path = parent_ty.map_or_else(
            || { Vec::new() },
            |id| { self.types[id].type_path.clone() },
        );
        if !oty.is_root() {
            type_path.push(*path.last().unwrap());
        }

        let id = TypeId::new(self.types.len());
        let dty = DType {
            id: id,
            path_str: st.put(&oty.path),
            path: path,
            type_path: type_path,
            parent: parent_ty,
            vars: Vec::new(),
            var_lookup: HashMap::new(),
        };

        self.type_by_node_id.insert(oty.index().index() as u64, id);
        self.type_by_path_str.insert(dty.path_str, id);
        self.types.push(dty);
        var_queue.push((id, oty));
        id
    }

    fn populate_ty_vars<'ot>(&mut self, dty_id: TypeId, oty: objtree::TypeRef<'ot>, objtree: &'ot objtree::ObjectTree, st: &mut StringTable) {
        let (mut vars, mut var_lookup) = match self.types[dty_id].parent {
            Some(ty_id) => {
                let p = &self.types[ty_id];
                (p.vars.clone(), p.var_lookup.clone())
            }
            None => (Vec::new(), HashMap::new()),
        };

        for (name, tv) in oty.vars.iter() {
            // metavariables we handle elsewhere
            if name == "parent_type" || name == "vars" || name == "type" {
                continue;
            }
            let var_id = st.put(name);
            if var_lookup.contains_key(&var_id) {
                // TODO: this is an error, we can't redef a var.
                // unfortunately, we need to ignore it for now, since spacemandmm gives us some
                // redefs by default. needs SDMM change
                continue;
            }
            vars.push(var_id);

            let mut var_info = VarInfo {
                assoc_dty: None,
            };

            if let Some(decl) = &tv.declaration {
                if !decl.var_type.type_path.is_empty() {
                    let assoc_dty_id = match &objtree.type_by_path(&decl.var_type.type_path) {
                        Some(assoc_ty) => {
                            self.type_by_node_id[&(assoc_ty.index().index() as u64)]
                        },
                        None => panic!("no such type {:?}", decl.var_type.type_path),
                    };
                    var_info.assoc_dty = Some(assoc_dty_id);
                }
            }

            var_lookup.insert(var_id, var_info);
        }

        let dty = &mut self.types[dty_id];
        dty.vars = vars;
        dty.var_lookup = var_lookup;
    }
}

// TODO: move into environment when env contains intern stringtable?
pub fn path_to_pvec<'ot>(tyr: objtree::TypeRef<'ot>, st: &mut StringTable) -> Vec<StringId> {
    assert!(tyr.is_root() || tyr.path.starts_with("/"));
    let mut path_vec = Vec::new();

    for seg in tyr.path.split("/").skip(1) {
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
    pub type_path: Vec<StringId>,
    pub parent: Option<TypeId>,
    // TODO: more detail
    pub vars: Vec<StringId>,
    pub var_lookup: HashMap<StringId, VarInfo>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VarInfo {
    // TODO: just a dty for now; could maybe be a fully-blown prim or complex type
    pub assoc_dty: Option<TypeId>,
}
