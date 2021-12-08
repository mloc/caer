use std::collections::HashMap;

use caer_ir as ir;
use caer_types::id::{
    FuncId, InstanceTypeId, PathTypeId, StringId, INSTANCE_TYPE_ID_LIST, INSTANCE_TYPE_ID_STRING,
};
use caer_types::instance::{InstanceType, InstanceTypes};
use caer_types::type_tree::{self, PathType, Specialization, TypeTree};
use dreammaker::objtree::{self, NodeIndex};
use index_vec::IndexVec;
use ir::string::StringTable;

use crate::objtree_wrapper::ObjtreeWrapper;
use crate::proc_builder::{BuiltinProc, ProcBody};

pub struct TreeBundle<'a> {
    pub type_tree: TypeTree,
    pub instances: InstanceTypes,
    pub objtree: ObjtreeWrapper<'a>,
    pub funcs: IndexVec<FuncId, (ProcBody, Option<FuncId>)>,
}

/// Walks the object tree, generating a type tree and queueing work for CfgBuilder.
pub struct TreeBuilder<'a, 's> {
    objtree: &'a objtree::ObjectTree,
    strings: &'s mut StringTable,
    funcs: IndexVec<FuncId, (ProcBody, Option<FuncId>)>,

    types: IndexVec<PathTypeId, PathType>,

    ty_mapping: HashMap<NodeIndex, PathTypeId>,
}

impl<'a, 's> TreeBuilder<'a, 's> {
    pub fn new(objtree: &'a objtree::ObjectTree, strings: &'s mut StringTable) -> Self {
        Self {
            objtree,
            strings,
            funcs: IndexVec::new(),
            types: IndexVec::new(),
            ty_mapping: HashMap::new(),
        }
    }

    pub fn build(mut self) -> TreeBundle<'a> {
        let mut types_topsort = Vec::new();
        self.populate_ty(self.objtree.root(), &mut types_topsort);
        for ty_ref in self.objtree.iter_types() {
            self.populate_ty(ty_ref, &mut types_topsort);
        }
        for (dty_id, oty) in types_topsort.iter() {
            self.populate_vars(*dty_id, *oty);
            self.populate_procs(*dty_id, *oty);
        }

        self.populate_builtin_types();

        let tree = TypeTree::from_parts(self.types);

        let ib = InstanceBuilder::new(&tree, self.strings);
        let instances = ib.build();
        let wrapper = ObjtreeWrapper::from_mapping(self.objtree, self.ty_mapping);

        TreeBundle {
            type_tree: tree,
            instances,
            objtree: wrapper,
            funcs: self.funcs,
        }
    }

    // nasty, copied from old tt builder
    // TODO: encapsulate types better
    fn populate_ty(
        &mut self, oty: objtree::TypeRef<'a>,
        types_topsort: &mut Vec<(PathTypeId, objtree::TypeRef<'a>)>,
    ) -> PathTypeId {
        if let Some(id) = self.ty_mapping.get(&oty.index()) {
            return *id;
        }

        let parent_ty = oty
            .parent_type_without_root()
            .map(|tyr| self.populate_ty(tyr, types_topsort));

        let path = self.path_to_pvec(oty);
        let mut type_path = parent_ty.map_or_else(Vec::new, |id| self.types[id].type_path.clone());
        if !oty.is_root() {
            type_path.push(*path.last().unwrap());
        }

        let mut specialization =
            parent_ty.map_or_else(|| Specialization::Datum, |id| self.types[id].specialization);
        if oty.path == "/list" {
            specialization = Specialization::List;
        }

        let id = self.types.next_idx();
        let dty = type_tree::PathType {
            id,
            path_str: self.strings.put(&oty.path),
            path_string: oty.path.clone(),
            path,
            type_path,
            parent: parent_ty,
            specialization,
            vars: Vec::new(),
            var_lookup: HashMap::new(),
            procs: Vec::new(),
            proc_lookup: HashMap::new(),
        };

        if oty.is_root() {
            assert_eq!(id.index(), 0);
        }

        self.ty_mapping.insert(oty.index(), id);
        self.types.push(dty);
        types_topsort.push((id, oty));
        id
    }

    fn populate_vars(&mut self, dty_id: PathTypeId, oty: objtree::TypeRef<'a>) {
        let (mut vars, mut var_lookup) = match self.types[dty_id].parent {
            Some(ty_id) => {
                let p = &self.types[ty_id];
                (p.vars.clone(), p.var_lookup.clone())
            },
            None => (Vec::new(), HashMap::new()),
        };

        for (name, tv) in oty.vars.iter() {
            // metavariables we handle elsewhere
            if name == "parent_type" || name == "vars" || name == "type" {
                continue;
            }

            let var_id = self.strings.put(name);
            if let Some(decl) = &tv.declaration {
                if var_lookup.contains_key(&var_id) {
                    panic!("ty {:?} redecls var {}", oty, name);
                }

                let mut var_info = type_tree::VarInfo {
                    name: var_id,
                    assoc_dty: None,
                };

                // TODO: handle static vars
                if !decl.var_type.type_path.is_empty() {
                    let assoc_dty_id = match &self.objtree.type_by_path(&decl.var_type.type_path) {
                        Some(assoc_ty) => self.ty_mapping[&(assoc_ty.index())],
                        None => panic!("no such type {:?}", decl.var_type.type_path),
                    };
                    var_info.assoc_dty = Some(assoc_dty_id);
                }

                vars.push(var_id);
                var_lookup.insert(var_id, var_info);
            } else {
                if !var_lookup.contains_key(&var_id) {
                    panic!("ty {:?} defs var {} before decl", oty, name);
                }
            }
        }

        let dty = &mut self.types[dty_id];
        dty.vars = vars;
        dty.var_lookup = var_lookup;
    }

    fn populate_procs(&mut self, dty_id: PathTypeId, oty: objtree::TypeRef<'a>) {
        let (mut procs, mut proc_lookup) = match self.types[dty_id].parent {
            Some(ty_id) => {
                let p = &self.types[ty_id];
                (p.procs.clone(), p.proc_lookup.clone())
            },
            None => (Vec::new(), HashMap::new()),
        };

        for (name, tp) in oty.procs.iter() {
            let name_id = self.strings.put(name);
            assert!(!tp.value.is_empty());

            let mut top_func = None;
            for pv in tp.value.iter() {
                match &pv.code {
                    objtree::Code::Present(_) => {
                        let func_id = self.funcs.next_idx();

                        let parent;
                        if let Some(id) = top_func {
                            parent = Some(id)
                        // At this point, proc_lookup hasn't been modified for this proc since it
                        // was cloned from the parent type.
                        } else if let Some(parent_proc) = proc_lookup.get(&name_id) {
                            parent = Some(parent_proc.top_proc)
                        // Root proc
                        } else {
                            parent = None
                        }

                        top_func = Some(func_id);
                        self.funcs.push((ProcBody::Ast(pv.clone()), parent));
                    },
                    objtree::Code::Invalid(err) => panic!("oh no dm error {:?}", err),
                    objtree::Code::Builtin => top_func = self.handle_builtin(oty, name, tp),
                    objtree::Code::Disabled => panic!("woop woop procs disabled"),
                }
            }

            if let Some(top) = top_func {
                match proc_lookup.get_mut(&name_id) {
                    Some(pi) => {
                        if tp.declaration.is_some() {
                            panic!("ty {:?} redecls proc {}", oty, name);
                        }
                        pi.top_proc = top;
                    },
                    None => {
                        if tp.declaration.is_none() {
                            panic!("ty {:?} defs proc {} before decl", oty, name);
                        }
                        // TODO: handle declaration
                        procs.push(name_id);
                        let pi = type_tree::ProcInfo {
                            name: name_id,
                            // wow nasty
                            top_proc: top,
                        };
                        proc_lookup.insert(name_id, pi);
                    },
                }
            }
        }

        let dty = &mut self.types[dty_id];
        dty.procs = procs;
        dty.proc_lookup = proc_lookup;
    }

    fn populate_builtin_types(&mut self) {
        self.populate_builtin_root_type("string", Specialization::String);
    }

    fn populate_builtin_root_type(
        &mut self, name: &str, specialization: Specialization,
    ) -> PathTypeId {
        let name_id = self.strings.put(name);
        let path_string = "/".to_owned() + name;
        let id = self.types.next_idx();
        let pty = PathType {
            id,
            path_str: self.strings.put(&path_string),
            path_string,
            path: vec![name_id],
            type_path: vec![name_id],
            parent: None,
            specialization,
            vars: Default::default(),
            var_lookup: Default::default(),
            procs: Default::default(),
            proc_lookup: Default::default(),
        };
        self.types.push(pty);

        id
    }

    fn handle_builtin(
        &mut self, oty: objtree::TypeRef<'_>, name: &str, _tp: &objtree::TypeProc,
    ) -> Option<FuncId> {
        let builtin = match (&oty.path as &str, name) {
            ("", "sleep") => BuiltinProc::Sleep,
            _ => return None,
        };

        let func_id = self.funcs.next_idx();
        self.funcs.push((ProcBody::Builtin(builtin), None));
        Some(func_id)
    }

    fn path_to_pvec(&mut self, tyr: objtree::TypeRef<'a>) -> Vec<StringId> {
        assert!(tyr.is_root() || tyr.path.starts_with('/'));
        let mut path_vec = Vec::new();

        for seg in tyr.path.split('/').skip(1) {
            assert_ne!(seg, "");
            path_vec.push(self.strings.put(seg));
        }

        path_vec
    }
}

struct InstanceBuilder<'a> {
    tree: &'a TypeTree,
    strings: &'a StringTable,
    populated: IndexVec<PathTypeId, bool>,
}

impl<'a> InstanceBuilder<'a> {
    fn new(tree: &'a TypeTree, strings: &'a StringTable) -> Self {
        let mut populated = IndexVec::new();
        populated.resize(tree.len(), false);
        Self {
            tree,
            strings,
            populated,
        }
    }

    fn build(mut self) -> InstanceTypes {
        let mut types = IndexVec::new();

        let string_pty = self
            .tree
            .lookup_path(self.strings.lookup("/string").unwrap())
            .unwrap();
        let list_pty = self
            .tree
            .lookup_path(self.strings.lookup("/list").unwrap())
            .unwrap();

        // meh
        types.push(self.build_instance(
            INSTANCE_TYPE_ID_STRING,
            self.tree.get_pty(string_pty).unwrap(),
        ));
        assert_eq!(types.last_idx(), INSTANCE_TYPE_ID_STRING);
        types
            .push(self.build_instance(INSTANCE_TYPE_ID_LIST, self.tree.get_pty(list_pty).unwrap()));
        assert_eq!(types.last_idx(), INSTANCE_TYPE_ID_LIST);

        for pty in self.tree.iter() {
            if self.populated[pty.id] {
                continue;
            }
            types.push(self.build_instance(types.next_idx(), pty));
        }

        InstanceTypes::new(types)
    }

    fn build_instance(&mut self, id: InstanceTypeId, pty: &PathType) -> InstanceType {
        self.populated[pty.id] = true;
        InstanceType {
            id,
            path_type: pty.id,
            pty: pty.clone(),
        }
    }
}
