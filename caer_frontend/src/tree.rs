use super::ir_builder::IrBuilder;
use caer_types::type_tree::{self, Specialization};
use caer_types::id::{StringId, FuncId, TypeId};
use dreammaker::objtree;
use index_vec::IndexVec;
use std::collections::HashMap;
use caer_ir as ir;

/// Walks the object tree, generating a type tree and queueing work for CfgBuilder.
pub struct TreeBuilder<'a, 'ot> {
    objtree: &'ot objtree::ObjectTree,
    env: &'a mut ir::env::Env,
    procs: IndexVec<FuncId, objtree::ProcValue>,
}

impl<'a, 'ot> TreeBuilder<'a, 'ot> {
    pub fn new(objtree: &'ot objtree::ObjectTree, env: &'a mut ir::env::Env) -> Self {
        Self {
            objtree,
            env,
            procs: IndexVec::new(),
        }
    }

    pub fn build(&mut self) {
        let mut types_topsort = Vec::new();
        self.populate_ty(self.objtree.root(), &mut types_topsort);
        for ty_ref in self.objtree.iter_types() {
            self.populate_ty(ty_ref, &mut types_topsort);
        }
        for (dty_id, oty) in types_topsort.iter() {
            self.populate_vars(*dty_id, *oty);
            self.populate_procs(*dty_id, *oty);
        }

        let mut cfg_builder = IrBuilder::new(self.env, &self.procs, self.objtree);
        cfg_builder.build();
    }

    // nasty, copied from old tt builder
    // TODO: encapsulate types better
    fn populate_ty(&mut self, oty: objtree::TypeRef<'ot>, types_topsort: &mut Vec<(TypeId, objtree::TypeRef<'ot>)>) -> TypeId {
        if let Some(id) = self.env.type_tree.type_by_node_id.get(&(oty.index().index() as u64)) {
            return *id;
        }

        let parent_ty = oty.parent_type_without_root().map(|tyr| self.populate_ty(tyr, types_topsort));

        let path = self.path_to_pvec(oty);
        let mut type_path = parent_ty.map_or_else(
            || { Vec::new() },
            |id| { self.env.type_tree.types[id].type_path.clone() },
        );
        if !oty.is_root() {
            type_path.push(*path.last().unwrap());
        }

        let mut specialization = parent_ty.map_or_else(
            || { Specialization::Datum },
            |id| { self.env.type_tree.types[id].specialization },
        );
        if oty.path == "/list" {
            specialization = Specialization::List;
        }

        let id = TypeId::new(self.env.type_tree.types.len());
        let dty = type_tree::DType {
            id,
            path_str: self.env.intern_string(&oty.path),
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

        self.env.type_tree.type_by_node_id.insert(oty.index().index() as u64, id);
        self.env.type_tree.type_by_path_str.insert(dty.path_str, id);
        self.env.type_tree.types.push(dty);
        types_topsort.push((id, oty));
        id
    }

    fn populate_vars(&mut self, dty_id: TypeId, oty: objtree::TypeRef<'ot>) {
        let (mut vars, mut var_lookup) = match self.env.type_tree.types[dty_id].parent {
            Some(ty_id) => {
                let p = &self.env.type_tree.types[ty_id];
                (p.vars.clone(), p.var_lookup.clone())
            }
            None => (Vec::new(), HashMap::new()),
        };

        for (name, tv) in oty.vars.iter() {
            // metavariables we handle elsewhere
            if name == "parent_type" || name == "vars" || name == "type" {
                continue;
            }

            let var_id = self.env.intern_string(name);
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
                        Some(assoc_ty) => {
                            self.env.type_tree.type_by_node_id[&(assoc_ty.index().index() as u64)]
                        },
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

        let dty = &mut self.env.type_tree.types[dty_id];
        dty.vars = vars;
        dty.var_lookup = var_lookup;
    }

    fn populate_procs(&mut self, dty_id: TypeId, oty: objtree::TypeRef<'ot>) {
        let (mut procs, mut proc_lookup) = match self.env.type_tree.types[dty_id].parent {
            Some(ty_id) => {
                let p = &self.env.type_tree.types[ty_id];
                (p.procs.clone(), p.proc_lookup.clone())
            }
            None => (Vec::new(), HashMap::new()),
        };

        for (name, tp) in oty.procs.iter() {
            let name_id = self.env.intern_string(name);
            assert!(!tp.value.is_empty());

            let mut top_proc = None;
            for pv in tp.value.iter() {
                match &pv.code {
                    objtree::Code::Present(_) => {},
                    objtree::Code::Invalid(err) => panic!("oh no dm error {:?}", err),
                    objtree::Code::Builtin => continue,
                    objtree::Code::Disabled => panic!("woop woop procs disabled"),
                }
                let proc_id = self.env.add_proc(name_id);
                top_proc = Some(proc_id);
                assert_eq!(proc_id.index(), self.procs.len());
                self.procs.push(pv.clone());
            }

            if let Some(top) = top_proc {
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
                    }
                }
            }
        }

        let dty = &mut self.env.type_tree.types[dty_id];
        dty.procs = procs;
        dty.proc_lookup = proc_lookup;
    }

    fn path_to_pvec(&mut self, tyr: objtree::TypeRef<'ot>) -> Vec<StringId> {
        assert!(tyr.is_root() || tyr.path.starts_with("/"));
        let mut path_vec = Vec::new();

        for seg in tyr.path.split("/").skip(1) {
            assert_ne!(seg, "");
            path_vec.push(self.env.intern_string(seg));
        }

        path_vec
    }
}
