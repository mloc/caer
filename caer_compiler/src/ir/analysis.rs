use super::cfg::{self, Op};
use super::id::*;
use super::env::Env;

use caer_runtime::val::Val;
use caer_runtime::environment::ProcId;

use index_vec::IndexVec;

use std::collections::{HashSet, HashMap};

// modifies a clone of the proc, for now. proc-level cfg opts
pub struct ProcAnalysis<'a> {
    env: &'a mut Env,
    proc: cfg::Proc,
    local_info: IndexVec<LocalId, LocalInfo>,
    var_info: IndexVec<VarId, VarInfo>,
}

impl<'a> ProcAnalysis<'a> {
    pub fn analyse_proc(env: &'a mut Env, proc_id: ProcId) -> IndexVec<LocalId, LocalInfo> {
        let proc = env.procs[proc_id].clone();
        let initial_local_info = proc
            .locals
            .indices()
            .map(|id| LocalInfo::new(id))
            .collect();
        let initial_var_info = proc
            .vars
            .indices()
            .map(|id| VarInfo::new(id))
            .collect();

        let mut pa = ProcAnalysis {
            env: env,
            proc: proc,
            local_info: initial_local_info,
            var_info: initial_var_info,
        };

        pa.do_analyse();

        pa.env.procs[proc_id] = pa.proc;

        pa.local_info
    }

    // TODO: ERRH(an)
    fn do_analyse(&mut self) {
        let mut visited = self.proc.blocks.iter().map(|_| false).collect();
        let mut postorder = Vec::new();
        self.build_orders(&self.proc.blocks.first().unwrap(), &mut visited, &mut postorder);
        assert!(visited.iter().all(|b| *b));
        assert_eq!(visited.len(), postorder.len());

        for block_id in postorder.iter() {
            let block = &self.proc.blocks[*block_id];
            for (i, op) in block.ops.iter().enumerate() {
                let dest = op.dest_local();
                let idx = OpIndex::new(block.id, i, dest);

                if let Some(dest) = dest {
                    let dest_info = &mut self.local_info[dest];
                    if let Some(fff) = dest_info.decl_op {
                        println!("PREV: {:?}, NEW: {:?}", fff, idx);
                    }
                    assert!(dest_info.decl_op.is_none());
                    dest_info.decl_op = Some(idx);
                }

                for src in op.source_locals() {
                    self.local_info[src].dependent_ops.push(RefIndex::Op(idx));
                }

                match op {
                    Op::MkVar(var) => {
                        let var_info = &mut self.var_info[*var];
                        assert!(var_info.decl_op.is_none());
                        var_info.decl_op = Some(idx);
                    },
                    Op::Load(_, var) => {
                        self.var_info[*var].loads.push(idx);
                    },
                    Op::Store(var, _) => {
                        self.var_info[*var].stores.push(idx);
                    },
                    _ => {}
                }
            }

            match &block.terminator {
                cfg::Terminator::Switch { discriminant, branches: _, default: _ } => {
                    self.local_info[*discriminant].dependent_ops.push(RefIndex::Terminator(block.id));
                },
                _ => {},
            }
        }

        println!("{:#?}", self.var_info);

        self.demote_vars();
    }

    // nasty but handy
    fn get_op(&self, index: impl Into<RefIndex>) -> &cfg::Op {
        match index.into() {
            RefIndex::Op(idx) => &self.proc.blocks[idx.block].ops[idx.op_offset],
            RefIndex::Terminator(_) => panic!(),
        }
    }

    fn get_op_mut(&mut self, index: impl Into<RefIndex>) -> &mut cfg::Op {
        match index.into() {
            RefIndex::Op(idx) => &mut self.proc.blocks[idx.block].ops[idx.op_offset],
            RefIndex::Terminator(_) => panic!(),
        }
    }

    fn build_orders(&self, block: &cfg::Block, visited: &mut IndexVec<BlockId, bool>, postorder: &mut Vec<BlockId>) {
        if visited[block.id] {
            return
        }
        visited[block.id] = true;

        for id in block.iter_successors() {
            self.build_orders(&self.proc.blocks[id], visited, postorder);
        }
        postorder.push(block.id);
    }

    // very basic pass
    // if a var is only stored once, and that store is in the same scope as its decl, we can demote
    // it to an ssa local
    // this works on the assumption that a mkvar is fused to a pure store
    fn demote_vars(&mut self) {
        let mut patch = ProcPatch::new(self.proc.id, self.proc.blocks.len());

        let mut to_demote = Vec::new();
        for var_info in self.var_info.iter() {
            if var_info.stores.len() == 1 {
                let decl_scope = if let Some(opidx) = var_info.decl_op {
                    self.proc.blocks[opidx.block].scope
                } else {
                    assert!(var_info.id.index() == 0);
                    continue
                };
                // safety net for return var and params
                // not needed?
                if decl_scope == self.proc.global_scope {
                    continue
                }
                let store_scope = self.proc.blocks[var_info.stores[0].block].scope;
                if decl_scope == store_scope {
                    to_demote.push(var_info.id);
                }
            }
        }

        for var_id in to_demote {
            println!("DEMOTING {:?}", var_id);
            patch.remove_op(self.var_info[var_id].decl_op.unwrap());

            let equiv_local = {
                let opidx = self.var_info[var_id].stores[0];
                patch.remove_op(opidx);
                let op = self.get_op(opidx);
                if let Op::Store(v, l) = op {
                    assert_eq!(*v, var_id);
                    assert_eq!(self.proc.locals[*l].construct_scope, self.proc.vars[*v].scope);
                    *l
                } else {
                    panic!("store opidx isn't a store op? {:?}", op);
                }
            };

            for load_idx in self.var_info[var_id].loads.iter() {
                patch.remove_op(*load_idx);
                let dest_l = if let Op::Load(l, v) = self.get_op(*load_idx) {
                    assert_eq!(*v, var_id);
                    *l
                } else {
                    panic!("load opidx isn't a load op?");
                };
                patch.subst_local(dest_l, equiv_local);
            }
        }

        patch.normalize();
        patch.resolve_ops(&mut self.proc);
        patch.repack_locals(&mut self.proc);
    }

    fn fold_consts(&mut self, order: &[BlockId]) -> bool {
        let mut changed = false;
        /*for block_id in order.iter() {
            let block = &self.proc.blocks[*block_id];
            for op in block.ops.iter() {
                if let Op::Binary(dst, op, lhs, rhs) = op {
                    if Self::resolve_const_binary(&mut self.local_info, *dst, *op, *lhs, *rhs) {
                        changed = true;
                    }
                }
            }
        }*/
        changed
    }

    /*fn resolve_const_binary(local_info: &mut IndexVec<LocalId, LocalInfo>, stdest: LocalId, op: caer_runtime::op::BinaryOp, lhs: LocalId, rhs: LocalId) -> bool {
        if local_info[dest].const_val.is_some() {
            return false
        }
        let lhs_i = &local_info[lhs];
        let rhs_i = &local_info[rhs];

        let mut const_v = None;
        if let Some(ref lhs_v) = lhs_i.const_val {
            if let Some(ref rhs_v) = rhs_i.const_val {
                const_v = Some(Val::binary_op_const(op, lhs_v, rhs_v, st));
            }
        }

        self.local_info[dest].const_val = const_v;
        const_v.is_some()
    }*/

    fn val_to_lit(val: Val) -> cfg::Literal {
        match val {
            Val::Null => cfg::Literal::Null,
            Val::Float(n) => cfg::Literal::Num(n),
            Val::String(id) => cfg::Literal::String(id),
            _ => unimplemented!("other val {:?}", val),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RefIndex {
    Op(OpIndex),
    Terminator(BlockId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct OpIndex {
    pub block: BlockId,
    pub op_offset: usize,
    pub result_local: Option<LocalId>,
}

impl OpIndex {
    fn new(block: BlockId, op_offset: usize, result_local: Option<LocalId>) -> Self {
        Self {
            block,
            op_offset,
            result_local,
        }
    }
}

impl Into<RefIndex> for OpIndex {
    fn into(self) -> RefIndex {
        RefIndex::Op(self)
    }
}

#[derive(Debug)]
pub struct LocalInfo {
    id: LocalId,

    dependent_ops: Vec<RefIndex>,

    decl_op: Option<OpIndex>, // block index, op index
    const_val: Option<caer_runtime::val::Val>,
}

impl LocalInfo {
    /// Create an empty info struct for a given local
    fn new(id: LocalId) -> Self {
        Self {
            id: id,

            dependent_ops: Vec::new(),

            decl_op: None,
            const_val: None,
        }
    }

    fn const_from_lit(&mut self, lit: &cfg::Literal) {
        self.const_val = Some(match lit {
            cfg::Literal::Null => Val::Null,
            cfg::Literal::Num(n) => Val::Float(*n),
            cfg::Literal::String(id) => Val::String(*id),
            _ => unimplemented!("{:?}", lit),
        });
    }
}

#[derive(Debug)]
pub struct VarInfo {
    id: VarId,
    decl_op: Option<OpIndex>, // MkVar op

    loads: Vec<OpIndex>,
    stores: Vec<OpIndex>,
}

impl VarInfo {
    fn new(id: VarId) -> Self {
        Self {
            id: id,
            decl_op: None,

            loads: Vec::new(),
            stores: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
struct ProcPatch {
    // redundant, here for sanity check
    proc: ProcId,

    remove_ops: IndexVec<BlockId, Vec<usize>>,
    add_ops: IndexVec<BlockId, Vec<(usize, cfg::Op)>>,

    remove_locals: Vec<LocalId>,
    subst_locals: HashMap<LocalId, LocalId>,
}

impl ProcPatch {
    fn new(id: ProcId, n_blocks: usize) -> Self {
        Self {
            proc: id,
            remove_ops: (0..n_blocks).map(|_| Vec::new()).collect(),
            add_ops: (0..n_blocks).map(|_| Vec::new()).collect(),
            remove_locals: Vec::new(),
            subst_locals: HashMap::new(),
        }
    }

    fn normalize(&mut self) {
        self.remove_ops.iter_mut().for_each(|v| v.sort_unstable());
        self.add_ops.iter_mut().for_each(|v| v.sort_by_key(|(i, _)| *i));
        self.remove_locals.sort_unstable();
    }

    fn remove_op(&mut self, idx: OpIndex) {
        self.remove_ops[idx.block].push(idx.op_offset);
    }

    fn remove_local(&mut self, id: LocalId) {
        self.remove_locals.push(id);
    }

    fn subst_local(&mut self, old: LocalId, new: LocalId) {
        self.remove_local(old);
        self.subst_locals.insert(old, new);
    }

    fn resolve_ops(&self, proc: &mut cfg::Proc) {
        for block in proc.blocks.iter_mut() {
            // TODO: can do this efficiently without hashing collections
            // very inefficent, loads of allocs and clones
            let remove_ops: HashSet<_> = self.remove_ops[block.id].iter().collect();
            let mut add_ops: HashMap<usize, Vec<cfg::Op>> = HashMap::new();
            for (index, op) in self.add_ops[block.id].iter() {
                let v = add_ops.entry(*index).or_default();
                v.push(op.clone());
            }

            block.ops = block.ops.iter().enumerate().flat_map(|(index, op)| {
                let start: Box<dyn Iterator<Item = cfg::Op>> = if remove_ops.contains(&index) {
                    Box::new(std::iter::empty())
                } else {
                    Box::new(std::iter::once(op.clone()))
                };

                let rest: Box<dyn Iterator<Item = cfg::Op>> =  if let Some(ops) = add_ops.get(&index) {
                    Box::new(ops.iter().map(|r| r.clone()))
                } else {
                    Box::new(std::iter::empty())
                };

                start.chain(rest)
            }).collect();
        }
    }

    fn repack_locals(&self, proc: &mut cfg::Proc) {
        let mut to_remove = self.remove_locals.as_slice();

        let new_n = proc.locals.len() - to_remove.len();

        let mut idmap: IndexVec<LocalId, Option<LocalId>> = (0..proc.locals.len()).map(|_| None).collect();

        let mut new_id_n = 0;
        proc.locals = proc.locals.iter_enumerated().filter_map(|(old_id, local)| {
            loop {
                match to_remove {
                    [] => break,
                    [rm_id, rest @ ..] => {
                        if *rm_id >= old_id {
                            break
                        }
                        to_remove = rest;
                    },
                }
            }
            if to_remove.first() == Some(&old_id) {
                None
            } else {
                idmap[old_id] = Some(new_id_n.into());
                let mut new_local = local.clone();
                new_local.id = new_id_n.into();
                new_id_n += 1;
                Some(new_local)
            }
        }).collect();

        for (old, new) in self.subst_locals.iter() {
            idmap[*old] = Some(idmap[*new].unwrap())
        }

        self.remap_locals(proc, idmap);
    }

    fn remap_locals(&self, proc: &mut cfg::Proc, idmap: IndexVec<LocalId, Option<LocalId>>) {
        let mut unused_locals: IndexVec<_, _> = idmap.iter().map(|_| true).collect();
        for new in idmap.iter() {
            if let Some(new) = new {
                unused_locals[*new] = false
            }
        }

        if proc.id.index() == 1 {
            println!("IDMAP: {:?}", idmap);
            println!("UNLOC: {:?}", unused_locals);
        }

        let replace = |lid: &mut LocalId| {
            *lid = idmap[*lid].unwrap()
        };
        let fm_fn = |lid: &LocalId| {
            if let Some(new) = idmap[*lid] {
                if unused_locals[new] {
                    None
                } else {
                    Some(new)
                }
            } else {
                None
            }
        };

        for block in proc.blocks.iter_mut() {
            for op in block.ops.iter_mut() {
                op.visit_dest(replace);
                op.visit_source(replace);
            }
            block.terminator.visit_local(replace);
        }

        for scope in proc.scopes.iter_mut() {
            scope.locals = scope.locals.iter().filter_map(fm_fn).collect();
            scope.locals.sort_unstable();
            scope.locals.dedup();
            scope.destruct_locals = scope.destruct_locals.iter().filter_map(fm_fn).collect();
        }
    }
}
