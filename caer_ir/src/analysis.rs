use std::collections::{BTreeSet, HashMap, HashSet};

use caer_infer as infer;
use caer_types::id::{FuncId, TypeId, TYPE_ID_ANY, TYPE_ID_FLOAT, TYPE_ID_REF_ANY};
use caer_types::op::BinaryOp;
use caer_types::ty;
use caer_types::type_tree::TypeTree;
use index_vec::IndexVec;
use ty::Type;

use crate::cfg::{self, Op};
use crate::const_val::ConstVal;
use crate::id::{BlockId, LocalId, VarId};
use crate::module::Module;
use crate::type_repo::TypeRepo;

// modifies a clone of the func, for now. func-level cfg opts
pub struct FuncAnalysis<'a> {
    env: &'a mut Module,
    func: cfg::Function,
    local_info: IndexVec<LocalId, LocalInfo>,
    var_info: IndexVec<VarId, VarInfo>,
}

impl<'a> FuncAnalysis<'a> {
    pub fn analyse_proc(env: &'a mut Module, func_id: FuncId) -> IndexVec<LocalId, LocalInfo> {
        let proc = env.funcs[func_id].clone();
        let initial_local_info = proc.locals.indices().map(LocalInfo::new).collect();
        let initial_var_info = proc.vars.indices().map(VarInfo::new).collect();

        let mut pa = FuncAnalysis {
            env,
            func: proc,
            local_info: initial_local_info,
            var_info: initial_var_info,
        };

        pa.do_analyse();

        pa.env.funcs.insert(func_id, pa.func);

        pa.local_info
    }

    // TODO: ERRH(an)
    fn do_analyse(&mut self) {
        let mut visited = self.func.blocks.iter().map(|_| false).collect();
        let mut postorder = Vec::new();
        self.build_orders(
            None,
            self.func.blocks.first().unwrap(),
            &mut visited,
            &mut postorder,
        );
        assert!(visited.iter().all(|b| *b));
        assert_eq!(visited.len(), postorder.len());

        for (_, block_id) in postorder.iter() {
            let block = &self.func.blocks[*block_id];
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
                    _ => {},
                }
            }

            match &block.terminator {
                cfg::Terminator::Switch {
                    discriminant,
                    branches: _,
                    default: _,
                } => {
                    self.local_info[*discriminant]
                        .dependent_ops
                        .push(RefIndex::Terminator(block.id));
                },
                _ => {},
            }
        }

        //self.binop_prop();
        self.demote_vars();
        self.infer_types(
            &postorder
                .iter()
                .map(|(_, x)| x)
                .cloned()
                .rev()
                .collect::<Vec<_>>(),
        );
    }

    fn build_orders(
        &self, pred: Option<BlockId>, block: &cfg::Block, visited: &mut IndexVec<BlockId, bool>,
        postorder: &mut Vec<(Option<BlockId>, BlockId)>,
    ) {
        if visited[block.id] {
            return;
        }
        visited[block.id] = true;

        for id in block.iter_successors() {
            self.build_orders(Some(block.id), &self.func.blocks[id], visited, postorder);
        }
        postorder.push((pred, block.id));
    }

    // forward only
    fn binop_prop(&mut self) {
        for local_info in self.local_info.iter() {
            if local_info.decl_op.is_none() {
                continue;
            }
            let op = local_info.decl_op.unwrap().fetch(&self.func).clone();
            if let cfg::Op::Binary(dst, op, lhs, rhs) = op {
                let lhs_ty = &self.func.locals[lhs].ty;
                let rhs_ty = &self.func.locals[rhs].ty;

                let mut infer_type = None;
                if lhs_ty == rhs_ty && *lhs_ty == TYPE_ID_FLOAT {
                    match op {
                        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
                            infer_type = Some(lhs_ty.clone());
                        },
                        _ => {},
                    }
                }

                if let Some(newty) = infer_type {
                    self.func.locals[dst].ty = newty;
                }
            }
        }
    }

    // very basic pass
    // if a var is only stored once, and that store is in the same scope as its decl, we can demote
    // it to an ssa local
    // this works on the assumption that a mkvar is fused to a pure store
    fn demote_vars(&mut self) {
        let mut patch = FuncPatch::new(self.func.id, self.func.blocks.len());

        let mut to_demote = Vec::new();
        for var_info in self.var_info.iter() {
            if !var_info.stores.is_empty() {
                // shouldn't be here
                // TODO: move into own pass or something
                if self.func.vars[var_info.id].ty == TYPE_ID_ANY {
                    let func = &self.func; // Captured for closure
                    let tys = var_info.stores.iter().flat_map(|idx| {
                        idx.fetch(func)
                            .source_locals()
                            .into_iter()
                            .map(|id| func.locals[id].ty)
                    });
                    let new_ty = unify_types(&mut self.env.types, tys);
                    self.func.vars[var_info.id].ty = new_ty;
                }
            }

            if var_info.stores.len() == 1 {
                println!(
                    "{:?} {:?}",
                    var_info.id, self.func.vars[var_info.id].captures
                );
                if !self.func.vars[var_info.id].captures.is_empty() {
                    // TODO: clean this way up
                    continue;
                }
                let decl_scope = if let Some(opidx) = var_info.decl_op {
                    self.func.blocks[opidx.block].scope
                } else {
                    assert!(var_info.id.index() == 0);
                    continue;
                };
                // safety net for return var and params
                // not needed?
                if decl_scope == self.func.global_scope {
                    continue;
                }
                let store_scope = self.func.blocks[var_info.stores[0].block].scope;
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
                let op = opidx.fetch(&self.func);
                if let Op::Store(v, l) = op {
                    assert_eq!(*v, var_id);
                    assert_eq!(
                        self.func.locals[*l].construct_scope,
                        self.func.vars[*v].scope
                    );
                    *l
                } else {
                    panic!("store opidx isn't a store op? {:?}", op);
                }
            };

            for load_idx in self.var_info[var_id].loads.iter() {
                patch.remove_op(*load_idx);
                let dest_l = if let Op::Load(l, v) = load_idx.fetch(&self.func) {
                    assert_eq!(*v, var_id);
                    *l
                } else {
                    panic!("load opidx isn't a load op?");
                };
                patch.subst_local(dest_l, equiv_local);
            }
        }

        patch.normalize();
        patch.resolve_ops(&mut self.func);
        patch.rewrite_locals(&mut self.func, &patch.subst_locals);
        patch.gc_locals(&mut self.func);
        patch.gc_vars(&mut self.func);
    }

    fn infer_types(&mut self, order: &[BlockId]) {
        println!("RUNNING type inference for {:?}", self.func.id);
        let mut runner = InferRunner::create(
            &mut self.env.types,
            &self.env.type_tree,
            &mut self.func,
            order.to_owned(),
        );
        runner.run(&mut self.func);
    }

    fn fold_consts(&mut self, _order: &[BlockId]) -> bool {
        let changed = false;
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

    /*fn val_to_lit(val: Val) -> cfg::Literal {
        match val {
            Val::Null => cfg::Literal::Null,
            Val::Float(n) => cfg::Literal::Num(n.into()),
            Val::String(id) => cfg::Literal::String(id),
            _ => unimplemented!("other val {:?}", val),
        }
    }*/
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RefIndex {
    Op(OpIndex),
    Terminator(BlockId),
}

impl RefIndex {
    // nasty but handy
    fn fetch(self, func: &cfg::Function) -> &cfg::Op {
        match self {
            RefIndex::Op(idx) => &func.blocks[idx.block].ops[idx.op_offset],
            RefIndex::Terminator(_) => panic!(),
        }
    }

    fn fetch_mut(self, func: &mut cfg::Function) -> &mut cfg::Op {
        match self {
            RefIndex::Op(idx) => &mut func.blocks[idx.block].ops[idx.op_offset],
            RefIndex::Terminator(_) => panic!(),
        }
    }
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

    fn fetch(self, func: &cfg::Function) -> &cfg::Op {
        RefIndex::from(self).fetch(func)
    }

    fn fetch_mut(self, func: &mut cfg::Function) -> &mut cfg::Op {
        RefIndex::from(self).fetch_mut(func)
    }
}

impl From<OpIndex> for RefIndex {
    fn from(val: OpIndex) -> Self {
        RefIndex::Op(val)
    }
}

#[derive(Debug)]
pub struct LocalInfo {
    id: LocalId,

    dependent_ops: Vec<RefIndex>,

    decl_op: Option<OpIndex>, // block index, op index
    const_val: Option<ConstVal>,
}

impl LocalInfo {
    /// Create an empty info struct for a given local
    fn new(id: LocalId) -> Self {
        Self {
            id,
            dependent_ops: Vec::new(),
            decl_op: None,
            const_val: None,
        }
    }

    fn const_from_lit(&mut self, lit: &cfg::Literal) {
        self.const_val = Some(match lit {
            cfg::Literal::Null => ConstVal::Null,
            cfg::Literal::Num(n) => ConstVal::Float((*n).into()),
            cfg::Literal::String(id) => ConstVal::String(*id),
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
            id,
            decl_op: None,

            loads: Vec::new(),
            stores: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
struct InferCheckpoint {
    block: usize,
    op: usize,
    snapshot_idx: usize,
    interest_idx: usize,
    attempt_idx: usize,
    on_fail: Vec<infer::Rule>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum InferRef {
    Local(LocalId),
    Var(VarId),
}

struct InferRunner<'ty> {
    types: &'ty mut TypeRepo,
    type_tree: &'ty TypeTree,

    // reverse postorder of the CFG
    block_order: Vec<BlockId>,

    engine: infer::InferEngine,
    local_ikey: IndexVec<LocalId, infer::InferKey>,
    var_ikey: IndexVec<VarId, infer::InferKey>,
    key_to_ref: IndexVec<infer::InferKey, InferRef>,

    checkpoints: Vec<InferCheckpoint>,
    interest: Vec<(usize, usize, cfg::Op)>,
    overload_attempted: Vec<(usize, usize)>,
}

impl<'ty> InferRunner<'ty> {
    fn create(
        types: &'ty mut TypeRepo, type_tree: &'ty TypeTree, proc: &mut cfg::Function,
        block_order: Vec<BlockId>,
    ) -> Self {
        let mut engine = infer::InferEngine::new();

        let local_ikey: IndexVec<_, _> = proc.locals.indices().map(|_| engine.add_var()).collect();
        let var_ikey: IndexVec<_, _> = proc.vars.indices().map(|_| engine.add_var()).collect();
        let key_to_ref: IndexVec<_, _> = local_ikey
            .indices()
            .map(InferRef::Local)
            .chain(var_ikey.indices().map(InferRef::Var))
            .collect();

        for (lid, k) in local_ikey.iter_enumerated() {
            assert_eq!(key_to_ref[*k], InferRef::Local(lid))
        }
        for (vid, k) in var_ikey.iter_enumerated() {
            assert_eq!(key_to_ref[*k], InferRef::Var(vid))
        }

        Self {
            types,
            type_tree,
            block_order,
            engine,
            local_ikey,
            var_ikey,
            key_to_ref,
            checkpoints: Vec::new(),
            interest: Vec::new(),
            overload_attempted: Vec::new(),
        }
    }

    fn build_subs(&mut self, proc: &cfg::Function) {
        let mut subs = Vec::new();

        // order doesn't matter
        for block in proc.blocks.iter() {
            for op in block.ops.iter() {
                match op {
                    Op::Store(var, local) => {
                        subs.push((self.local_ikey[*local], self.var_ikey[*var]));
                    },
                    _ => {},
                }
            }
        }

        self.engine.register_subs(subs).unwrap();
    }

    fn apply_rule(&mut self, rule: &infer::Rule) -> Result<(), infer::InferUnifyError> {
        self.engine.process_rule(rule)
    }

    fn checkpoint(&mut self, block: usize, op: usize, on_fail: Vec<infer::Rule>) {
        let snapshot_idx = self.engine.snapshot();
        self.checkpoints.push(InferCheckpoint {
            block,
            op,
            snapshot_idx,
            interest_idx: self.interest.len(),
            attempt_idx: self.overload_attempted.len(),
            on_fail,
        })
    }

    fn rollback_to(&mut self, checkpoint: &InferCheckpoint) -> Result<(), infer::InferUnifyError> {
        self.engine.rollback_snapshot(checkpoint.snapshot_idx);
        self.interest.truncate(checkpoint.interest_idx);
        self.overload_attempted.truncate(checkpoint.attempt_idx);
        for rule in checkpoint.on_fail.iter() {
            println!("applying {:?} due to rollback", rule);
            self.apply_rule(rule)?;
        }
        Ok(())
    }

    fn commit_all(&mut self) {
        for checkpoint in self.checkpoints.drain(..).rev() {
            self.engine.commit_snapshot(checkpoint.snapshot_idx);
        }
    }

    fn run(&mut self, proc: &mut cfg::Function) {
        self.build_subs(proc);
        let mut start_block = 0;
        let mut start_op = 0;
        loop {
            match self.run_types_from(proc, start_block, start_op) {
                Ok(_) => break,
                Err(err) => match self.checkpoints.pop() {
                    Some(checkpoint) => {
                        println!("backtracking due to {}", err);
                        self.rollback_to(&checkpoint).unwrap();
                        start_block = checkpoint.block;
                        start_op = checkpoint.op;
                    },
                    None => {
                        panic!("out of checkpoints, still erroring");
                    },
                },
            }
        }
        self.commit_all();
        for (block, op, rep_op) in self.interest.drain(..) {
            proc.blocks[self.block_order[block]].ops[op] = rep_op;
        }
        for (k, assign) in self.engine.resolve_assignments().into_iter_enumerated() {
            match self.key_to_ref[k] {
                InferRef::Local(lid) => {
                    proc.locals[lid].ty = self.types.insert_sum(assign.as_tys())
                },
                InferRef::Var(vid) => proc.vars[vid].ty = self.types.insert_sum(assign.as_tys()),
            }
        }
    }

    fn run_types_from(
        &mut self, proc: &mut cfg::Function, start_block: usize, start_op: usize,
    ) -> Result<(), infer::InferUnifyError> {
        for block_idx in start_block..self.block_order.len() {
            let block_id = self.block_order[block_idx];
            let block = &proc.blocks[block_id];
            let op_range = if block_idx == start_block {
                start_op..block.ops.len()
            } else {
                0..block.ops.len()
            };
            for op_idx in op_range {
                self.process_op(&block.ops[op_idx], block_idx, op_idx)?;
            }
        }
        self.engine.propogate_subs()?;
        Ok(())
    }

    fn process_op(
        &mut self, op: &cfg::Op, block_idx: usize, op_idx: usize,
    ) -> Result<(), infer::InferUnifyError> {
        match op {
            Op::Noop => {},
            Op::Literal(local, lit) => {
                let lit_ty = self.types.insert(lit.get_ty());
                self.apply_rule(&infer::Rule::Const(self.local_ikey[*local], lit_ty))?;
            },
            Op::MkVar(_) => {}, // TODO: consider better types for vars
            Op::Load(local, var) => {
                self.apply_rule(&infer::Rule::Equals(
                    self.local_ikey[*local],
                    self.var_ikey[*var],
                ))?;
            },
            Op::Store(_, _) => {
                // handled by sub rule
            },
            Op::Put(_) => {},
            Op::Call(out_l, _, _) => {
                self.apply_rule(&infer::Rule::Const(self.local_ikey[*out_l], TYPE_ID_ANY))?;
            },
            Op::Cast(out_l, _, ty) => {
                self.apply_rule(&infer::Rule::Const(self.local_ikey[*out_l], (*ty).into()))?;
            },
            Op::AllocDatum(out_l, _) => {
                // TODO: handle datum types
                self.apply_rule(&infer::Rule::Const(
                    self.local_ikey[*out_l],
                    TYPE_ID_REF_ANY,
                ))?;
            },
            Op::DatumLoadVar(out_l, datum_l, _) => {
                // TODO: in future, with hard datums, we can set out_l's ty
                self.apply_rule(&infer::Rule::Const(self.local_ikey[*out_l], TYPE_ID_ANY))?;
                self.apply_rule(&infer::Rule::Const(
                    self.local_ikey[*datum_l],
                    TYPE_ID_REF_ANY,
                ))?;
            },
            Op::DatumStoreVar(datum_l, _, _) => {
                self.apply_rule(&infer::Rule::Const(
                    self.local_ikey[*datum_l],
                    TYPE_ID_REF_ANY,
                ))?;
            },
            Op::DatumCallProc(out_l, datum_l, _, _) => {
                self.apply_rule(&infer::Rule::Const(self.local_ikey[*out_l], TYPE_ID_ANY))?;
                self.apply_rule(&infer::Rule::Const(
                    self.local_ikey[*datum_l],
                    TYPE_ID_REF_ANY,
                ))?;
            },
            Op::Throw(_) => {},
            Op::CatchException(maybe_except_v) => {
                if let Some(except_v) = maybe_except_v {
                    self.apply_rule(&infer::Rule::Const(self.var_ikey[*except_v], TYPE_ID_ANY))?;
                }
            },
            Op::Spawn(_, _) => {},
            Op::Sleep(_) => {},

            // flow cases
            Op::Binary(out_l, binop, lhs_l, rhs_l) => {
                self.engine.propogate_subs()?;
                let mut hardened = false;

                let lhs_ty = self.types.insert_sum(
                    self.engine
                        .probe_assignment(self.local_ikey[*lhs_l])
                        .as_tys(),
                );
                let rhs_ty = self.types.insert_sum(
                    self.engine
                        .probe_assignment(self.local_ikey[*rhs_l])
                        .as_tys(),
                );
                if self
                    .overload_attempted
                    .last()
                    .map_or(true, |t| *t != (block_idx, op_idx))
                {
                    if let Some(hard_op) = caer_types::op::HardBinary::from_in_ty(
                        *binop,
                        (self.types.get(lhs_ty), self.types.get(rhs_ty)),
                    ) {
                        self.overload_attempted.push((block_idx, op_idx));
                        self.checkpoint(
                            block_idx,
                            op_idx,
                            vec![infer::Rule::Const(self.local_ikey[*out_l], TYPE_ID_ANY)],
                        );
                        self.interest.push((
                            block_idx,
                            op_idx,
                            cfg::Op::HardBinary(*out_l, hard_op, *lhs_l, *rhs_l),
                        ));
                        let out_ty = self.types.insert(hard_op.out_ty());
                        self.apply_rule(&infer::Rule::Const(self.local_ikey[*out_l], out_ty))?;
                        self.apply_rule(&infer::Rule::ConstFreeze(
                            self.local_ikey[*lhs_l],
                            lhs_ty,
                        ))?;
                        self.apply_rule(&infer::Rule::ConstFreeze(
                            self.local_ikey[*rhs_l],
                            rhs_ty,
                        ))?;
                        hardened = true;
                    }
                }
                if !hardened {
                    self.apply_rule(&infer::Rule::Const(self.local_ikey[*out_l], TYPE_ID_ANY))?;
                }
            },
            _ => unimplemented!("unhandled op: {:?}", op),
        };
        Ok(())
    }
}

#[derive(Debug, Clone)]
struct FuncPatch {
    // redundant, here for sanity check
    proc: FuncId,

    remove_ops: IndexVec<BlockId, Vec<usize>>,
    add_ops: IndexVec<BlockId, Vec<(usize, cfg::Op)>>,

    remove_locals: Vec<LocalId>,
    subst_locals: HashMap<LocalId, LocalId>,
}

impl FuncPatch {
    fn new(id: FuncId, n_blocks: usize) -> Self {
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
        self.add_ops
            .iter_mut()
            .for_each(|v| v.sort_by_key(|(i, _)| *i));
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

    fn resolve_ops(&self, proc: &mut cfg::Function) {
        for block in proc.blocks.iter_mut() {
            // TODO: can do this efficiently without hashing collections
            // very inefficent, loads of allocs and clones
            let remove_ops: HashSet<_> = self.remove_ops[block.id].iter().collect();
            let mut add_ops: HashMap<usize, Vec<cfg::Op>> = HashMap::new();
            for (index, op) in self.add_ops[block.id].iter() {
                let v = add_ops.entry(*index).or_default();
                v.push(op.clone());
            }

            block.ops = block
                .ops
                .iter()
                .enumerate()
                .flat_map(|(index, op)| {
                    let start: Box<dyn Iterator<Item = cfg::Op>> = if remove_ops.contains(&index) {
                        Box::new(std::iter::empty())
                    } else {
                        Box::new(std::iter::once(op.clone()))
                    };

                    let rest: Box<dyn Iterator<Item = cfg::Op>> =
                        if let Some(ops) = add_ops.get(&index) {
                            Box::new(ops.iter().cloned())
                        } else {
                            Box::new(std::iter::empty())
                        };

                    start.chain(rest)
                })
                .collect();
        }
    }

    fn gc_locals(&self, proc: &mut cfg::Function) {
        let mut unused_locals: IndexVec<_, _> = proc.locals.iter().map(|_| true).collect();
        let mut visit_fn = |id: &mut _| {
            unused_locals[*id] = false;
        };

        for block in proc.blocks.iter_mut() {
            for op in block.ops.iter_mut() {
                op.visit_dest(&mut visit_fn);
                op.visit_source(&mut visit_fn);
            }
            block.terminator.visit_local(&mut visit_fn);
        }

        let mut remap = HashMap::new();
        proc.locals.retain(|l| !unused_locals[l.id]);
        proc.locals.iter_mut_enumerated().for_each(|(id, l)| {
            remap.insert(l.id, id);
            l.id = id;
        });

        self.rewrite_locals(proc, &remap);
    }

    fn gc_vars(&self, proc: &mut cfg::Function) {
        let mut unused_vars: IndexVec<_, _> = proc.vars.iter().map(|_| true).collect();
        // TODO: better handling for ret var
        unused_vars[0] = false;
        let mut visit_fn = |id: &mut _| {
            unused_vars[*id] = false;
        };

        for block in proc.blocks.iter_mut() {
            for op in block.ops.iter_mut() {
                op.visit_var(&mut visit_fn);
            }
        }
        proc.params.iter_mut().for_each(visit_fn);

        let mut remap = HashMap::new();
        proc.vars.retain(|l| !unused_vars[l.id]);
        proc.vars.iter_mut_enumerated().for_each(|(id, v)| {
            remap.insert(v.id, id);
            v.id = id;
        });

        self.rewrite_vars(proc, &remap);
    }

    fn rewrite_locals(
        &self, proc: &mut cfg::Function, map: impl caer_util::traits::Map<LocalId, LocalId>,
    ) {
        let map_fn = |old: &_| {
            if let Some(new) = map.map_get(old) {
                *new
            } else {
                *old
            }
        };

        let visit_fn = |old: &mut _| {
            *old = map_fn(old);
        };

        for block in proc.blocks.iter_mut() {
            for op in block.ops.iter_mut() {
                op.visit_dest(visit_fn);
                op.visit_source(visit_fn);
            }
            block.terminator.visit_local(visit_fn);
        }

        for scope in proc.scopes.iter_mut() {
            scope.locals.iter_mut().for_each(visit_fn);
            scope.locals.sort_unstable();
            scope.locals.dedup();
            scope.destruct_locals = scope.destruct_locals.iter().map(map_fn).collect();
        }
    }

    // bad copypaste, TODO: nicer visitor
    fn rewrite_vars(
        &self, proc: &mut cfg::Function, map: impl caer_util::traits::Map<VarId, VarId>,
    ) {
        let map_fn = |old: &_| {
            if let Some(new) = map.map_get(old) {
                *new
            } else {
                *old
            }
        };

        let visit_fn = |old: &mut _| {
            *old = map_fn(old);
        };

        for block in proc.blocks.iter_mut() {
            for op in block.ops.iter_mut() {
                op.visit_var(visit_fn);
            }
        }

        for scope in proc.scopes.iter_mut() {
            scope.vars.iter_mut().for_each(visit_fn);
            scope.vars.sort_unstable();
            scope.vars.dedup();
            scope.destruct_vars = scope.destruct_vars.iter().map(map_fn).collect();
            scope
                .vars_by_name
                .iter_mut()
                .map(|(_, v)| v)
                .for_each(visit_fn);
        }

        proc.vars_by_name
            .iter_mut()
            .map(|(_, v)| v)
            .for_each(visit_fn);
        proc.params.iter_mut().for_each(visit_fn);
        proc.params.sort_unstable();
        // do a check instead, dups are bad
        //scope.vars.dedup();
    }
}

pub fn unify_types(repo: &mut TypeRepo, iter: impl Iterator<Item = TypeId>) -> TypeId {
    let mut set = BTreeSet::new();

    for id in iter {
        match repo.get(id) {
            Type::Any => return id,
            Type::OneOf(oset) => {
                set.extend(oset.iter().cloned());
            },
            _ => {
                // TODO: intern types, use id
                set.insert(id);
            },
        }
    }

    match set.len() {
        0 => panic!("oneof with no types?"),
        1 => set.into_iter().next().unwrap(),
        _ => repo.insert_sum(set),
    }
}
