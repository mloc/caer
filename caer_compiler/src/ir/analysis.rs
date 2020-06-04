use super::cfg::{self, Op};
use super::id::*;
use super::env::Env;

use caer_runtime::val::Val;
use caer_runtime::environment::ProcId;

use index_vec::IndexVec;

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

        let mut ops_to_remove = Vec::new();

        for var_id in to_demote {
            println!("DEMOTING {:?}", var_id);
            ops_to_remove.push(self.var_info[var_id].decl_op.unwrap());

            let equiv_local = {
                let opidx = self.var_info[var_id].stores[0];
                ops_to_remove.push(opidx);
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
                ops_to_remove.push(*load_idx);
                let dest_l = if let Op::Load(l, v) = self.get_op(*load_idx) {
                    assert_eq!(*v, var_id);
                    *l
                } else {
                    panic!("load opidx isn't a load op?");
                };

                for dep_ref in self.local_info[dest_l].dependent_ops.iter() {
                    match dep_ref {
                        RefIndex::Op(opidx) => {
                            let op = &mut self.proc.blocks[opidx.block].ops[opidx.op_offset];
                            op.subst_source(dest_l, equiv_local);
                        },
                        RefIndex::Terminator(block_id) => {
                            self.proc.blocks[*block_id].terminator.subst_local(dest_l, equiv_local);
                        },
                    }
                }
            }
        }

        for opidx in ops_to_remove {
            *self.get_op_mut(opidx) = Op::Noop;
        }
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
