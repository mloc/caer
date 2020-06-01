use super::cfg::{self, Op};
use super::id::*;
use super::env::Env;

use caer_runtime::val::Val;

use index_vec::IndexVec;

pub struct ProcAnalysis<'a> {
    env: &'a mut Env,
    proc: &'a cfg::Proc,
    local_info: IndexVec<LocalId, LocalInfo>,
}

impl<'a> ProcAnalysis<'a> {
    pub fn analyse_proc(env: &'a mut Env, proc: &'a cfg::Proc) -> IndexVec<LocalId, LocalInfo> {
        let initial_info = proc
            .locals
            .iter_enumerated()
            .map(|(id, _)| LocalInfo::new(id))
            .collect();

        let mut pa = ProcAnalysis {
            env: env,
            proc: proc,
            local_info: initial_info,
        };

        pa.do_analyse();

        pa.local_info
    }

    // TODO: ERRH(an)
    fn do_analyse(&mut self) {
        let mut visited = self.proc.blocks.iter().map(|_| false).collect();
        let mut postorder = Vec::new();
        self.build_postorder(&self.proc.blocks.first().unwrap(), &mut visited, &mut postorder);
        assert!(visited.iter().all(|b| *b));

        for block_id in postorder.iter() {
            let block = &self.proc.blocks[*block_id];
            for (i, op) in block.ops.iter().enumerate() {
                let dest = op.dest_local();
                let idx = OpIndex::Op(block.id, i, dest);

                if let Some(dest) = dest {
                    let dest_info = &mut self.local_info[dest];
                    assert!(dest_info.decl_op.is_none());
                    dest_info.decl_op = Some(idx);
                }

                for src in op.source_locals() {
                    self.local_info[src].dependent_ops.push(idx);
                }
            }

            match &block.terminator {
                cfg::Terminator::Switch { discriminant, branches: _, default: _ } => {
                    self.local_info[*discriminant].dependent_ops.push(OpIndex::Terminator(block.id));
                },
                _ => {},
            }
        }

        while self.fold_consts(&postorder) {}
    }

    fn build_postorder(&mut self, block: &cfg::Block, visited: &mut IndexVec<BlockId, bool>, postorder: &mut Vec<BlockId>) {
        if visited[block.id] {
            return
        }
        visited[block.id] = true;
        postorder.push(block.id);

        for id in block.iter_successors() {
            self.build_postorder(&self.proc.blocks[id], visited, postorder);
        }
    }

    fn fold_consts(&mut self, order: &[BlockId]) -> bool {
        let mut changed = false;
        for block_id in order.iter() {
            let block = &self.proc.blocks[*block_id];
            for op in block.ops.iter() {
                if let Op::Binary(dst, op, lhs, rhs) = op {
                    if self.resolve_const_binary(*dst, *op, *lhs, *rhs) {
                        changed = true;
                    }
                }
            }
        }
        changed
    }

    fn resolve_const_binary(&mut self, dest: LocalId, op: caer_runtime::op::BinaryOp, lhs: LocalId, rhs: LocalId) -> bool {
        let lhs_i = &self.local_info[lhs];
        let rhs_i = &self.local_info[rhs];

        let mut const_v = None;
        if let Some(ref lhs_v) = lhs_i.const_val {
            if let Some(ref rhs_v) = rhs_i.const_val {
                const_v = Some(Val::binary_op_const(op, lhs_v, rhs_v, &mut self.env.string_table));
            }
        }

        self.local_info[dest].const_val = const_v;
        const_v.is_some()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpIndex {
    // op block, op index in block, "output" from op
    Op(BlockId, usize, Option<LocalId>),
    Terminator(BlockId),
}

pub struct LocalInfo {
    id: LocalId,

    dependent_ops: Vec<OpIndex>,

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
