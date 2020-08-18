use crate::id::BlockId;
use crate::cfg;
use index_vec::IndexVec;

// TODO: rename
pub trait WalkActor {
    fn pre_start(&mut self, walker: &CFGWalker);
    fn block_start(&mut self, walker: &CFGWalker, id: BlockId);
    fn block_end(&mut self, walker: &CFGWalker, id: BlockId);
    fn handle_op(&mut self, walker: &CFGWalker, id: BlockId, op: &cfg::Op, idx: usize);
    fn end(&mut self, walker: &CFGWalker);
}

#[derive(Debug)]
pub struct CFGWalker {
    rev_postorder: Vec<BlockId>,
    preds: IndexVec<BlockId, Option<BlockId>>,
}

impl<'a> CFGWalker {
    pub fn build(proc: &'a cfg::Proc) -> Self {
        let (postorder, preds) = Self::build_postorder(proc);
        let rev_postorder = {
            let mut v = postorder.clone();
            v.reverse();
            v
        };

        Self {
            rev_postorder,
            preds,
        }
    }

    pub fn walk(mut self, proc: &'a cfg::Proc, actor: &mut impl WalkActor) {
        actor.pre_start(&mut self);
        for (id, block) in proc.blocks.iter_enumerated() {
            actor.block_start(&mut self, id);
            for (i, op) in block.ops.iter().enumerate() {
                actor.handle_op(&mut self, id, op, i);
            }
            actor.block_end(&mut self, id);
        }
        actor.end(&mut self);
    }

    fn build_postorder(proc: &'a cfg::Proc) -> (Vec<BlockId>, IndexVec<BlockId, Option<BlockId>>) {
        let mut postorder = Vec::new();
        let mut preds = IndexVec::new();
        preds.resize(proc.blocks.len(), None);

        let mut visited: IndexVec<BlockId, _> = IndexVec::new();
        visited.resize(proc.blocks.len(), false);
        fn rec(cur: BlockId, pred: Option<BlockId>, proc: &cfg::Proc, postorder: &mut Vec<BlockId>, preds: &mut IndexVec<BlockId, Option<BlockId>>, visited: &mut IndexVec<BlockId, bool>) {
            if visited[cur] {
                return;
            }

            preds[cur] = pred;
            visited[cur] = true;
            for succ in proc.blocks[cur].iter_successors() {
                rec(succ, Some(cur), proc, postorder, preds, visited);
            }
            postorder.push(cur);
        }
        rec(BlockId::new(0), None, proc, &mut postorder, &mut preds, &mut visited);

        (postorder, preds)
    }

    pub fn get_predecessor(&self, id: BlockId) -> Option<BlockId> {
        self.preds[id]
    }
}
