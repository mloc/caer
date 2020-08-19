use crate::id::{BlockId, VarId, LocalId, ScopeId};
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
pub struct CFGWalker<'a> {
    rev_postorder: Vec<BlockId>,
    preds: IndexVec<BlockId, Option<BlockId>>,
    lifetime_tracker: LifetimeTracker<'a>,
}

impl<'a> CFGWalker<'a> {
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
            lifetime_tracker: LifetimeTracker::new(proc),
        }
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

    pub fn walk(mut self, proc: &'a cfg::Proc, actor: &mut impl WalkActor) {
        actor.pre_start(&mut self);
        for id in self.rev_postorder.clone().iter().copied() {
            let block = &proc.blocks[id];

            self.lifetime_tracker.block_start(id, self.preds[id]);
            actor.block_start(&mut self, id);

            for (i, op) in block.ops.iter().enumerate() {
                actor.handle_op(&mut self, id, op, i);
                self.lifetime_tracker.handle_op(op);
            }

            actor.block_end(&mut self, id);
            self.lifetime_tracker.block_end(id);
        }
        actor.end(&mut self);
    }

    pub fn get_predecessor(&self, id: BlockId) -> Option<BlockId> {
        self.preds[id]
    }

    pub fn get_cur_lifetimes(&self) -> Lifetimes {
        self.lifetime_tracker.get_cur_lifetimes()
    }
}

#[derive(Debug)]
pub struct Lifetimes<'a> {
    pub local: &'a [LocalId],
    pub var: &'a [VarId],
}

#[derive(Debug)]
struct LifetimeTracker<'a> {
    proc: &'a cfg::Proc,
    block_end: IndexVec<BlockId, Option<(Vec<LocalId>, Vec<VarId>)>>,
    cur_lifetimes: Option<(Vec<LocalId>, Vec<VarId>)>,
    scope_jb: IndexVec<ScopeId, Option<(usize, usize)>>,
    var_live: IndexVec<VarId, bool>,
}

impl<'a> LifetimeTracker<'a> {
    fn new(proc: &'a cfg::Proc) -> Self {
        let mut block_end = IndexVec::new();
        block_end.resize(proc.blocks.len(), None);
        let mut scope_jb = IndexVec::new();
        scope_jb.resize(proc.scopes.len(), None);
        let mut var_live = IndexVec::new();
        var_live.resize(proc.vars.len(), false);

        Self {
            proc,
            block_end,
            cur_lifetimes: Default::default(),
            scope_jb,
            var_live,
        }
    }

    fn get_cur_lifetimes(&self) -> Lifetimes {
        if let Some(ref cur_lifetimes) = self.cur_lifetimes {
            Lifetimes {
                local: &cur_lifetimes.0,
                var: &cur_lifetimes.1,
            }
        } else {
            panic!();
        }
    }

    fn block_start(&mut self, id: BlockId, pred: Option<BlockId>) {
        let mut alive_locals;
        let mut alive_vars;

        // entry block is a special case, need to track+prop "global" locals/vars, but don't need
        // to worry about pred
        if id.index() == 0 {
            alive_locals = Vec::new();
            alive_vars = Vec::new();

            for local in self.proc.locals.iter() {
                if local.construct_scope == self.proc.global_scope {
                    alive_locals.push(local.id);
                }
            }
            for var in self.proc.vars.iter() {
                if var.scope == self.proc.global_scope {
                    alive_vars.push(var.id);
                }
            }
        } else {
            let pred = pred.unwrap();

            let pred_alive = self.block_end[pred].clone().unwrap();
            alive_locals = pred_alive.0;
            alive_vars = pred_alive.1;

            let pred_block = &self.proc.blocks[pred];
            let cur_block = &self.proc.blocks[id];

            let pred_scope = &self.proc.scopes[pred_block.scope];
            let cur_scope = &self.proc.scopes[cur_block.scope];

            if pred_scope.id == cur_scope.id {
                // fine, nothing needs done
            } else if cur_scope.parent == Some(pred_scope.id) {
                // add a jumpback record for the new scope
                assert!(self.scope_jb[cur_scope.id].is_none(), "scope {:?} has multiple entry points", cur_scope.id);
                self.scope_jb[cur_scope.id] = Some((alive_locals.len(), alive_vars.len()));
            } else if pred_scope.parent == Some(cur_scope.id) {
                // leaving a scope, rollback
                self.jb_rollback(pred_scope.id, &mut alive_locals, &mut alive_vars);
            } else if pred_scope.parent.is_some() && pred_scope.parent == cur_scope.parent {
                // special case, jumping between two sibling scopes. rollback then add a jb rec
                assert!(self.scope_jb[cur_scope.id].is_none(), "scope {:?} has multiple entry points", cur_scope.id);
                self.scope_jb[cur_scope.id] = self.scope_jb[pred_scope.id].clone();
                self.jb_rollback(pred_scope.id, &mut alive_locals, &mut alive_vars);
            } else {
                panic!("could not find relation between {:?} and {:?}", pred_scope.id, cur_scope.id);
            }
        }

        self.cur_lifetimes = Some((alive_locals, alive_vars));
    }

    fn jb_rollback(&mut self, scope: ScopeId, alive_locals: &mut Vec<LocalId>, alive_vars: &mut Vec<VarId>) {
        let (local_jb, var_jb) = self.scope_jb[scope].unwrap();
        alive_locals.truncate(local_jb);
        for id in alive_vars[var_jb..].iter() {
            self.var_live[*id] = false;
        }
        alive_vars.truncate(var_jb);
    }

    fn handle_op(&mut self, op: &cfg::Op) {
        if let Some(dest) = op.dest_local() {
            let alive_locals = &mut self.cur_lifetimes.as_mut().unwrap().0;
            alive_locals.push(dest);
            return
        }

        match op {
            cfg::Op::Store(dest, _) | cfg::Op::CatchException(Some(dest)) => {
                if self.var_live[*dest] {
                    return
                }
                let alive_vars = &mut self.cur_lifetimes.as_mut().unwrap().1;
                alive_vars.push(*dest);
                self.var_live[*dest] = true;
            }
            _ => {}
        }
    }

    fn block_end(&mut self, id: BlockId) {
        assert!(self.block_end[id].is_none());
        let lifetimes = std::mem::replace(&mut self.cur_lifetimes, None).unwrap();
        self.block_end[id] = Some(lifetimes);
    }
}
