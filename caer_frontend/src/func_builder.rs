// naming is hard, TODO: rename some other uses of "proc" to "func"

use caer_ir::cfg;
use caer_ir::id::{BlockId, ClosureSlotId, LocalId, ScopeId, VarId};
use caer_ir::module::Module;
use caer_types::id::{FuncId, PathTypeId, StringId, TYPE_ID_ANY};
use caer_types::ty::Type;
use dreammaker::ast;
use index_vec::IndexVec;

use crate::block_builder::BlockBuilder;
use crate::ir_builder::{FuncQueue, FuncRecipe};
use crate::objtree_wrapper::ObjtreeWrapper;

pub struct FuncBuilder<'a, 'ot> {
    pub(crate) fq: &'a mut FuncQueue<'ot>,
    pub(crate) ir: &'a mut Module,
    // TODO: can we get rid of this?
    pub(crate) objtree: &'ot ObjtreeWrapper<'ot>,
    pub(crate) func: &'a mut cfg::Function,

    next_block_id: usize,
    closure_slots: IndexVec<ClosureSlotId, (ScopeId, &'ot ast::Block)>,
    finished_blocks: Vec<cfg::Block>,
}

impl<'a, 'ot> FuncBuilder<'a, 'ot> {
    pub(crate) fn for_proc(
        fq: &'a mut FuncQueue<'ot>, ir: &'a mut Module, objtree: &'ot ObjtreeWrapper<'ot>,
        func: &'a mut cfg::Function,
    ) -> Self {
        Self {
            fq,
            ir,
            objtree,
            func,
            next_block_id: 0,
            closure_slots: IndexVec::new(),
            finished_blocks: Vec::new(),
        }
    }

    pub(crate) fn for_closure(
        fq: &'a mut FuncQueue<'ot>, ir: &'a mut Module, objtree: &'ot ObjtreeWrapper<'ot>,
        func: &'a mut cfg::Function, over: FuncId, scope: ScopeId,
    ) -> Self {
        func.set_closure(over, scope);
        Self {
            fq,
            ir,
            objtree,
            func,
            next_block_id: 0,
            closure_slots: IndexVec::new(),
            finished_blocks: Vec::new(),
        }
    }

    pub(crate) fn finalize(mut self) -> FuncId {
        self.finished_blocks.sort_by_key(|b| b.id);
        for block in self.finished_blocks.drain(..) {
            self.func.add_block(block);
        }
        self.func.analyze();

        let mut closure_funcs = IndexVec::new();
        for (scope, block) in self.closure_slots {
            let recipe = FuncRecipe::Closure {
                over: self.func.id,
                parent_scope: scope,
                block,
            };
            closure_funcs.push(self.fq.push(recipe));
        }

        self.func.child_closures = closure_funcs;

        self.func.id
    }

    // TODO: ERRH within frontend
    pub(crate) fn lookup_var(&mut self, scope: ScopeId, name: StringId) -> VarId {
        if let Some(var_in_proc) = self.func.lookup_var(scope, name) {
            return var_in_proc;
        }
        if let Some(var) = self.resolve_closure_var(scope, name, false) {
            return var;
        }
        panic!("can't find var: {:?}", name);
    }

    // If read_only is true, don't attempt to create a new var if it doesn't already exist.
    fn resolve_closure_var(
        &mut self, _scope: ScopeId, name: StringId, _read_only: bool,
    ) -> Option<VarId> {
        let first_parent;
        let first_scope;
        if let Some(closure) = &self.func.closure {
            first_parent = closure.over;
            first_scope = closure.scope;
        } else {
            return None;
        }

        let mut capture_stack = Vec::new();
        let mut cur_func_id = first_parent;
        let mut cur_scope = first_scope;
        let mut top_var;

        // Iterate downwards until we find a matching var.
        loop {
            println!("{:?}, {:?}", cur_func_id, cur_scope);

            let cur_func = self.ir.funcs.get_mut(cur_func_id).unwrap();
            if let Some(match_var) = cur_func.lookup_var(cur_scope, name) {
                top_var = match_var;
                break;
            }

            if let Some(closure) = &mut cur_func.closure {
                cur_func_id = closure.over;
                cur_scope = closure.scope;
                capture_stack.push(cur_func_id);
            } else {
                // If we've hit the root func without finding a var, we're out of options.
                return None;
            }
        }

        // At this point, top_var references a variable in cur_func_id.
        // We now need to unwind capture_stack and add capturing vars.
        let mut cur_parent = cur_func_id;
        for func_id in capture_stack.into_iter().rev() {
            println!("!! {:?}", func_id);
            let func = self.ir.funcs.get_mut(func_id).unwrap();
            let new_top_var = func.add_captured_var(name, top_var);
            let parent_func = self.ir.funcs.get_mut(cur_parent).unwrap();
            parent_func.mark_var_captured(top_var, func_id, new_top_var);
            top_var = new_top_var;
            cur_parent = func_id;
        }

        let final_var = self.func.add_captured_var(name, top_var);
        let parent_func = self.ir.funcs.get_mut(cur_parent).unwrap();
        parent_func.mark_var_captured(top_var, self.func.id, final_var);

        Some(final_var)
    }

    pub(crate) fn new_block(&mut self, scope: ScopeId) -> cfg::Block {
        let block = cfg::Block::new(BlockId::new(self.next_block_id), scope);
        self.next_block_id += 1;
        block
    }

    pub(crate) fn new_scope(&mut self, parent: ScopeId) -> ScopeId {
        self.func.new_scope(parent)
    }

    pub(crate) fn set_landingpad(&mut self, scope_id: ScopeId, landingpad: BlockId) {
        let scope = self.func.scopes.get_mut(scope_id).unwrap();
        if scope.landingpad.is_some() {
            panic!("landingpad already set");
        }
        self.set_landingpad_rec(scope_id, landingpad);
    }

    fn set_landingpad_rec(&mut self, scope_id: ScopeId, landingpad: BlockId) {
        let scope = self.func.scopes.get_mut(scope_id).unwrap();
        if scope.landingpad.is_some() {
            return;
        }
        scope.landingpad = Some(landingpad);
        let scope_children = scope.children.clone();
        for child_scope_id in scope_children {
            self.set_landingpad_rec(child_scope_id, landingpad);
        }
    }

    pub(crate) fn finalize_block(&mut self, block: cfg::Block) {
        self.finished_blocks.push(block);
    }

    // maybe make helper in BlBu which infers scope
    pub(crate) fn add_var(&mut self, scope: ScopeId, name: StringId) -> VarId {
        self.func.add_var(scope, TYPE_ID_ANY, name)
    }

    pub(crate) fn add_local(&mut self, scope: ScopeId, ty: Type) -> LocalId {
        let id = self.ir.types.insert(ty);
        self.func.add_local(scope, id)
    }

    pub(crate) fn set_var_dty(&mut self, var: VarId, dty: PathTypeId) {
        self.func.vars[var].assoc_dty = Some(dty)
    }

    pub(crate) fn add_closure_slot(
        &mut self, scope: ScopeId, block: &'ot ast::Block,
    ) -> ClosureSlotId {
        let ret = self.closure_slots.next_idx();
        self.closure_slots.push((scope, block));
        ret
    }

    pub fn build_block(
        &mut self, stmts: &'ot [ast::Spanned<ast::Statement>], parent_scope: Option<ScopeId>,
        next_block: Option<BlockId>,
    ) -> (BlockId, ScopeId) {
        let scope = self
            .func
            .new_scope(parent_scope.unwrap_or(self.func.global_scope));
        let block = self.new_block(scope);

        let mut bb = BlockBuilder::new(block);
        bb.build_stmts(self, stmts.iter());

        if let Some(next) = next_block {
            bb.set_terminator(cfg::Terminator::Jump(next));
        }

        let root_block_id = bb.done(self);
        (root_block_id, scope)
    }

    pub fn build_raw_sleep(&mut self, arg: VarId) {
        let scope = self.func.new_scope(self.func.global_scope);
        let mut block = self.new_block(scope);
        let lid = self.add_local(scope, Type::Any);
        block.push_op(cfg::Op::Load(lid, arg));
        block.push_op(cfg::Op::Sleep(lid));
        self.finalize_block(block)
    }

    pub fn builder_within_scope(&mut self, parent_scope: ScopeId) -> BlockBuilder {
        let scope = self.func.new_scope(parent_scope);
        let block = self.new_block(scope);
        BlockBuilder::new(block)
    }

    pub fn build_within_scope(
        &mut self, parent_scope: ScopeId, f: impl FnOnce(&mut BlockBuilder),
    ) -> (BlockId, ScopeId) {
        let scope = self.func.new_scope(parent_scope);
        let block = self.new_block(scope);

        let mut builder = BlockBuilder::new(block);

        f(&mut builder);

        let root_block_id = builder.done(self);
        (root_block_id, scope)
    }
}
