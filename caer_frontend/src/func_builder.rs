// naming is hard, TODO: rename some other uses of "proc" to "func"

use crate::ir_builder::IrBuilder;
use crate::block_builder::BlockBuilder;
use caer_ir::env::Env;
use std::collections::{BTreeMap, HashMap};
use caer_ir::cfg;
use caer_ir::id::{ScopeId, BlockId, VarId, LocalId};
use dreammaker::{ast, objtree};
use caer_types::id::{StringId, FuncId, TypeId};
use caer_types::ty;

pub struct FuncBuilder<'a> {
    pub(crate) env: &'a mut Env,
    // TODO: can we get rid of this?
    pub(crate) objtree: &'a objtree::ObjectTree,
    pub(crate) func: cfg::Function,
    closure: Option<ClosureEnvironment<'a, 'a>>,

    next_block_id: usize,
    finished_blocks: Vec<cfg::Block>,
}

impl<'a> FuncBuilder<'a> {
    pub(crate) fn for_proc(env: &'a mut Env, objtree: &'a objtree::ObjectTree, func: cfg::Function) -> Self {
        Self {
            env,
            objtree,
            func,
            closure: None,
            next_block_id: 0,
            finished_blocks: Vec::new(),
        }
    }

    pub(crate) fn for_closure(env: &'a mut Env, objtree: &'a objtree::ObjectTree, closure: ClosureEnvironment<'a, 'a>) -> Self {
        let func = env.new_func();
        Self {
            env,
            objtree,
            func,
            closure: Some(closure),
            next_block_id: 0,
            finished_blocks: Vec::new(),
        }
    }

    pub(crate) fn finalize(mut self) -> cfg::Function {
        self.finished_blocks.sort_by_key(|b| b.id);
        for block in self.finished_blocks.drain(..) {
            self.func.add_block(block);
        }
        self.func
    }

    // TODO: ERRH within frontend
    pub(crate) fn lookup_var(&mut self, scope: ScopeId, name: StringId) -> VarId {
        if let Some(var_in_proc) = self.func.lookup_var(scope, name) {
            return var_in_proc
        }

        if let Some(closure) = &mut self.closure {
            let parent_var = closure.parent_builder.lookup_var(closure.capture_scope, name);
            let binding_var = self.func.add_captured_var(name);
            closure.captured.push((parent_var, binding_var));

            return binding_var
        }

        panic!("can't find var: {:?}", name);
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
        self.func.add_var(scope, ty::Complex::Any, name)
    }

    pub(crate) fn add_local(&mut self, scope: ScopeId, ty: ty::Complex) -> LocalId {
        self.func.add_local(scope, ty)
    }

    pub(crate) fn set_var_dty(&mut self, var: VarId, dty: TypeId) {
        self.func.vars[var].assoc_dty = Some(dty)
    }

    pub fn build_block(&mut self, stmts: &[ast::Spanned<ast::Statement>], parent_scope: Option<ScopeId>, next_block: Option<BlockId>) -> (BlockId, ScopeId) {
        let scope = self.func.new_scope(parent_scope.unwrap_or(self.func.global_scope));
        let block = self.new_block(scope);

        let mut bb = BlockBuilder::new(block);
        bb.build_stmts(self, stmts.iter());

        if let Some(next) = next_block {
            bb.set_terminator(cfg::Terminator::Jump(next));
        }

        let root_block_id = bb.done(self);
        (root_block_id, scope)
    }

    pub fn builder_within_scope(&mut self, parent_scope: ScopeId) -> BlockBuilder {
        let scope = self.func.new_scope(parent_scope);
        let block = self.new_block(scope);
        BlockBuilder::new(block)
    }

    pub fn build_within_scope(&mut self, parent_scope: ScopeId, f: impl FnOnce(&mut BlockBuilder)) -> (BlockId, ScopeId) {
        let scope = self.func.new_scope(parent_scope);
        let block = self.new_block(scope);

        let mut builder = BlockBuilder::new(block);

        f(&mut builder);

        let root_block_id = builder.done(self);
        (root_block_id, scope)
    }
}

pub struct ClosureEnvironment<'a, 'fb> {
    parent_builder: &'a mut FuncBuilder<'fb>,
    capture_scope: ScopeId,
    // var in parent -> var in closure
    captured: Vec<(VarId, VarId)>,
}

impl<'a, 'fb> ClosureEnvironment<'a, 'fb> {
    pub fn new(parent_builder: &'a mut FuncBuilder<'fb>, capture_scope: ScopeId) -> Self {
        Self {
            parent_builder,
            capture_scope,
            captured: Vec::new(),
        }
    }
}
