// TODO: find a better home
// TODO: find better names

use std::borrow::Cow;
use caer_ir::id::{ScopeId, BlockId, VarId, LocalId};
use dreammaker::{ast, objtree};
use caer_types::ty;
use caer_ir::cfg;
use caer_types::id::{StringId, FuncId, TypeId};
use crate::ir_builder::IrBuilder;

trait BuilderEncapIr {
    fn intern_string<'s>(&mut self, s: impl Into<Cow<'s, str>>) -> StringId;

    // TODO: cop-out. break it up
    fn ir(&self) -> &IrBuilder;
}

trait BuilderEncapFunc {
    fn add_var(&mut self, scope: ScopeId, name: StringId) -> VarId;
    fn add_local(&mut self, scope: ScopeId, ty: ty::Complex) -> LocalId;
    fn set_var_dty(&mut self, var: VarId, dty: TypeId);
    fn finalize(self) -> cfg::Function;
    fn lookup_var(&mut self, scope: ScopeId, name: StringId) -> VarId;
    fn new_block(&mut self, scope: ScopeId) -> cfg::Block;
    fn new_scope(&mut self, parent: ScopeId) -> ScopeId;
    fn set_landingpad(&mut self, scope_id: ScopeId, landingpad: BlockId);
    fn finalize_block(&mut self, block: cfg::Block);
}
