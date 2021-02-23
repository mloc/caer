use super::block_builder::BlockBuilder;
use super::func_builder::FuncBuilder;
use super::ir_builder::IrBuilder;
use caer_ir::cfg;
use caer_ir::cfg::Function;
use caer_ir::env::Env;
use caer_ir::id::{BlockId, LocalId, ScopeId, VarId};
use caer_types::id::{FuncId, StringId};
use caer_types::ty;
use dreammaker::{ast, objtree};
use std::collections::HashMap;

pub struct ProcBuilder<'a> {
    // should be ProcId, eventually
    pub id: FuncId,
    pub env: &'a mut Env,
    pub objtree: &'a objtree::ObjectTree,
    pub ast_proc: &'a objtree::ProcValue,
    pub vars: HashMap<String, LocalId>,
}

impl<'a> ProcBuilder<'a> {
    pub fn build(
        id: FuncId,
        env: &'a mut Env,
        objtree: &'a objtree::ObjectTree,
        ast_proc: &'a objtree::ProcValue,
    ) -> FuncId {
        let pb = Self {
            id,
            env,
            objtree,
            ast_proc,
            vars: HashMap::new(),
        };

        pb.build_proc()
    }

    fn build_proc(mut self) -> FuncId {
        let mut proc = Function::new(self.id);
        proc.new_scope(proc.global_scope);

        for (i, param) in self.ast_proc.parameters.iter().enumerate() {
            // TODO: need to record name separately for keyword args?
            let name_id = self.env.intern_string(&param.name);
            let var_id = proc.add_var(proc.global_scope, ty::Complex::Any, name_id);
            proc.params.push(var_id);

            let spec = self.env.get_proc_mut(proc.id);
            spec.params.push(name_id);
            spec.names.push((name_id, i as u32));
        }
        let spec = self.env.get_proc_mut(proc.id);
        spec.names.sort_unstable_by_key(|(ref s, _)| *s);

        let mut func_builder = FuncBuilder::for_proc(&mut self.env, &self.objtree, proc);

        let body = if let objtree::Code::Present(ref b) = self.ast_proc.code {
            b
        } else {
            panic!("not present")
        };
        func_builder.build_block(body.as_slice(), None, None);
        func_builder.finalize()
    }

    /*pub fn build_block(&mut self, stmts: &[ast::Spanned<ast::Statement>], parent_scope: ScopeId, next_block: Option<BlockId>) -> (BlockId, ScopeId) {
        self.build_within_scope(parent_scope, |bb| {
            bb.build_stmts(stmts.iter());

            if let Some(next) = next_block {
                bb.block.terminator = cfg::Terminator::Jump(next);
            }
        })
    }*/

    /*pub fn build_within_scope(&mut self, parent_scope: ScopeId, f: impl FnOnce(&mut BlockBuilder)) -> (BlockId, ScopeId) {
        let scope = self.proc.new_scope(parent_scope);

        let mut builder = BlockBuilder::new(self, scope);

        f(&mut builder);

        let root_block_id = builder.root_block_id;
        let mut block = builder.done();
        block.scope_end = true;

        self.finalize_block(block);
        (root_block_id, scope)
    }*/
}
