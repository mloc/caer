use super::ir_builder::IrBuilder;
use super::block_builder::BlockBuilder;
use super::func_builder::FuncBuilder;
use std::collections::HashMap;
use caer_ir::cfg;
use caer_ir::id::{ScopeId, BlockId, VarId, LocalId};
use dreammaker::{ast, objtree};
use caer_types::id::{StringId, FuncId};
use caer_ir::env::Env;
use caer_types::ty;

pub struct ProcBuilder<'a> {
    pub env: &'a mut Env,
    pub objtree: &'a objtree::ObjectTree,
    pub ast_proc: &'a objtree::ProcValue,
    pub vars: HashMap<String, LocalId>,
}

impl<'a> ProcBuilder<'a> {
    pub fn build(env: &'a mut Env, objtree: &'a objtree::ObjectTree, ast_proc: &'a objtree::ProcValue) -> cfg::Function {
        let pb = Self {
            env,
            objtree,
            ast_proc,
            vars: HashMap::new(),
        };

        pb.build_proc()
    }

    fn build_proc(mut self) -> cfg::Function {
        let mut proc = self.env.new_func();
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
        spec.names.sort_unstable_by_key(|(ref s, _)| {*s});

        let gs = proc.global_scope;
        let mut func_builder = FuncBuilder::for_proc(&mut self.env, &self.objtree, proc);

        let body = if let objtree::Code::Present(ref b) = self.ast_proc.code {
            b
        } else {
            panic!("not present")
        };
        func_builder.build_block(body.as_slice(), None, None);

        let mut proc = func_builder.finalize();

        proc.analyze();

        //self.proc.dot(self.builder.env.string_table.get(self.proc.name));
        proc.dot(&format!("proc_{}", proc.id.index()));

        proc
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
