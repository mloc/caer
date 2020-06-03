use super::cfg_builder::CfgBuilder;
use super::block_builder::BlockBuilder;
use std::collections::HashMap;
use crate::ir::cfg;
use crate::ir::id::*;
use dreammaker::{ast, objtree};
use caer_runtime::string_table::StringId;
use caer_runtime::environment::ProcId;
use crate::ty;

pub struct ProcBuilder<'a, 'cb, 'ot> {
    pub builder: &'a mut CfgBuilder<'cb, 'ot>,
    pub ast_proc: &'ot objtree::ProcValue,
    pub vars: HashMap<String, LocalId>,
    pub proc: cfg::Proc,

    finished_blocks: Vec<cfg::Block>,
}

impl<'a, 'cb, 'ot> ProcBuilder<'a, 'cb, 'ot> {
    pub fn build(builder: &'a mut CfgBuilder<'cb, 'ot>, id: ProcId, ast_proc: &'ot objtree::ProcValue) -> cfg::Proc {
        let mut proc = cfg::Proc::new(id);
        let scope = proc.new_scope(proc.global_scope);

        let pb = Self {
            builder: builder,
            ast_proc: ast_proc,
            vars: HashMap::new(),
            proc: proc,

            finished_blocks: Vec::new(),
        };

        pb.build_proc()
    }

    pub fn finalize_block(&mut self, block: cfg::Block) {
        self.finished_blocks.push(block);
    }

    fn build_proc(mut self) -> cfg::Proc {
        for (i, param) in self.ast_proc.parameters.iter().enumerate() {
            // TODO: need to record name separately for keyword args?
            let name_id = self.builder.add_string(&param.name);
            let var_id = self.add_var(self.proc.global_scope, name_id);
            self.proc.params.push(var_id);

            let spec = self.builder.env.rt_env.get_proc_mut(self.proc.id);
            spec.params.push(name_id);
            spec.names.push((name_id, i as u32));
        }
        let spec = self.builder.env.rt_env.get_proc_mut(self.proc.id);
        spec.names.sort_unstable_by_key(|(ref s, _)| {*s});

        let body = if let objtree::Code::Present(ref b) = self.ast_proc.code {
            b
        } else {
            panic!("not present")
        };
        self.build_block(body.as_slice(), self.proc.global_scope, None);

        self.finished_blocks.sort_by_key(|b| b.id);
        for block in self.finished_blocks.drain(..) {
            self.proc.add_block(block);
        }

        self.proc.analyze();
        self.proc.dot(self.builder.env.string_table.get(self.proc.name));

        self.proc
    }

    pub fn build_block(&mut self, stmts: &[ast::Spanned<ast::Statement>], parent_scope: ScopeId, next_block: Option<BlockId>) -> BlockId {
        let scope = self.proc.new_scope(parent_scope);

        let mut builder = BlockBuilder::new(self, scope);

        for stmt in stmts.iter() {
            builder.build_stmt(&stmt.elem);
        }

        let block_id = builder.root_block_id;
        let mut block = builder.done();

        block.scope_end = true;

        if let Some(next) = next_block {
            block.terminator = cfg::Terminator::Jump(next);
        }

        self.finalize_block(block);

        block_id
    }

    // maybe make helper in BlBu which infers scope
    pub fn add_var(&mut self, scope: ScopeId, name: StringId) -> VarId {
        self.proc.add_var(scope, ty::Complex::Any, name)
    }
}
