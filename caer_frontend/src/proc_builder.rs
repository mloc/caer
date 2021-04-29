use super::func_builder::FuncBuilder;
use caer_ir::cfg;
use caer_ir::cfg::Function;
use caer_ir::env::Env;
use caer_types::func::{CallingSpec, ProcSpec};
use caer_types::id::FuncId;
use caer_types::ty;
use dreammaker::objtree::{self, ProcValue};

pub struct ProcBuilder<'a> {
    // should be ProcId, eventually
    pub id: FuncId,
    pub env: &'a mut Env,
    pub objtree: &'a objtree::ObjectTree,
}

impl<'a> ProcBuilder<'a> {
    pub fn build(
        func: Function,
        env: &'a mut Env,
        objtree: &'a objtree::ObjectTree,
        body: &'a ProcBody,
    ) -> FuncId {
        let pb = Self {
            id: func.id,
            env,
            objtree,
        };

        pb.build_proc(func, body)
    }

    fn build_proc(mut self, mut func: Function, body: &'a ProcBody) -> FuncId {
        func.new_scope(func.global_scope);

        match body {
            ProcBody::Ast(pv) => {
                self.build_ast_proc(func, pv)
            },
            ProcBody::Builtin(b) => {
                self.build_builtin(func, b)
            },
        }
    }

    fn build_ast_proc(&mut self, mut func: Function, pv: &ProcValue) -> FuncId {
        let mut proc_spec = ProcSpec::default();

        for (i, param) in pv.parameters.iter().enumerate() {
            // TODO: need to record name separately for keyword args?
            let name_id = self.env.intern_string(&param.name);
            let var_id = func.add_var(func.global_scope, ty::Complex::Any, name_id);
            func.params.push(var_id);

            proc_spec.params.push(name_id);
            proc_spec.names.push((name_id, i as u32));
        }
        proc_spec.names.sort_unstable_by_key(|(ref s, _)| *s);
        func.calling_spec = Some(CallingSpec::Proc(proc_spec));

        let mut func_builder = FuncBuilder::for_proc(&mut self.env, &self.objtree, func);

        let body = if let objtree::Code::Present(ref b) = pv.code {
            b
        } else {
            panic!("not present")
        };
        func_builder.build_block(body.as_slice(), None, None);
        func_builder.finalize()
    }

    fn build_builtin(&mut self, mut func: Function, builtin: &BuiltinProc) -> FuncId {
        match builtin {
            BuiltinProc::Sleep => {
                // TODO: handle sleep arg
                let proc_spec = ProcSpec::default();
                func.calling_spec = Some(CallingSpec::Proc(proc_spec));
                let mut func_builder = FuncBuilder::for_proc(&mut self.env, &self.objtree, func);
                func_builder.build_raw(&[cfg::Op::Sleep(None)]);
                func_builder.func.builtin = true;
                func_builder.finalize()
            },
        }
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

#[derive(Clone, Debug)]
pub enum ProcBody {
    Ast(objtree::ProcValue),
    Builtin(BuiltinProc),
}

#[derive(Clone, Debug)]
pub enum BuiltinProc {
    Sleep,
}
