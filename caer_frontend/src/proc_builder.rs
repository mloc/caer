use caer_ir::cfg;
use caer_ir::cfg::Function;
use caer_ir::module::Module;
use caer_types::func::{CallingSpec, ProcSpec};
use caer_types::id::{FuncId, TYPE_ID_ANY};
use caer_types::ty;
use dreammaker::objtree::{self, ProcValue};

use super::func_builder::FuncBuilder;
use crate::ir_builder::{FuncQueue, FuncRecipe};
use crate::objtree_wrapper::ObjtreeWrapper;

pub struct ProcBuilder<'a, 'ot> {
    // should be ProcId, eventually
    pub id: FuncId,
    fq: &'a mut FuncQueue<'ot>,
    pub ir: &'a mut Module,
    pub objtree: &'ot ObjtreeWrapper<'ot>,
}

impl<'a, 'ot> ProcBuilder<'a, 'ot> {
    pub fn build(
        func: &'a mut Function, fq: &'a mut FuncQueue<'ot>, ir: &'a mut Module,
        objtree: &'ot ObjtreeWrapper<'ot>, body: ProcBody<'ot>, parent: Option<FuncId>,
    ) -> FuncId {
        let pb = Self {
            fq,
            id: func.id,
            ir,
            objtree,
        };

        pb.build_proc(func, body, parent)
    }

    fn build_proc(
        mut self, func: &'a mut Function, body: ProcBody<'ot>, parent: Option<FuncId>,
    ) -> FuncId {
        func.parent = parent;
        func.new_scope(func.global_scope);

        match body {
            ProcBody::Ast(pv) => self.build_ast_proc(func, pv),
            ProcBody::Builtin(b) => self.build_builtin(func, b),
        }
    }

    fn build_ast_proc(&mut self, func: &'a mut Function, pv: &'ot ProcValue) -> FuncId {
        let mut proc_spec = ProcSpec::default();

        for (i, param) in pv.parameters.iter().enumerate() {
            // TODO: need to record name separately for keyword args?
            let name_id = self.ir.intern_string(&param.name);
            let var_id = func.add_var(func.global_scope, TYPE_ID_ANY, name_id);
            func.params.push(var_id);

            proc_spec.params.push(name_id);
            proc_spec.names.push((name_id, i as u32));
        }
        proc_spec.names.sort_unstable_by_key(|(ref s, _)| *s);
        func.calling_spec = Some(CallingSpec::Proc(proc_spec));

        let mut func_builder = FuncBuilder::for_proc(self.fq, self.ir, self.objtree, func);

        let body = if let objtree::Code::Present(ref b) = pv.code {
            b
        } else {
            panic!("not present")
        };
        func_builder.build_block(body.as_slice(), None, None);
        func_builder.finalize()
    }

    // wow this is awful!! TODO: Better builtin
    fn build_builtin(&mut self, func: &'a mut Function, builtin: BuiltinProc) -> FuncId {
        match builtin {
            BuiltinProc::Sleep => {
                let s_delay = self.ir.intern_string("delay");
                // TODO: handle args better
                let id = func.add_var(func.global_scope, TYPE_ID_ANY, s_delay);
                func.params.push(id);
                let mut proc_spec = ProcSpec::default();
                proc_spec.params.push(s_delay);
                proc_spec.names.push((s_delay, 0));
                func.calling_spec = Some(CallingSpec::Proc(proc_spec));
                let mut func_builder = FuncBuilder::for_proc(self.fq, self.ir, self.objtree, func);
                func_builder.build_raw_sleep(id);
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

#[derive(Clone, Copy, Debug)]
pub enum ProcBody<'ot> {
    Ast(&'ot objtree::ProcValue),
    Builtin(BuiltinProc),
}

#[derive(Clone, Copy, Debug)]
pub enum BuiltinProc {
    Sleep,
}
