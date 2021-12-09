use std::collections::VecDeque;

use caer_ir::cfg::{self, Function};
use caer_ir::id::ScopeId;
use caer_ir::module::Module;
use caer_types::id::{FuncId, StringId};
use dreammaker::ast::{Spanned, Statement};
use index_vec::IndexVec;

use super::proc_builder::ProcBuilder;
use crate::func_builder::FuncBuilder;
use crate::objtree_wrapper::ObjtreeWrapper;
use crate::proc_builder::ProcBody;

pub struct IrBuilder<'a, 'ot> {
    pub ir: &'a mut Module,
    func_queue: FuncQueue<'ot>,
    pub objtree: &'ot ObjtreeWrapper<'ot>,
}

impl<'a, 'ot> IrBuilder<'a, 'ot> {
    pub fn new(
        ir: &'a mut Module, funcs: IndexVec<FuncId, FuncRecipe<'ot>>,
        objtree: &'ot ObjtreeWrapper<'ot>,
    ) -> Self {
        Self {
            ir,
            func_queue: FuncQueue::from_init_funcs(funcs),
            objtree,
        }
    }

    // TODO: ERRH(fe)
    pub fn build(mut self) {
        while let Some((id, recipe)) = self.func_queue.pop_next() {
            let mut func = self.ir.new_func();
            assert_eq!(func.id, id);

            self.build_func(&mut func, recipe);
            self.ir.assimilate_func(func);
        }

        // wow ewww
        for proc_id_n in 0..self.ir.funcs.len() {
            let proc_id = FuncId::new(proc_id_n);
            self.ir.funcs[proc_id].dot(&format!("proc_{}", proc_id.index()));
            caer_ir::analysis::FuncAnalysis::analyse_proc(self.ir, proc_id);
            self.ir.funcs[proc_id].dot(&format!("opt_proc_{}", proc_id.index()));
        }
    }

    fn build_func(&mut self, func: &mut Function, recipe: FuncRecipe<'ot>) {
        match recipe {
            FuncRecipe::ForProc { body, parent } => {
                ProcBuilder::build(
                    func,
                    &mut self.func_queue,
                    self.ir,
                    self.objtree,
                    body,
                    parent,
                );
            },
            FuncRecipe::Closure {
                over,
                parent_scope,
                block,
            } => {
                let mut closure_builder = FuncBuilder::for_closure(
                    &mut self.func_queue,
                    self.ir,
                    self.objtree,
                    func,
                    over,
                    parent_scope,
                );
                closure_builder.build_block(block, None, None);
                closure_builder.finalize();
            },
        }
    }

    pub fn add_string(&mut self, s: impl Into<String> + AsRef<str>) -> StringId {
        self.ir.string_table.put(s)
    }
}

pub struct FuncQueue<'ot> {
    queue: VecDeque<FuncRecipe<'ot>>,
    front_id: FuncId,
    next_back_id: FuncId,
}

impl<'ot> FuncQueue<'ot> {
    fn from_init_funcs(funcs: IndexVec<FuncId, FuncRecipe<'ot>>) -> Self {
        Self {
            next_back_id: funcs.next_idx(),
            front_id: 0.into(),
            queue: funcs.into_iter().collect(),
        }
    }

    fn pop_next(&mut self) -> Option<(FuncId, FuncRecipe<'ot>)> {
        if self.queue.is_empty() {
            return None;
        }
        let ret = self.queue.pop_front().map(|recipe| (self.front_id, recipe));
        self.front_id += 1;

        if self.queue.is_empty() {
            assert_eq!(self.front_id, self.next_back_id);
        }

        ret
    }

    pub fn push(&mut self, recipe: FuncRecipe<'ot>) -> FuncId {
        let id = self.next_back_id;
        self.queue.push_back(recipe);
        self.next_back_id += 1;
        id
    }
}

#[derive(Debug)]
pub enum FuncRecipe<'ot> {
    ForProc {
        body: ProcBody<'ot>,
        parent: Option<FuncId>,
    },
    Closure {
        over: FuncId,
        parent_scope: ScopeId,
        block: &'ot Vec<Spanned<Statement>>,
    },
}
