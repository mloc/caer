use super::proc_builder::ProcBuilder;
use caer_ir::env::Env;
use caer_types::id::{FuncId, StringId};
use dreammaker::objtree;
use index_vec::IndexVec;
use std::borrow::Cow;

pub struct IrBuilder<'a> {
    pub env: &'a mut Env,
    procs: &'a IndexVec<FuncId, objtree::ProcValue>,
    pub objtree: &'a objtree::ObjectTree,
}

impl<'a> IrBuilder<'a> {
    pub fn new(
        env: &'a mut Env,
        procs: &'a IndexVec<FuncId, objtree::ProcValue>,
        objtree: &'a objtree::ObjectTree,
    ) -> Self {
        Self {
            env,
            procs,
            objtree,
        }
    }

    // TODO: ERRH(fe)
    pub fn build<'s: 'a>(&'s mut self) {
        /*self.env.procs = self
            .procs
            .iter_enumerated()
            .map(|(i, proc)| ProcBuilder::build(self, i, &proc))
            .collect();*/

        self.env.funcs = Default::default();
        for (i, proc) in self.procs.iter_enumerated() {
            println!("building {:?} @ {:?}", i, proc.location);
            ProcBuilder::build(i, self.env, self.objtree, proc);
        }

        // wow ewww
        for proc_id_n in 0..self.env.funcs.len() {
            let proc_id = FuncId::new(proc_id_n);
            caer_ir::analysis::ProcAnalysis::analyse_proc(self.env, proc_id);
            self.env.funcs[&proc_id].dot(&format!("opt_proc_{}", proc_id.index()));
        }
    }

    pub fn add_string<'s>(&mut self, s: impl Into<Cow<'s, str>>) -> StringId {
        self.env.string_table.put(s)
    }
}
