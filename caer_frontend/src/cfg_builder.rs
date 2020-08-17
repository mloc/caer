use super::proc_builder::ProcBuilder;
use caer_ir::env::Env;
use caer_types::id::{ProcId, StringId};
use dreammaker::objtree;
use index_vec::IndexVec;
use std::borrow::Cow;

pub struct CfgBuilder<'a, 'ot> {
    pub env: &'a mut Env,
    procs: &'ot IndexVec<ProcId, objtree::ProcValue>,
    pub objtree: &'ot objtree::ObjectTree,
}
impl<'a, 'ot> CfgBuilder<'a, 'ot> {
    pub fn new(
        env: &'a mut Env,
        procs: &'ot IndexVec<ProcId, objtree::ProcValue>,
        objtree: &'ot objtree::ObjectTree,
    ) -> Self {
        Self {
            env,
            procs,
            objtree,
        }
    }

    // TODO: ERRH(fe)
    pub fn build(&mut self) {
        self.env.procs = self
            .procs
            .iter_enumerated()
            .map(|(i, proc)| ProcBuilder::build(self, i, &proc))
            .collect();

        // wow ew
        for proc_id_n in 0..self.procs.len() {
            let proc_id = ProcId::new(proc_id_n);
            caer_ir::analysis::ProcAnalysis::analyse_proc(self.env, proc_id);
            self.env.procs[proc_id].dot(&format!("opt_proc_{}", proc_id.index()));
        }
    }

    pub fn add_string<'s>(&mut self, s: impl Into<Cow<'s, str>>) -> StringId {
        self.env.string_table.put(s)
    }
}
