use crate::ir::env::Env;
use dreammaker::objtree;
use caer_runtime::string_table::StringId;
use caer_runtime::environment::ProcId;
use std::borrow::Cow;
use super::proc_builder::ProcBuilder;
use index_vec::{IndexVec};

pub struct CfgBuilder<'a, 'ot> {
    pub env: &'a mut Env,
    procs: &'ot IndexVec<ProcId, objtree::ProcValue>,
    pub objtree: &'ot objtree::ObjectTree,
}
impl<'a, 'ot> CfgBuilder<'a, 'ot> {
    pub fn new(env: &'a mut Env, procs: &'ot IndexVec<ProcId, objtree::ProcValue>, objtree: &'ot objtree::ObjectTree) -> Self {
        Self {
            env: env,
            procs: procs,
            objtree: objtree,
        }
    }

    // TODO: ERRH(fe)
    pub fn build(&mut self) {
        self.env.procs = self.procs.iter_enumerated().map(|(i, proc)| {
            ProcBuilder::build(self, i, &proc)
        }).collect();

        // wow ew
        for proc_id_n in 0..self.procs.len() {
            let proc_id = ProcId::new(proc_id_n);
            crate::ir::analysis::ProcAnalysis::analyse_proc(self.env, proc_id);
            self.env.procs[proc_id].dot(&format!("proc_opt_{}", proc_id.index()));
        }
    }

    pub fn add_string<'s>(&mut self, s: impl Into<Cow<'s, str>>) -> StringId {
        self.env.string_table.put(s)
    }
}
