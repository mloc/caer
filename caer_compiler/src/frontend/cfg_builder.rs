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
    }

    pub fn add_string<'s>(&mut self, s: impl Into<Cow<'s, str>>) -> StringId {
        self.env.string_table.put(s)
    }
}
