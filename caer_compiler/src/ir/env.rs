use std::collections::HashMap;
use std::borrow::Cow;
use caer_runtime::string_table::{StringTable, StringId};
use indexed_vec::{IndexVec, Idx};
use caer_runtime::environment::ProcId;
use caer_runtime;
use super::cfg::*;

// this should be renamed; ir-something
// this is the output of the frontend
#[derive(Debug)]
pub struct Env {
    // is this bad? yes
    // but it's easy
    // TODO: do something less tightly coupled
    pub string_table: StringTable,
    pub rt_env: caer_runtime::environment::Environment,
    pub procs: IndexVec<ProcId, Proc>,
}

impl Env {
    pub fn new() -> Self {
        let mut string_table = StringTable::new();
        Self {
            string_table: string_table,
            rt_env: caer_runtime::environment::Environment::new(),
            procs: IndexVec::new(),
        }
    }

    pub fn intern_string<'s>(&mut self, s: impl Into<Cow<'s, str>>) -> StringId {
        self.string_table.put(s)
    }

    pub fn intern_string_ro(&self, s: impl AsRef<str>) -> StringId {
        self.string_table.lookup(s.as_ref()).unwrap()
    }
}

