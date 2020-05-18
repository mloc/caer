// this is gonna be too monolithic, split it up;

use indexed_vec::{IndexVec, newtype_index, Idx};
use serde::{Serialize, Deserialize};

use crate::proc_spec::ProcSpec;

newtype_index!(ProcId {pub idx});

// TODO: move stringtable into here, at least static
#[derive(Debug, Serialize, Deserialize)]
pub struct Environment {
    pub(crate) proc_specs: IndexVec<ProcId, ProcSpec>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            proc_specs: IndexVec::new(),
        }
    }
}
