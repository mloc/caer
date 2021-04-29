use serde::{Deserialize, Serialize};

use crate::id::{FuncId, StringId};

/// Holds run-time information about a func.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FuncInfo {
    //pub name: StringId,
    pub calling_spec: CallingSpec,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CallingSpec {
    // Regular proc calls, with named and default vars, and nulling
    Proc(ProcSpec),
    // DM closures are all argumentless (for now)
    Closure(ClosureSpec),
}

impl CallingSpec {
    pub fn get_proc_spec(&self) -> Option<&ProcSpec> {
        match self {
            CallingSpec::Proc(s) => Some(s),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ProcSpec {
    pub params: Vec<StringId>,
    // name + index into params list. sorted by name string id, used to resolve keyword args
    // we assume no two params have the same name. TODO: validate this assumption
    pub names: Vec<(StringId, u32)>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClosureSpec {
    pub closure_parent: FuncId,
    pub env_size: u64,
}
