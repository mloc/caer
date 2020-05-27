use std::collections::HashMap;
use std::borrow::Cow;
use crate::ir::cfg;
use dreammaker::{ast, objtree};
use indexed_vec::Idx;
use caer_runtime::op::BinaryOp;
use caer_runtime::string_table::StringId;
use caer_runtime::proc_spec::ProcSpec;
use caer_runtime::type_tree;
use crate::ty;

pub mod builder;
mod proc_builder;
mod block_builder;
