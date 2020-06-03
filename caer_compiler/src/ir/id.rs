use index_vec::{define_index_type};
use serde::{Serialize, Deserialize};

define_index_type!{pub struct LocalId = u32;}
define_index_type!{pub struct VarId = u32;}
define_index_type!{pub struct BlockId = u32;}
define_index_type!{pub struct ScopeId = u32;}
