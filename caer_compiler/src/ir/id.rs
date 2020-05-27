use indexed_vec::{newtype_index, Idx};
use serde::{Serialize, Deserialize};

newtype_index!(LocalId {pub idx});
newtype_index!(BlockId {pub idx});
newtype_index!(ScopeId {pub idx});
