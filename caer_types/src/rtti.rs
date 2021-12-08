use serde::{Deserialize, Serialize};

use crate::ty;
use crate::type_tree::Specialization;

// A captured "instance" of a type known at compile time
// Used for dynamic dispatch and some RTTI.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TyInstance {
    pub ty: ty::Type,
    pub specialization: Option<Specialization>,
}
