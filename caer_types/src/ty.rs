use std::collections::BTreeSet;

use serde::{Deserialize, Serialize};

use crate::id::{PathTypeId, TypeId};

pub trait Ty {
    fn needs_destructor(&self) -> bool;
    fn as_primitive(&self) -> Option<Type>;
    fn is_primitive(&self, prim: Type) -> bool;
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Serialize, Deserialize)]
pub enum Type {
    Null,

    Float,

    // GC-kind
    //String,
    //List(TypeId),
    // One day, String and List will be represented as path nodes. One day.
    //Datum(PathTypeId),
    //DatumAny,

    // Object ref
    Ref(RefType),
    // btreeset is bad.
    OneOf(BTreeSet<TypeId>),
    // Val type
    Any,
    /*Proc {
        args: Vec<PathTypeId>,
        var_args: Option<Box<PathTypeId>>,
        ret: Box<PathTypeId>,
    },*/
}

impl Type {
    // term any is overloaded.
    // in this case it means any ty that needs RTTI
    pub fn is_any(&self) -> bool {
        matches!(
            self,
            Type::Ref(RefType::Subtype(_) | RefType::Any) | Type::OneOf(_) | Type::Any
        )
    }

    /*pub fn contains(&self, prim: Prim) -> bool {
        match self {
            Type::Any => true,
            Type::Primitive(my_prim) => *my_prim == prim,
            Type::OneOf(tys) => tys.iter().any(|ty| ty.contains(prim)),
            _ => false,
        }
    }*/

    // BAD. TODO: remove, x64 specific
    pub fn get_store_size(&self) -> u64 {
        if self.is_any() {
            return 16;
        }
        match self {
            Type::Null | Type::Float | Type::Ref(RefType::String) => 8,
            _ => panic!("can't get size of {:?}", self),
        }
    }
}

impl From<RefType> for Type {
    fn from(rt: RefType) -> Self {
        Self::Ref(rt)
    }
}

/*impl Ty for Prim {
    fn needs_destructor(&self) -> bool {
        matches!(self, Prim::Ref(_))
    }

    fn as_primitive(&self) -> Option<Prim> {
        Some(*self)
    }

    fn is_primitive(&self, prim: Prim) -> bool {
        *self == prim
    }
}*/

impl Ty for Type {
    fn needs_destructor(&self) -> bool {
        todo!();
        /*match self {
            Type::Any => true,
            Type::Primitive(p) => p.needs_destructor(),
            Type::OneOf(tys) => tys.iter().any(|ty| ty.needs_destructor()),
            Type::Proc { .. } => false,
            _ => unimplemented!("{:?}", self),
        }*/
    }

    fn as_primitive(&self) -> Option<Type> {
        todo!();
        /*match self {
            Type::
        }
        if let Type::Primitive(my_prim) = self {
            Some(*my_prim)
        } else {
            None
        }*/
    }

    fn is_primitive(&self, prim: Type) -> bool {
        todo!();
        /*if let Type::Primitive(my_prim) = self {
            *my_prim == prim
        } else {
            false
        }*/
    }
}

// Eventually this will contain: generic args, no special casing for list/string, no special casing
// for Any?
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Serialize, Deserialize)]
pub enum RefType {
    Any,
    String,
    List(TypeId),
    Exact(PathTypeId),
    Subtype(PathTypeId),
}
