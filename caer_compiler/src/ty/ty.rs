use indexed_vec::{newtype_index, Idx};
use serde::{Serialize, Deserialize};
use caer_runtime::type_tree;

newtype_index!(TyId {pub idx});

pub trait Ty {
    fn needs_destructor(&self) -> bool;
    fn as_primitive(&self) -> Option<Primitive>;
    fn is_primitive(&self, prim: Primitive) -> bool;
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Primitive {
    Null,
    Float,
    String,
    Int,
    // None => any ref. maybe replace with id 0
    Ref(Option<type_tree::TypeId>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Complex {
    Any,
    List(Box<Complex>),
    Primitive(Primitive),
    OneOf(Vec<Complex>),
    Proc { args: Vec<Complex>, var_args: Option<Box<Complex>>, ret: Box<Complex> },
}

impl Complex {
    pub fn contains(&self, prim: Primitive) -> bool {
        match self {
            Complex::Any => true,
            Complex::Primitive(my_prim) => *my_prim == prim,
            Complex::OneOf(tys) => tys.iter().any(|ty| ty.contains(prim)),
            _ => false,
        }
    }
}

impl Ty for Primitive {
    fn needs_destructor(&self) -> bool {
        match self {
            Primitive::Ref(_) => true,
            _ => false,
        }
    }

    fn as_primitive(&self) -> Option<Primitive> {
        return Some(*self)
    }

    fn is_primitive(&self, prim: Primitive) -> bool {
        *self == prim
    }
}

impl Ty for Complex {
    fn needs_destructor(&self) -> bool {
        match self {
            Complex::Any => true,
            Complex::Primitive(p) => p.needs_destructor(),
            Complex::OneOf(tys) => tys.iter().any(|ty| ty.needs_destructor()),
            Complex::Proc { .. } => false,
            _ => unimplemented!("{:?}", self),
        }
    }

    fn as_primitive(&self) -> Option<Primitive> {
        if let Complex::Primitive(my_prim) = self {
            Some(*my_prim)
        } else {
            None
        }
    }

    fn is_primitive(&self, prim: Primitive) -> bool {
        if let Complex::Primitive(my_prim) = self {
            *my_prim == prim
        } else {
            false
        }
    }
}

impl From<Primitive> for Complex {
    fn from(p: Primitive) -> Complex {
        Complex::Primitive(p)
    }
}
