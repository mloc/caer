use crate::id::TypeId;
use index_vec::define_index_type;
use std::collections::BTreeSet;
use serde::Serialize;

define_index_type! {pub struct TyId = u32;}

pub trait Ty {
    fn needs_destructor(&self) -> bool;
    fn as_primitive(&self) -> Option<Primitive>;
    fn is_primitive(&self, prim: Primitive) -> bool;
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Serialize)]
pub enum Primitive {
    Null,
    Float,
    String,
    //Int,
    // None => any ref. maybe replace with id 0
    Ref(Option<TypeId>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize)]
pub enum Complex {
    Any,
    Pointer(Box<Complex>),
    List(Box<Complex>),
    Primitive(Primitive),
    // btreeset is bad.
    OneOf(BTreeSet<Complex>),
    Proc {
        args: Vec<Complex>,
        var_args: Option<Box<Complex>>,
        ret: Box<Complex>,
    },
}

impl Complex {
    pub fn unify<'a>(iter: impl Iterator<Item = &'a Complex>) -> Self {
        let mut set = BTreeSet::new();

        for ty in iter {
            /*if ty == Complex::Any {
                return Complex::Any;
            }*/
            match ty {
                Complex::Any => return Complex::Any,
                Complex::OneOf(oset) => {
                    set.extend(oset.iter().cloned());
                }
                _ => {
                    set.insert(ty.clone());
                }
            }
        }

        match set.len() {
            0 => panic!("oneof with no types?"),
            1 => set.into_iter().next().unwrap(),
            _ => Complex::OneOf(set),
        }
    }

    // term any is overloaded.
    // in this case it means any ty that needs RTTI
    pub fn is_any(&self) -> bool {
        match self {
            Complex::Any | Complex::OneOf(_) => true,
            _ => false,
        }
    }

    pub fn contains(&self, prim: Primitive) -> bool {
        match self {
            Complex::Any => true,
            Complex::Primitive(my_prim) => *my_prim == prim,
            Complex::OneOf(tys) => tys.iter().any(|ty| ty.contains(prim)),
            _ => false,
        }
    }

    // BAD. TODO: remove, x64 specific
    pub fn get_store_size(&self) -> u64 {
        match self {
            Complex::Any | Complex::OneOf(_) => 16, // discrim + val
            Complex::Primitive(_) => 8,
            _ => panic!("can't get size of {:?}", self),
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
        return Some(*self);
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
