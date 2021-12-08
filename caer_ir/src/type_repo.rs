use std::collections::{BTreeSet, HashMap};
use std::ops::Index;

use caer_types::id::{
    TypeId, TYPE_ID_ANY, TYPE_ID_FLOAT, TYPE_ID_LIST, TYPE_ID_REF_ANY, TYPE_ID_STRING,
};
use caer_types::ty::{RefType, Type};
use index_vec::IndexVec;

#[derive(Debug)]
pub struct TypeRepo {
    types: IndexVec<TypeId, Type>,
    lookup: HashMap<Type, TypeId>,
}

impl TypeRepo {
    pub fn new() -> Self {
        // A hack: TypeId 0 is always Any
        let mut s = Self {
            types: IndexVec::new(),
            lookup: HashMap::new(),
        };
        assert_eq!(s.insert(Type::Any), TYPE_ID_ANY);
        assert_eq!(s.insert(Type::Ref(RefType::String)), TYPE_ID_STRING);
        assert_eq!(
            s.insert(Type::Ref(RefType::List(TYPE_ID_ANY))),
            TYPE_ID_LIST
        );
        assert_eq!(s.insert(Type::Float), TYPE_ID_FLOAT);
        assert_eq!(s.insert(Type::Ref(RefType::Any)), TYPE_ID_REF_ANY);
        s
    }

    pub fn insert(&mut self, ty: Type) -> TypeId {
        let lu = &mut self.lookup;
        let tys = &mut self.types;

        *lu.entry(ty).or_insert_with_key(|k| {
            tys.push(k.clone());
            tys.last_idx()
        })
    }

    pub fn insert_sum(&mut self, tys: BTreeSet<TypeId>) -> TypeId {
        let ty = if tys.is_empty() {
            Type::Any
        } else if tys.len() == 1 {
            return tys.into_iter().next().unwrap();
        } else {
            Type::OneOf(tys)
        };
        self.insert(ty)
    }

    pub fn get(&self, id: TypeId) -> &Type {
        // TODO: ERRH
        self.types.get(id).unwrap()
    }

    pub fn get_all(&self) -> IndexVec<TypeId, Type> {
        self.types.clone()
    }

    pub fn iter(&self) -> impl Iterator<Item = (TypeId, &Type)> {
        self.types.iter_enumerated()
    }
}

impl Index<TypeId> for TypeRepo {
    type Output = Type;

    fn index(&self, index: TypeId) -> &Self::Output {
        &self.types[index]
    }
}

impl Default for TypeRepo {
    fn default() -> Self {
        Self::new()
    }
}
