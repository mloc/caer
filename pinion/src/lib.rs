pub mod composite;
pub mod func;
pub mod indexer;
pub mod ptr;
pub mod scalar;
pub mod traits;
pub mod types;

#[cfg(test)]
mod test {
    use inkwell::context::Context;

    use crate::ptr::Ptr;
    use crate::traits::{PinionBasicType, PinionType};
    use crate::types::*;
    use crate::{func_type, index, struct_type};

    struct_type! {Compound { a: Bool, b: Int8 }, false}
    struct_type! {
        Compound2 {
            a: Bool,
            b: Compound
        },
        false
    }
    #[test]
    fn test() {
        func_type!(Bar(Bool, Compound) -> Int16);

        type Foo = Ptr<Ptr<Compound2, 0>, 0>;
        //type Foo = Ptr<Bool, 0>;

        println!("{}", Foo::debug_stringify());
        let ctx = Context::create();
        println!("{:?}", Foo::instantiate(&ctx));

        println!("{}", Bar::debug_stringify());
        println!("{:?}", Bar::instantiate(&ctx));

        let x = Ptr::<Ptr<Int8, 0>, 0>::create_empty();
        let y = x.resolve_index("*").unwrap().1.resolve_index("*");
        assert!(y.is_some());
        assert!(y.unwrap().1.resolve_index("*").is_none());
        println!("{:?}", index!(Ptr<Compound2, 0>, *.b.a));
    }
}
