pub mod composite;
pub mod func;
pub mod ptr;
pub mod scalar;
pub mod traits;
pub mod types;

pub use crate::types::*;

#[test]
fn test() {
    struct_type!(Compound, {Bool, Int8}, false);
    struct_type!(Compound2, {Bool, Compound}, false);

    func_type!(Bar(Bool, Compound) -> Int16);

    use inkwell::context::Context;

    use crate::ptr::Ptr;
    use crate::traits::PinionType;

    type Foo = Ptr<Ptr<Compound2, 0>, 0>;
    //type Foo = Ptr<Bool, 0>;

    println!("{}", Foo::debug_stringify());
    let ctx = Context::create();
    println!("{:?}", Foo::instantiate(&ctx));

    println!("{}", Bar::debug_stringify());
    println!("{:?}", Bar::instantiate(&ctx));
}
