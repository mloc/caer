pub mod compound;
pub mod ptr;
pub mod scalar;
pub mod traits;

int_type!(Bool, bool_type);
int_type!(Int8, i8_type);
int_type!(Int16, i16_type);
int_type!(Int32, i32_type);
int_type!(Int64, i64_type);

float_type!(Float16, f16_type);
float_type!(Float32, f32_type);
float_type!(Float64, f64_type);

#[test]
fn test() {
    struct_type!(Compound, {Bool, Int8});
    struct_type!(Compound2, {Bool, Compound});

    use inkwell::context::Context;

    use crate::ptr::Ptr;
    use crate::traits::Type;

    type Foo = Ptr<Ptr<Compound2, 0>, 0>;
    //type Foo = Ptr<Bool, 0>;

    println!("{}", Foo::debug_stringify());
    let ctx = Context::create();
    println!("{:?}", Foo::instantiate(&ctx));
}
