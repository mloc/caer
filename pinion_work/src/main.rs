use pinion::PinionBasicType;

#[derive(PinionBasicType)]
#[pinion(name = "foo")]
#[repr(C)]
struct Foo<'a> {
    x: u8,
    y: Ffff,
    p: &'a Ffff,
    z: f32,
}

#[derive(PinionBasicType)]
#[repr(C)]
struct Ffff {}

fn main() {
    println!("{:#?}", Foo::get_layout());
}
