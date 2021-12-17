use pinion::{PinionBasicType, PinionStruct};

#[derive(PinionStruct)]
#[pinion(name = "foo")]
#[repr(C)]
struct Foo<'a> {
    x: u8,
    y: Ffff,
    p: &'a Ffff,
    z: f32,
}

#[derive(PinionStruct)]
#[repr(C)]
struct Ffff {
    sub: Subsub,
}

#[derive(PinionStruct)]
#[repr(C)]
struct Subsub {
    fin: f64,
}

fn main() {
    println!("{:#?}", Foo::get_layout());

    println!("{:#?}", Foo::get_gep_indices(&["y", "sub", "fin"]));
}
