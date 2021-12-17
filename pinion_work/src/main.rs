use pinion::{gep_path, PinionBasicType, PinionStruct};

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

#[derive(PinionStruct)]
#[repr(C)]
struct VtMock {
    fn_ptr: extern "C" fn(x: &Ffff) -> &Subsub,
}

fn main() {
    println!("{:#?}", Foo::get_layout());

    println!("{:#?}", Foo::get_gep_indices(gep_path!(y.sub.fin)));
}
