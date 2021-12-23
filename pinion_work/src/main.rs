use pinion::{gep_path, pinion_export_funcs, PinionData, PinionStruct};

#[derive(PinionData)]
#[pinion(name = "foo")]
#[repr(C)]
struct Foo<'a> {
    x: u8,
    y: Ffff,
    p: &'a Ffff,
    z: f32,
}

#[derive(PinionData)]
#[repr(C)]
struct Ffff {
    sub: Subsub,
}

#[derive(PinionData)]
#[repr(C)]
struct Subsub {
    fin: f64,
}

#[derive(PinionData)]
#[repr(C)]
struct VtMock {
    fn_ptr: extern "C" fn(x: &Ffff) -> &Subsub,
}

#[derive(PinionData)]
#[repr(C)]
struct ExFunc(extern "C" fn(x: &Ffff) -> &Subsub);

#[derive(PinionData, Copy, Clone)]
#[repr(u8)]
enum WhateverState {
    Whee,
    Whoo,
    Fwoo = 7,
    Fwen,
}

fn main() {
    println!("{:#?}", Foo::get_layout());

    println!("{:#?}", Foo::get_gep_indices(gep_path!(y.sub.fin)));
}

#[pinion_export_funcs(SubsubExports, export_subsub)]
trait SubsubExtern {
    fn foo(&self, s: WhateverState);
}

impl SubsubExtern for Subsub {
    fn foo(&self, s: WhateverState) {}
}

export_subsub!(Subsub);
