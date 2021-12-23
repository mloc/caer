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

fn main() {
    println!("{:#?}", Foo::get_layout());

    println!("{:#?}", Foo::get_gep_indices(gep_path!(y.sub.fin)));
}

#[pinion_export_funcs(SubsubExports, export_subsub)]
trait SubsubExtern {
    fn foo(&self);
}

impl SubsubExtern for Subsub {
    fn foo(&self) {}
}

export_subsub!(Subsub);
