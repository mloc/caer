use pinion::PinionStruct;

#[derive(PinionStruct)]
#[repr(C)]
struct Foo<'a> {
    x: u8,
    y: Ffff,
    p: &'a Ffff,
    z: f32,
}

#[derive(PinionStruct)]
#[repr(C)]
struct Ffff {}
