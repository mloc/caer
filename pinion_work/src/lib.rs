use pinion::PinionStruct;

#[derive(PinionStruct)]
#[repr(C)]
struct Foo {
    x: u8,
    //y: Ffff,
    z: f32,
}

struct Ffff {}
