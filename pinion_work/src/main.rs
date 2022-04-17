#![feature(arbitrary_enum_discriminant)]

use pinion::layout_ctx::LayoutCtx;
use pinion::{gep_path, pinion_export, pinion_module, PinionData, PinionStruct};

#[derive(PinionData)]
#[pinion(name = "foo")]
#[repr(C)]
struct Foo {
    p: &'static Ffff,
    x: u8,
    y: Ffff,
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

#[derive(PinionData)]
#[repr(C, u8)]
enum Fenum {
    A(u8) = 7,
    B(u64),
    W = 28,
    C(Ffff),
}

fn main() {
    let mut ctx = LayoutCtx::new();
    let id = ctx.populate::<Foo>();
    println!("{:#?}", ctx.get(id));

    let feid = ctx.populate::<Fenum>();
    println!("{:#?}", ctx.get(feid));

    println!("{:#?}", ctx.get_gep_indices::<Foo>(gep_path!(y.sub.fin)));

    //std::any::TypeId::of::<hhh>();
}

fn sub<'a>() {
    let mut ctx = LayoutCtx::new();
    let feid = ctx.populate::<&Fenum>();
    println!("{:#?}", ctx.get(feid));
}

fn hhh(x: &u8) {
    let mut ctx = LayoutCtx::new();
    <&u8 as PinionData>::get_layout(&mut ctx);
}

#[pinion_export]
fn expo(x: &u8) {}

pinion_module! {
    whee,
    [
        ::expo,
    ]
}
