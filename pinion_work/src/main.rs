use std::marker::PhantomData;

use pinion::layout_ctx::LayoutCtx;
use pinion::{
    gep_path, pinion_export, pinion_module, PinionConstWrap, PinionData, PinionField, PinionStruct,
    PinionValueHolder,
};

#[repr(C)]
#[derive(PinionData, PinionConstWrap)]
struct Foo {
    a: bool,
    b: Bar,
}

#[repr(C)]
#[derive(PinionData, PinionConstWrap)]
struct Bar {
    count: i64,
}

#[repr(C, u8)]
#[derive(PinionData, PinionConstWrap)]
enum Bcwar {
    A(i8),
    B(Bar),
    E,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PinionData, PinionConstWrap)]
enum Sunit {
    E,
    F,
    J,
}

#[pinion_export]
pub fn expo(a: u8) {}

pinion_module! {
    Whee,
    [
        crate::expo,
    ]
}

struct Valb<T> {
    v: T,
}

impl<T: PinionData + std::fmt::Debug> PinionValueHolder<T> for Valb<T> {
    type Reified = String;

    fn reify(&self) -> Self::Reified {
        format!("{:?}", self.v)
    }
}

fn main() {
    let mut ctx = LayoutCtx::new();
    let id = ctx.populate::<Foo>();
    println!("{:#?}", ctx.get(id));

    let x = Foo {
        a: false,
        b: Bar { count: 3939 },
    };
    println!("{:#?}", x.const_wrap(&mut ctx));

    type Ef = <Whee as pinion::PinionModule>::TFuncs;

    let x = Ef::expo().bind::<String>(Valb { v: 8 });
    let y = <Whee as pinion::PinionModule>::get_bundle_func(&x);
    assert_eq!(y, <Whee as pinion::PinionModule>::Funcs::expo);
}

/*#[derive(PinionData)]
#[repr(C)]
struct Foo<P> {
    a: u8,
    b: Option<*const P>,
}

pub fn main() {
    let x = <Foo<u32> as PinionStruct>::Fields::b();
    let s = StateRef::conv(x);
    let y = s.phantom;
}

struct StateRef<T> {
    state: u8,
    phantom: PhantomData<T>,
}

impl<T: PinionData> StateRef<T> {
    fn conv<const N: u32, S: PinionStruct>(field: PinionField<N, S, T>) -> Self {
        Self {
            state: 9,
            phantom: PhantomData,
        }
    }
}

#[derive(PinionData)]
#[repr(C, u8)]
enum FooE {
    A,
    B(u8) = 7,
}*/

/*#[derive(PinionData)]
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

fn hhh(_x: &u8) {
    let mut ctx = LayoutCtx::new();
    <&u8 as PinionData>::get_layout(&mut ctx);
}

#[pinion_export]
fn expo(_x: &u8) {}

pinion_module! {
    Whee,
    [
        crate::expo,
    ]
}*/
