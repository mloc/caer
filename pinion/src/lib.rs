#[allow(unused_imports)]
#[macro_use]
extern crate pinion_macros;
#[doc(hidden)]
pub use pinion_macros::*;

#[derive(PinionStruct)]
#[repr(C)]
struct Foo {
    x: u8,
}
