// jinxing GAT stabilization
#![feature(generic_associated_types)]

pub mod interface;
pub mod layout;
pub mod layout_ctx;
pub mod traits;
pub mod types;

#[allow(unused_imports)]
#[macro_use]
extern crate pinion_macros;
pub use interface::*;
#[doc(hidden)]
pub use pinion_macros::*;
pub use traits::*;

pub mod rex {
    pub use once_cell;
}
