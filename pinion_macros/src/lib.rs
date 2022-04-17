mod attr;
mod data;
mod export;
mod func;
mod ty;

extern crate proc_macro;

use data::DeriveCtx;
use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(PinionData, attributes(pinion))]
pub fn derive_data(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input);

    let ctx = DeriveCtx::create(input);
    ctx.derive().into()
}

#[proc_macro_attribute]
pub fn pinion_export(_: TokenStream, item: TokenStream) -> TokenStream {
    export::build_export_func(syn::parse(item).unwrap()).into()
}

#[proc_macro]
pub fn pinion_module(input: TokenStream) -> TokenStream {
    export::build_module_export(syn::parse(input).unwrap()).into()
}
