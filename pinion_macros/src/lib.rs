mod attr;
mod data;
mod export;
mod func;
mod ty;

extern crate proc_macro;

use attr::DataAttributes;
use data::DeriveCtx;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{parse_macro_input, parse_quote, DeriveInput};

#[proc_macro_derive(PinionData, attributes(pinion))]
pub fn derive_data(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input);

    let ctx = DeriveCtx::create(input);
    ctx.derive().into()
}

#[proc_macro_attribute]
pub fn pinion_export_funcs(attr: TokenStream, item: TokenStream) -> TokenStream {
    let attr_tok: TokenStream2 = attr.into();
    let args = parse_quote! { #attr_tok }; // meh, easy
    export::build_export_funcs(args, syn::parse(item).unwrap()).into()
}
