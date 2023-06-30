mod attr;
mod ctx;
mod derivable;
mod export;
mod func;
mod helpers;
mod ty;

extern crate proc_macro;

use ctx::DeriveCtx;
use derivable::pinion_const_wrap::PinionConstWrapDerivable;
use derivable::pinion_data::PinionDataDerivable;
use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(PinionData, attributes(pinion))]
pub fn derive_pinion_data(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input);
    let ctx = DeriveCtx::create(input);

    derivable::derive(&PinionDataDerivable, &ctx)
        .unwrap()
        .into()
}

#[proc_macro_derive(PinionConstWrap)]
pub fn derive_pinion_const_wrap(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input);
    let ctx = DeriveCtx::create(input);

    derivable::derive(&PinionConstWrapDerivable, &ctx)
        .unwrap()
        .into()
}

#[proc_macro_attribute]
pub fn pinion_export(_: TokenStream, item: TokenStream) -> TokenStream {
    export::build_export_func(syn::parse(item).unwrap()).into()
}

#[proc_macro]
pub fn pinion_module(input: TokenStream) -> TokenStream {
    export::build_module_export(syn::parse(input).unwrap()).into()
}
