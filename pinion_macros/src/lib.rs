extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, parse_quote, Attribute, DeriveInput};

#[proc_macro_derive(PinionStruct)]
pub fn derive_struct(input: TokenStream) -> TokenStream {
    let DeriveInput {
        ident, data, attrs, ..
    } = parse_macro_input!(input);

    let mut found_repr = false;

    let repr_c: Attribute = parse_quote! { #[repr(C)] };

    for attr in attrs.iter() {
        if *attr == repr_c {
            found_repr = true;
        }
    }
    if !found_repr {
        panic!("no repr")
    }

    match data {
        syn::Data::Struct(_) => {},
        _ => panic!("can only derive for structs"),
    };

    let output = quote! {
    impl #ident {
        fn foo() {
        println!("it works!");
        }
    }
    };

    output.into()
}
