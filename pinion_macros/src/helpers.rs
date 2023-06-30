use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::parse::{ParseStream, Parser};
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::{parse_quote, GenericParam, Token};

pub fn ident_to_litstr(ident: &syn::Ident) -> syn::LitStr {
    syn::LitStr::new(&ident.to_string(), ident.span())
}

pub fn make_ty_static(ty_generics: &syn::TypeGenerics) -> TokenStream {
    let ts = ty_generics.to_token_stream();
    if ts.is_empty() {
        return ts;
    }

    let params = parse_spec.parse2(ts).expect("weewhooo");

    let tss = params.iter().map(|param| match param {
        GenericParam::Lifetime(_) => parse_quote! {'static},
        GenericParam::Type(ty) => quote! {<#ty as pinion::PinionData> :: Static},
        GenericParam::Const(_) => panic!("can't handle const"),
    });

    quote! {
        < #(#tss,)* >
    }
}

pub fn phantom_generics(ty_generics: &syn::TypeGenerics) -> TokenStream {
    let ts = ty_generics.to_token_stream();
    if ts.is_empty() {
        return quote! { <()> };
    }

    let params = parse_spec.parse2(ts).expect("weewhooo");

    let tss = params.iter().map(|param| match param {
        GenericParam::Lifetime(lt) => parse_quote! { & #lt () },
        GenericParam::Type(ty) => quote! { #ty },
        GenericParam::Const(_) => panic!("can't handle const"),
    });

    quote! {
        < ( #(#tss),* ) >
    }
}

pub fn parse_spec(input: ParseStream) -> Result<Punctuated<syn::GenericParam, Comma>, syn::Error> {
    let _: Token![<] = input.parse()?;
    let res = Punctuated::<GenericParam, Comma>::parse_separated_nonempty(input)?;
    let _: Token![>] = input.parse()?;
    Ok(res)
}

pub fn make_impl_bounds(generics: &syn::Generics) -> TokenStream {
    if generics.params.is_empty() {
        return quote! {};
    }
    let mut where_clauses: Vec<_> = generics
        .where_clause
        .as_ref()
        .map(|c| c.predicates.iter().cloned().collect())
        .unwrap_or_default();

    for param in generics.params.iter() {
        if let GenericParam::Type(ty) = param {
            let ident = &ty.ident;
            where_clauses.push(parse_quote! { #ident: pinion::PinionData })
        }
    }

    quote! {
        where #(#where_clauses),*
    }
}
