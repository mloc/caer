#![recursion_limit="4096"]

extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;
extern crate syn;

use proc_macro::{TokenStream};
use proc_macro2::Span;
use quote::quote;
use syn::spanned::Spanned;
use std::collections::HashSet;
use std::sync::Mutex;

#[proc_macro_attribute]
pub fn expose_c_stubs(attr: TokenStream, input: TokenStream) -> TokenStream {
    let prim_ty: HashSet<syn::Type> = {
        let mut hs = HashSet::new();
        for ts in vec![
            quote! { i8 },
            quote! { i16 },
            quote! { i32 },
            quote! { i64 },
            quote! { i128 },
            quote! { isize },
            quote! { u8 },
            quote! { u16 },
            quote! { u32 },
            quote! { u64 },
            quote! { u128 },
            quote! { usize },
            quote! { f32 },
            quote! { f64 },
            quote! { bool },
        ] {
            let ts1: TokenStream = ts.into();
            let ty: syn::Type = syn::parse(ts1).unwrap();
            hs.insert(ty);
        }
        hs
    };

    let arg: syn::Ident = syn::parse(attr).unwrap();
    let prefix = arg.to_string();
    let item: syn::Item = syn::parse(input).unwrap();

    let mut output = quote!{#item};

    match item {
        syn::Item::Impl(ref impl_item) => {
            //println!("{:#?}", impl_item);
            let impl_ty = &impl_item.self_ty;
            for sub in impl_item.items.iter() {
                if let syn::ImplItem::Method(ref method) = sub {
                    let fn_ident = &method.sig.ident;
                    let expose_name = format!("{}_{}", prefix, fn_ident.to_string());
                    let expose_ident = syn::Ident::new(&expose_name, proc_macro2::Span::call_site());
                    let mut ret_ty = method.sig.decl.output.clone();

                    let mut stub_params = syn::punctuated::Punctuated::<syn::FnArg, syn::token::Comma>::new();
                    let mut call_args = syn::punctuated::Punctuated::<syn::Expr, syn::token::Comma>::new();

                    for param in method.sig.decl.inputs.iter() {
                        match param {
                            syn::FnArg::SelfRef(ref self_ref) => {
                                let mutability = self_ref.mutability;
                                stub_params.push(syn::parse(quote!{__self: *mut #impl_ty}.into()).expect("stub params failure"));
                                call_args.push(syn::parse(quote!{&#mutability *__self}.into()).expect("call args failure"));
                            },

                            syn::FnArg::SelfValue(_) => {
                                stub_params.push(syn::parse(quote!{__self: *mut #impl_ty}.into()).expect("stub params failure"));
                                call_args.push(syn::parse(quote!{*Box::from_raw(__self)}.into()).expect("call args failure"));
                            },

                            syn::FnArg::Captured(ref cap) => {
                                let cap_pat = &cap.pat;
                                let cap_ty = &cap.ty;

                                match cap_ty {
                                    syn::Type::Reference(ref ref_ty) => {
                                        let mutability = ref_ty.mutability;
                                        let ref_ty_ty = &ref_ty.elem;
                                        stub_params.push(syn::parse(quote!{#cap_pat: *mut #ref_ty_ty}.into()).expect("stub params failure"));
                                        call_args.push(syn::parse(quote!{&#mutability *#cap_pat}.into()).expect("call args failure"));
                                    },
                                    syn::Type::Ptr(ref ptr_ty) => {
                                        let const_t = ptr_ty.const_token;
                                        let mut_t = ptr_ty.mutability;
                                        let ty = &ptr_ty.elem;
                                        stub_params.push(syn::parse(quote!{#cap_pat: *#const_t #mut_t #ty}.into()).expect("stub params failure"));
                                        call_args.push(syn::parse(quote!{#cap_pat}.into()).expect("call args failure"));
                                    },
                                    _ => {
                                        let prim = prim_ty.contains(cap_ty);

                                        if !prim {
                                            stub_params.push(syn::parse(quote!{#cap_pat: *mut #cap_ty}.into()).expect("stub params failure"));
                                            call_args.push(syn::parse(quote!{*Box::from_raw(#cap_pat)}.into()).expect("call args failure"));
                                        } else {
                                            stub_params.push(syn::parse(quote!{#cap_pat: #cap_ty}.into()).expect("stub params failure"));
                                            call_args.push(syn::parse(quote!{#cap_pat}.into()).expect("call args failure"));
                                        }

                                    }
                                }

                            },
                            _ => panic!("oh no"),
                        }
                    }

                    let mut res = quote!{
                        #impl_ty::#fn_ident(#call_args)
                    };

                    let mut ret_tok = None;
                    if let syn::ReturnType::Type(_, ref mut ty) = ret_ty {
                        if **ty == syn::parse::<syn::Type>(quote!{Self}.into()).unwrap() {
                            *ty = impl_ty.clone();
                        }

                        let prim = prim_ty.contains(&**ty);
                        let ptr = match **ty {
                            syn::Type::Ptr(_) => true,
                            _ => false,
                        };

                        let mut pt = None;

                        if !ptr && !prim {
                            res = quote!{
                                Box::into_raw(Box::new(#res))
                            };

                            pt = Some(quote!{*mut});
                        }

                        ret_tok = Some(quote!{
                            -> #pt #ty
                        });

                    }

                    let generics = &impl_item.generics;

                    output.extend(quote!{
                        #[no_mangle]
                        pub unsafe extern "C" fn #expose_ident #generics(#stub_params) #ret_tok {
                            #res
                        }
                    });
                }
            }
        },
        _ => {
            // item.span().unstable().error("expose_c_stubs can only be applied to an impl block").emit();
        },
    }

    TokenStream::from(output)
}
