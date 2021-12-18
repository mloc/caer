mod attr;

extern crate proc_macro;

use attr::StructAttributes;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{parse_macro_input, parse_quote, DeriveInput, Type};

fn build_type(ctx: &syn::Expr, ty: &Type) -> syn::Expr {
    match ty {
        Type::Path(path) => {
            parse_quote! { <#path as pinion::PinionBasicType>::create_in_context(#ctx) }
        },
        Type::Reference(ptr) => {
            parse_quote! { <#ptr as pinion::PinionBasicType>::create_in_context(#ctx) }
        },
        Type::Ptr(ptr) => {
            parse_quote! { <#ptr as pinion::PinionBasicType>::create_in_context(#ctx) }
        },
        Type::BareFn(bare_fn) => {
            assert_eq!(bare_fn.abi, Some(parse_quote! { extern "C" }));
            let param_types = bare_fn.inputs.iter().map(|arg| build_type(ctx, &arg.ty));
            let ret_type: syn::Expr = match &bare_fn.output {
                syn::ReturnType::Default => parse_quote! {None},
                syn::ReturnType::Type(_, ty) => {
                    let ty = build_type(ctx, ty);
                    parse_quote! {Some(#ty)}
                },
            };

            parse_quote! {
                {
                    let params = [#(#param_types,)*];
                    let ret_ty = #ret_type;
                    let func = #ctx.make_function_type(&params, ret_ty);
                    #ctx.make_func_ptr_type(func)
                }
            }
        },
        _ => todo!("can't synth type {:?}", ty),
    }
}

fn build_layout(ty: &Type) -> syn::Expr {
    match ty {
        Type::Path(path) => {
            parse_quote! { <#path as pinion::PinionBasicType>::get_layout() }
        },
        Type::Reference(ptr) => {
            parse_quote! { <#ptr as pinion::PinionBasicType>::get_layout() }
        },
        Type::Ptr(ptr) => {
            parse_quote! { <#ptr as pinion::PinionBasicType>::get_layout() }
        },
        Type::BareFn(_bare_fn) => {
            parse_quote! { <pinion::PinionFuncPtr as pinion::PinionBasicType>::get_layout() }
        },
        _ => todo!("can't make layout {:?}", ty),
    }
}

fn ident_to_litstr(ident: &syn::Ident) -> syn::LitStr {
    syn::LitStr::new(&ident.to_string(), ident.span())
}

#[proc_macro_derive(PinionStruct, attributes(pinion))]
pub fn derive_struct(input: TokenStream) -> TokenStream {
    let DeriveInput {
        ident,
        data,
        generics,
        attrs,
        ..
    } = parse_macro_input!(input);

    let out = match data {
        syn::Data::Struct(s) => match s.fields {
            syn::Fields::Named(f) => {
                let fields: Vec<_> = f.named.iter().cloned().collect();
                derive_struct_named(&ident, &generics, &attrs, &fields)
            },
            syn::Fields::Unnamed(f) => {
                let fields: Vec<_> = f.unnamed.iter().cloned().collect();
                derive_struct_newtype(&ident, &generics, &fields)
            },
            _ => panic!("struct must have fields"),
        },
        _ => panic!("can only derive for structs"),
    };

    out.into()
}

fn derive_struct_named(
    ident: &syn::Ident, generics: &syn::Generics, attrs: &[syn::Attribute], fields: &[syn::Field],
) -> TokenStream2 {
    let attrs = StructAttributes::from_attrs(attrs).unwrap();
    let ctxq: syn::Expr = parse_quote! { ctx };
    let fq = fields.iter().map(|f| build_type(&ctxq, &f.ty));

    let field_names = fields
        .iter()
        .map(|f| ident_to_litstr(f.ident.as_ref().unwrap()));
    let field_layouts = fields.iter().map(|f| build_layout(&f.ty));

    let name = attrs.name().unwrap_or("");
    let packed = attrs.packed();

    let once_cell: syn::Path = parse_quote! { pinion::rex::once_cell };
    let layout: syn::Path = parse_quote! { pinion::types::layout };

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    quote! {
        #[automatically_derived]
        impl #impl_generics pinion::PinionBasicType for #ident #ty_generics #where_clause {
            fn create_in_context<C: pinion::Context>(ctx: &mut C) -> C::BasicType {
                let fields = [#(#fq,)*];
                ctx.make_struct_type(&fields, #packed, #name)
            }

            fn get_layout() -> &'static #layout::CycleCell {
                static LAYOUT: #once_cell::sync::OnceCell<#layout::CycleCell> = #once_cell::sync::OnceCell::new();

                if let Some(v) = LAYOUT.get() {
                    return v
                }

                match LAYOUT.set(#layout::CycleCell::new_empty()) {
                    Ok(()) => {},
                    Err(_) => panic!(),
                }

                let fields = [#((#field_names,#field_layouts),)*];
                let struct_layout = #layout::StructLayout::new(&fields);
                let lty = #layout::BasicType::Struct(struct_layout);

                let layout = LAYOUT.get().unwrap();
                layout.update(lty);
                layout
            }
        }
        #[automatically_derived]
        impl #impl_generics pinion::PinionStruct for #ident #ty_generics #where_clause {}
    }
}

fn derive_struct_newtype(
    ident: &syn::Ident, generics: &syn::Generics, fields: &[syn::Field],
) -> TokenStream2 {
    assert_eq!(fields.len(), 1);

    let s_ty = &fields.first().unwrap().ty;

    let ctxq: syn::Expr = parse_quote! { ctx };

    let layout_b = build_layout(s_ty);
    let ctxty = build_type(&ctxq, s_ty);

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    quote! {
        #[automatically_derived]
        impl #impl_generics pinion::PinionBasicType for #ident #ty_generics #where_clause {
            fn create_in_context<C: pinion::Context>(ctx: &mut C) -> C::BasicType {
                #ctxty
            }

            fn get_layout() -> &'static pinion::types::layout::CycleCell {
                #layout_b
            }
        }
        impl #impl_generics pinion::PinionStruct for #ident #ty_generics #where_clause {}
    }
}
