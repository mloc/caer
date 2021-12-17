mod attr;

extern crate proc_macro;

use attr::StructAttributes;
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, parse_quote, DeriveInput, Type};

fn build_type(ctx: &syn::Expr, ty: &Type) -> syn::Expr {
    match ty {
        Type::Path(path) => {
            parse_quote! { <#path as pinion::PinionBasicType>::create_in_context(#ctx) }
        },
        Type::Reference(ptr) => {
            let elem_type = build_type(ctx, &ptr.elem);
            // TODO: figure out gcptrs
            parse_quote! {
                {
                    let elem = #elem_type;
                    #ctx.make_pointer_type(elem, false)
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
            let elem_ty = &ptr.elem;
            // TODO: figure out gcptrs
            parse_quote! { <pinion::PinionPointer<#elem_ty, false> as pinion::PinionBasicType>::get_layout() }
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

    let attrs = StructAttributes::from_attrs(&attrs).unwrap();

    let fields: Vec<syn::Field> = match data {
        syn::Data::Struct(s) => match s.fields {
            syn::Fields::Named(fields) => fields.named.iter().cloned().collect(),
            _ => panic!("struct must have named fields"),
        },
        _ => panic!("can only derive for structs"),
    };

    let ctxq: syn::Expr = parse_quote! { ctx };
    let fq = fields.iter().map(|f| build_type(&ctxq, &f.ty));

    let field_names = fields
        .iter()
        .map(|f| ident_to_litstr(f.ident.as_ref().unwrap()));
    let field_layouts = fields
        .iter()
        .map(|f| build_layout(&f.ty))
        .map(|e| -> syn::Expr {
            parse_quote! {#e.clone()}
        });

    let name = attrs.name().unwrap_or("");
    let packed = attrs.packed();

    let once_cell: syn::Path = parse_quote! { pinion::rex::once_cell };
    let layout: syn::Path = parse_quote! { pinion::types::layout };

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let output = quote! {
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
        impl #impl_generics pinion::PinionStruct for #ident #ty_generics #where_clause {}
    };

    output.into()
}
