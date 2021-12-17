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
            let elem_layout = build_layout(&ptr.elem);
            // TODO: figure out gcptrs
            parse_quote! {
                {
                    let elem = #elem_layout;
                    let ptr = pinion::types::layout::Pointer::new(elem, false);
                    pinion::types::layout::BasicType::Pointer(ptr)
                }
            }
        },
        _ => todo!("can't make layout {:?}", ty),
    }
}

fn ident_to_litstr(ident: &syn::Ident) -> syn::LitStr {
    syn::LitStr::new(&ident.to_string(), ident.span())
}

#[proc_macro_derive(PinionBasicType, attributes(pinion))]
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
    let field_layouts = fields.iter().map(|f| build_layout(&f.ty));

    let name = attrs.name().unwrap_or("");
    let packed = attrs.packed();

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let output = quote! {
        #[automatically_derived]
        impl #impl_generics pinion::PinionBasicType for #ident #ty_generics #where_clause {
            fn create_in_context<C: pinion::Context>(ctx: &mut C) -> C::BasicType {
                let fields = [#(#fq,)*];
                ctx.make_struct_type(&fields, #packed, #name)
            }

            fn get_layout() -> pinion::types::layout::BasicType {
                let fields = [#((#field_names,#field_layouts),)*];
                let struct_layout = pinion::types::layout::StructLayout::new(&fields);
                pinion::types::layout::BasicType::Struct(struct_layout)
            }
        }
    };

    output.into()
}
