mod attr;

extern crate proc_macro;
use std::collections::HashMap;

use attr::StructAttributes;
use pinion_types::Primitive;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use syn::{parse_macro_input, parse_quote, DeriveInput, Type};

#[derive(Debug)]
enum Field {
    Prim(Primitive),
    Type(Type),
}

fn make_prim_lookup() -> HashMap<Type, Primitive> {
    [
        (parse_quote! {bool}, Primitive::Bool),
        (parse_quote! {i8}, Primitive::Int8),
        (parse_quote! {u8}, Primitive::Int8),
        (parse_quote! {i16}, Primitive::Int16),
        (parse_quote! {u16}, Primitive::Int16),
        (parse_quote! {i32}, Primitive::Int32),
        (parse_quote! {u32}, Primitive::Int32),
        (parse_quote! {i64}, Primitive::Int64),
        (parse_quote! {u64}, Primitive::Int64),
        (parse_quote! {f32}, Primitive::Float32),
        (parse_quote! {f64}, Primitive::Float64),
    ]
    .into()
}

fn quote_primitive(prim: Primitive) -> syn::Expr {
    let variant = match prim {
        Primitive::Bool => quote! {Bool},
        Primitive::Int8 => quote! {Int8},
        Primitive::Int16 => quote! {Int16},
        Primitive::Int32 => quote! {Int32},
        Primitive::Int64 => quote! {Int64},
        Primitive::Float16 => quote! {Float16},
        Primitive::Float32 => quote! {Float32},
        Primitive::Float64 => quote! {Float64},
    };

    parse_quote! { pinion::types::Primitive::#variant }
}

fn create_type_for_type(ctx: &syn::Expr, ty: &Type) -> syn::Expr {
    match ty {
        Type::Path(path) => {
            parse_quote! { <#path as pinion::PinionStruct>::create_in_context(#ctx) }
        },
        Type::Reference(ptr) => {
            let elem_type = create_type_for_type(ctx, &ptr.elem);
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

fn create_field_type(ctx: &syn::Expr, field: &Field) -> syn::Expr {
    match field {
        Field::Prim(prim) => {
            let pq = quote_primitive(*prim);
            parse_quote! { #ctx.make_primitive_type(#pq) }
        },
        Field::Type(ty) => create_type_for_type(ctx, ty),
    }
}

impl ToTokens for Field {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            Field::Prim(p) => {
                let x = format!("{:?}", p);
                quote! {
                    #x
                }
                .to_tokens(tokens);
            },
            Field::Type(t) => t.to_tokens(tokens),
        }
    }
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

    let prim_lookup = make_prim_lookup();

    let mut vf = Vec::new();

    match data {
        syn::Data::Struct(s) => match s.fields {
            syn::Fields::Named(fields) => {
                for field in fields.named {
                    if let Some(p) = prim_lookup.get(&field.ty) {
                        vf.push(Field::Prim(*p))
                    } else {
                        vf.push(Field::Type(field.ty))
                    }
                }
            },
            _ => panic!(),
        },
        _ => panic!("can only derive for structs"),
    };

    let ctxq: syn::Expr = parse_quote! { ctx };
    let fq = vf.iter().map(|f| create_field_type(&ctxq, f));

    let name = attrs.name().unwrap_or("");
    let packed = attrs.packed();

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let output = quote! {
        #[automatically_derived]
        impl #impl_generics pinion::PinionStruct for #ident #ty_generics #where_clause {
            fn create_in_context<C: pinion::Context>(ctx: &mut C) -> C::BasicType {
                let fields = [#(#fq,)*];
                ctx.make_struct_type(&fields, #packed, #name)
            }
        }
    };

    output.into()
}
