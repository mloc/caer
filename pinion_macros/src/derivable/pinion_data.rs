use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::parse_quote;

use super::Derivable;
use crate::ctx::DeriveCtx;
use crate::helpers::{ident_to_litstr, make_impl_bounds, make_ty_static, phantom_generics};
use crate::ty;
pub struct PinionDataDerivable;

impl Derivable for PinionDataDerivable {
    fn derive_struct_named(
        &self, ctx: &DeriveCtx, fields: &[syn::Field],
    ) -> syn::Result<TokenStream> {
        Ok(Self::derive_named(ctx, fields))
    }

    // TODO: newtype handling needs a rework, probably with an attribute
    fn derive_struct_unnamed(
        &self, ctx: &DeriveCtx, fields: &[syn::Field],
    ) -> syn::Result<TokenStream> {
        Ok(Self::derive_newtype(ctx, fields))
    }

    fn derive_unit_enum(
        &self, ctx: &DeriveCtx, enum_data: &syn::DataEnum, disc_ty: &syn::Type,
    ) -> syn::Result<TokenStream> {
        Ok(Self::derive_unit_enum(ctx, enum_data, disc_ty, &ctx.ident))
    }

    fn derive_field_enum(
        &self, ctx: &DeriveCtx, enum_data: &syn::DataEnum, disc_ty: &syn::Type,
    ) -> syn::Result<TokenStream> {
        Ok(Self::derive_enum(ctx, enum_data, disc_ty))
    }
}

impl PinionDataDerivable {
    pub fn derive_named(ctx: &DeriveCtx, fields: &[syn::Field]) -> TokenStream {
        let lctxq: syn::Ident = parse_quote! { lctx };

        let ident = &ctx.ident;
        let ident_str = ident.to_string();
        let vis = &ctx.vis;

        let (impl_generics, ty_generics, _) = ctx.generics.split_for_impl();
        let static_params = make_ty_static(&ty_generics);
        let where_clause = make_impl_bounds(&ctx.generics);
        let phantom_generics = phantom_generics(&ty_generics);

        let field_names = fields
            .iter()
            .map(|f| ident_to_litstr(f.ident.as_ref().unwrap()));
        let field_layouts = fields
            .iter()
            .map(|f| -> syn::Expr { ty::populate_ty(&lctxq, &f.ty) });
        let field_funcs = fields.iter().enumerate().map(|(i, f)| -> _ {
            let field_ident = &f.ident;
            let ty = &f.ty;
            let i_u32 = i as u32;
            quote! {
                pub fn #field_ident() -> pinion::PinionField<#i_u32, #ident #ty_generics, #ty> #where_clause {
                    unsafe { pinion::PinionField::create() }
                }
            }
        });

        let field_validates = fields.iter().map(|f| {
            let field_ident = f.ident.as_ref().unwrap();
            let ptr = parse_quote! {(&(*sptr).#field_ident) as *const _ as *const u8};
            ty::build_validate(&ptr, &f.ty)
        });

        let field_ty_name = syn::Ident::new(
            &format!("__PINION_FIELD_ENUM__{}", ctx.ident),
            Span::call_site(),
        );

        quote! {
            #[automatically_derived]
            impl #impl_generics pinion::PinionData for #ident #ty_generics #where_clause {
                type Static = #ident #static_params;

                fn get_layout(#lctxq: &mut pinion::layout_ctx::LayoutCtx) -> pinion::layout::Layout {
                    let fields = [#((#field_names,#field_layouts),)*];
                    let struct_layout = pinion::layout::StructLayout::new(Some(#ident_str), &fields);
                    pinion::layout::Layout::Struct(struct_layout)
                }

                unsafe fn validate(ptr: *const u8) {
                    let sptr = ptr as *const Self;

                    #(#field_validates;)*
                }
            }
            #[automatically_derived]
            impl #impl_generics pinion::PinionStruct for #ident #ty_generics #where_clause {
                type Fields = #field_ty_name #ty_generics;
            }

            #[doc(hidden)]
            #[allow(non_camel_case_type)]
            #vis enum #field_ty_name #ty_generics {
                _never(std::convert::Infallible, std::marker::PhantomData #phantom_generics)
            }

            #[automatically_derived]
            impl #ty_generics pinion::PinionStructFields for #field_ty_name #ty_generics {}

            impl #ty_generics #field_ty_name #ty_generics {
                #(#field_funcs)*
            }
        }
    }

    fn derive_newtype(ctx: &DeriveCtx, fields: &[syn::Field]) -> TokenStream {
        assert_eq!(fields.len(), 1);

        let s_ty = &fields.first().unwrap().ty;

        let lctxq: syn::Ident = parse_quote! { lctx };

        let inner_data = ty::populate_ty(&lctxq, s_ty);
        let validate_body = ty::build_validate(&parse_quote! { ptr }, s_ty);
        let ident = &ctx.ident;

        let (impl_generics, ty_generics, _) = ctx.generics.split_for_impl();
        let static_params = make_ty_static(&ty_generics);
        let where_clause = make_impl_bounds(&ctx.generics);

        quote! {
            #[automatically_derived]
            impl #impl_generics pinion::PinionData for #ident #ty_generics #where_clause {
                type Static = #ident #static_params;

                fn get_layout(#lctxq: &mut pinion::layout_ctx::LayoutCtx) -> pinion::layout::Layout {
                    let id = #inner_data;
                    (*#lctxq.get(id).unwrap()).clone()
                }

                unsafe fn validate(ptr: *const u8) {
                    #validate_body
                }
            }
        }
    }

    // Create a unit (fieldless) enum from this enum, and derive the right bits for it.
    fn materialize_enum(
        ctx: &DeriveCtx, enum_data: &syn::DataEnum, name: &syn::Ident, prim: &syn::Type,
    ) -> TokenStream {
        let variants = enum_data.variants.iter().map(|v| syn::Variant {
            attrs: vec![],
            ident: v.ident.clone(),
            fields: syn::Fields::Unit,
            discriminant: v.discriminant.clone(),
        });
        let vis = &ctx.vis;

        let derives = Self::derive_unit_enum(ctx, enum_data, prim, name);

        quote! {
            #[repr(#prim)]
            #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
            #vis enum #name {
                #(#variants,)*
            }

            #derives
        }
    }

    fn derive_unit_enum(
        ctx: &DeriveCtx, enum_data: &syn::DataEnum, disc_ty: &syn::Type, name: &syn::Ident,
    ) -> TokenStream {
        let disc_const_names: Vec<syn::Ident> = (0..enum_data.variants.len())
            .map(|i| syn::Ident::new(&format!("DISC_{}", i), Span::call_site()))
            .collect();

        let mut disc_values: Vec<syn::Expr> = vec![];
        let mut disc_values_reverse: Vec<syn::Expr> = vec![];
        for (i, v) in enum_data.variants.iter().enumerate() {
            let v_ident = &v.ident;
            let i_expr: syn::Expr = parse_quote! { #i };
            disc_values.push(parse_quote! { #name::#v_ident as #disc_ty });
            disc_values_reverse.push(parse_quote! { (#name::#v_ident as u64, #i_expr) });
        }

        let ident_str = name.to_string();

        // dude your unit enum shouldn't have generics ok? ok
        quote! {
            #[automatically_derived]
            impl pinion::PinionData for #name {
                type Static = Self;

                fn get_layout(lctx: &mut pinion::layout_ctx::LayoutCtx) -> pinion::layout::Layout {
                    let enum_layout = pinion::layout::Enum {
                        name: Some(#ident_str),
                        disc_layout: lctx.populate::<#disc_ty>(),
                        disc_values: vec![#(#disc_values as u64,)*],
                        disc_values_reverse: [#(#disc_values_reverse,)*].into(),
                    };
                    pinion::layout::Layout::Enum(enum_layout)
                }

                unsafe fn validate(ptr: *const u8) {
                    #(const #disc_const_names: #disc_ty = #disc_values;)*

                    let disc: #disc_ty = *(ptr as *const #disc_ty);
                    match disc {
                        #(#disc_const_names)|* => {},
                        _ => panic!(),
                    }
                }
            }
            #[automatically_derived]
            impl pinion::PinionEnum for #name {
                type Disc = #disc_ty;

                fn to_disc(self) -> Self::Disc {
                    self as #disc_ty
                }
            }
        }
    }

    // wow so many verbs, is this a thesaurus?
    // organise!!!
    fn build_union(ctx: &DeriveCtx, enum_data: &syn::DataEnum, name: &syn::Ident) -> TokenStream {
        let vis = &ctx.vis;
        let name_str = name.to_string();

        let mut variant_names: Vec<&syn::Ident> = vec![];
        let mut variant_tys: Vec<&syn::Type> = vec![];

        let unit_ty: syn::Type = parse_quote! { () };
        for v in enum_data.variants.iter() {
            variant_names.push(&v.ident);
            match &v.fields {
                syn::Fields::Named(_) => todo!(),
                syn::Fields::Unnamed(u) => {
                    assert_eq!(
                        u.unnamed.len(),
                        1,
                        "union can't handle variant with >1 types"
                    );
                    let ty = &u.unnamed[0].ty;
                    variant_tys.push(ty);
                },
                syn::Fields::Unit => {
                    variant_tys.push(&unit_ty);
                },
            }
        }

        assert!(!variant_tys.is_empty());

        let generics = &ctx.generics;

        quote! {
            #[repr(C)]
            #vis union #name #generics {
                #(#variant_names: std::mem::ManuallyDrop<#variant_tys>,)*
            }

            #[automatically_derived]
            impl pinion::PinionData for #name {
                type Static = Self;

                fn get_layout(lctx: &mut pinion::layout_ctx::LayoutCtx) -> pinion::layout::Layout {
                    let unit_layout_id = lctx.populate::<()>();
                    let union_layout = pinion::layout::Union {
                        name: Some(#name_str),
                        layouts: vec![#(lctx.populate::<#variant_tys>(),)*],
                        size: std::mem::size_of::<Self>(),
                        alignment: std::mem::align_of::<Self>(),
                    };
                    pinion::layout::Layout::Union(union_layout)
                }
                unsafe fn validate(ptr: *const u8) {
                    // There's no validation we can do for unions.
                }
            }

            #[automatically_derived]
            impl pinion::PinionUnion for #name {}
        }
    }

    fn derive_enum(ctx: &DeriveCtx, enum_data: &syn::DataEnum, disc_ty: &syn::Type) -> TokenStream {
        // We create a unit enum to act as the tag, and a union marker type for the fields.
        // TODO: names from attr
        let tag_enum_name = syn::Ident::new(&format!("{}Tag", ctx.ident), ctx.ident.span());
        let field_union_name = syn::Ident::new(&format!("{}Union", ctx.ident), ctx.ident.span());

        let tag_enum_decl = Self::materialize_enum(ctx, enum_data, &tag_enum_name, disc_ty);
        let field_union_decl = Self::build_union(ctx, enum_data, &field_union_name);

        // Sanity check: currently we only handle unnamed fields with a single type
        // TODO(ERRH)
        enum_data.variants.iter().for_each(|v| match &v.fields {
            syn::Fields::Named(_) => todo!(),
            syn::Fields::Unnamed(u) => assert_eq!(u.unnamed.len(), 1, "enum has >1 types"),
            syn::Fields::Unit => {},
        });

        let as_tag_mappers = enum_data.variants.iter().map(|v| {
            let ident = &v.ident;
            let field_q = (!v.fields.is_empty()).then(|| quote!((..)));
            quote!(Self::#ident #field_q => Self::Tag::#ident)
        });

        let ident = &ctx.ident;
        let ident_str = ident.to_string();
        let (impl_generics, ty_generics, _) = ctx.generics.split_for_impl();
        let static_params = make_ty_static(&ty_generics);
        let where_clause = make_impl_bounds(&ctx.generics);

        quote! {
            #[automatically_derived]
            impl #impl_generics pinion::PinionData for #ident #ty_generics #where_clause {
                type Static = #ident #static_params;

                fn get_layout(lctx: &mut pinion::layout_ctx::LayoutCtx) -> pinion::layout::Layout {
                    let layout = pinion::layout::TaggedUnion {
                        name: Some(#ident_str),
                        tag_layout: lctx.populate::<#tag_enum_name>(),
                        union_layout: lctx.populate::<#field_union_name>(),
                    };
                    pinion::layout::Layout::TaggedUnion(layout)
                }

                unsafe fn validate(ptr: *const u8) {
                    // TODO: more safety justification here
                    <#tag_enum_name as pinion::PinionData>::validate(ptr);
                }
            }
            #[automatically_derived]
            impl #impl_generics pinion::PinionTaggedUnion for #ident #ty_generics #where_clause {
                type Tag = #tag_enum_name;
                type Union = #field_union_name;

                fn as_tag(&self) -> Self::Tag {
                    match self {
                        #(#as_tag_mappers,)*
                    }
                }
            }

            #tag_enum_decl

            #field_union_decl
        }
    }
}
