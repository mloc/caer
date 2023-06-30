use proc_macro2::TokenStream;
use quote::quote;

use super::Derivable;
use crate::ctx::DeriveCtx;

pub struct PinionConstWrapDerivable;

impl Derivable for PinionConstWrapDerivable {
    fn derive_struct_named(
        &self, ctx: &DeriveCtx, fields: &[syn::Field],
    ) -> syn::Result<TokenStream> {
        Self::derive_struct(ctx, fields)
    }

    fn derive_struct_unnamed(
        &self, ctx: &DeriveCtx, fields: &[syn::Field],
    ) -> syn::Result<TokenStream> {
        Self::derive_struct(ctx, fields)
    }

    fn derive_unit_enum(
        &self, ctx: &DeriveCtx, _: &syn::DataEnum, disc_ty: &syn::Type,
    ) -> syn::Result<TokenStream> {
        Self::derive_unit_enum(ctx, disc_ty)
    }

    fn derive_field_enum(
        &self, ctx: &DeriveCtx, enum_data: &syn::DataEnum, disc_ty: &syn::Type,
    ) -> syn::Result<TokenStream> {
        Self::derive_field_enum(ctx, enum_data, disc_ty)
    }
}

impl PinionConstWrapDerivable {
    // Named and unnamed structs are handled the same.
    fn derive_struct(ctx: &DeriveCtx, fields: &[syn::Field]) -> syn::Result<TokenStream> {
        let ident = &ctx.ident;

        let field_wraps = fields.iter().map(|field| {
            let field_ident = &field.ident;
            let field_ty = &field.ty;
            quote!(<#field_ty as pinion::PinionConstWrap>::const_wrap(&self.#field_ident, lctx))
        });

        Ok(quote! {
            #[automatically_derived]
            impl pinion::PinionConstWrap for #ident {
                fn const_wrap(&self, lctx: &mut pinion::layout_ctx::LayoutCtx) -> pinion::ConstItem {
                    let field_items = [#(#field_wraps,)*].into();
                    pinion::ConstItem::make_for::<Self>(lctx, pinion::ConstValue::Struct(field_items))
                }
            }
        })
    }

    fn derive_unit_enum(ctx: &DeriveCtx, disc_ty: &syn::Type) -> syn::Result<TokenStream> {
        let ident = &ctx.ident;

        Ok(quote! {
            #[automatically_derived]
            impl pinion::PinionConstWrap for #ident where #ident : pinion::PinionEnum {
                fn const_wrap(&self, lctx: &mut pinion::layout_ctx::LayoutCtx) -> pinion::ConstItem {
                    let c = <Self as Clone>::clone(self);
                    let disc_item = <#disc_ty as pinion::PinionConstWrap>::const_wrap(&(c as #disc_ty), lctx);
                    let const_value = pinion::ConstValue::Enum(Box::new(disc_item));
                    pinion::ConstItem::make_for::<Self>(lctx, const_value)
                }
            }
        })
    }

    fn derive_field_enum(
        ctx: &DeriveCtx, enum_data: &syn::DataEnum, disc_ty: &syn::Type,
    ) -> syn::Result<TokenStream> {
        let ident = &ctx.ident;

        let field_mappers = enum_data.variants.iter().map(|v| {
            let field_ident = &v.ident;
            // TODO: this logic is repeated in a bunch of places, clean up
            let rhs = match &v.fields {
                syn::Fields::Named(_) => unimplemented!(),
                syn::Fields::Unnamed(f) => {
                    assert_eq!(f.unnamed.len(), 1);
                    let f = f.unnamed.first().unwrap();
                    quote!(Some(pinion::PinionConstWrap::const_wrap(x, lctx)))
                },
                syn::Fields::Unit => quote!(None),
            };
            let field_q = (!v.fields.is_empty()).then(|| quote!((x)));
            quote!(Self::#field_ident #field_q => #rhs)
        });

        Ok(quote! {
            #[automatically_derived]
            impl pinion::PinionConstWrap for #ident {
                fn const_wrap(&self, lctx: &mut pinion::layout_ctx::LayoutCtx) -> pinion::ConstItem {
                    let tag = pinion::PinionTaggedUnion::as_tag(self);
                    let tag_prim = pinion::PinionEnum::to_disc(tag);
                    let tag_wrap = pinion::PinionConstWrap::const_wrap(&tag_prim, lctx);
                    let field_wrap = match self {
                        #(#field_mappers,)*
                    };
                    let const_value = pinion::ConstValue::TaggedUnion(Box::new((tag_wrap, field_wrap)));
                    pinion::ConstItem::make_for::<Self>(lctx, const_value)
                }
            }
        })
    }
}
