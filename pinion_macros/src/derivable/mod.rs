pub mod pinion_const_wrap;
pub mod pinion_data;

use proc_macro2::TokenStream;

use crate::ctx::DeriveCtx;

pub trait Derivable {
    fn derive_struct_named(
        &self, ctx: &DeriveCtx, fields: &[syn::Field],
    ) -> syn::Result<TokenStream>;

    fn derive_struct_unnamed(
        &self, ctx: &DeriveCtx, fields: &[syn::Field],
    ) -> syn::Result<TokenStream>;

    fn derive_unit_enum(
        &self, ctx: &DeriveCtx, enum_data: &syn::DataEnum, disc_ty: &syn::Type,
    ) -> syn::Result<TokenStream>;

    fn derive_field_enum(
        &self, ctx: &DeriveCtx, enum_data: &syn::DataEnum, disc_ty: &syn::Type,
    ) -> syn::Result<TokenStream>;
}

pub fn derive<T: Derivable>(derivable: &T, ctx: &DeriveCtx) -> syn::Result<TokenStream> {
    match &ctx.data {
        syn::Data::Struct(s) => match &s.fields {
            syn::Fields::Named(f) => {
                let fields: Vec<_> = f.named.iter().cloned().collect();
                derivable.derive_struct_named(ctx, &fields)
            },
            syn::Fields::Unnamed(f) => {
                let fields: Vec<_> = f.unnamed.iter().cloned().collect();
                derivable.derive_struct_unnamed(ctx, &fields)
            },
            _ => panic!("struct must have fields"),
        },
        syn::Data::Enum(e) => {
            let repr = ctx.attrs.repr().as_enum().unwrap();
            let has_fields = e
                .variants
                .iter()
                .any(|v| !matches!(v.fields, syn::Fields::Unit));
            if has_fields {
                assert!(repr.has_c);
                derivable.derive_field_enum(ctx, e, &repr.disc_ty)
            } else {
                assert!(!repr.has_c);
                derivable.derive_unit_enum(ctx, e, &repr.disc_ty)
            }
        },
        _ => panic!("can only derive for structs and enums"),
    }
}
