use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::parse_quote;

use crate::attr::DataAttributes;
use crate::ty;

pub struct DeriveCtx {
    ident: syn::Ident,
    data: syn::Data,
    generics: syn::Generics,
    attrs: DataAttributes,
}

impl DeriveCtx {
    pub fn create(input: syn::DeriveInput) -> Self {
        let syn::DeriveInput {
            ident,
            data,
            generics,
            attrs,
            ..
        } = input;

        let attrs = DataAttributes::from_attrs(&attrs).unwrap();

        Self {
            ident,
            data,
            generics,
            attrs,
        }
    }

    pub fn derive(&self) -> TokenStream {
        match &self.data {
            syn::Data::Struct(s) => match &s.fields {
                syn::Fields::Named(f) => {
                    let fields: Vec<_> = f.named.iter().cloned().collect();
                    self.derive_named(&fields)
                },
                syn::Fields::Unnamed(f) => {
                    let fields: Vec<_> = f.unnamed.iter().cloned().collect();
                    self.derive_newtype(&fields)
                },
                _ => panic!("struct must have fields"),
            },
            syn::Data::Enum(e) => self.derive_enum(e),
            _ => panic!("can only derive for structs and enums"),
        }
    }

    pub fn derive_named(&self, fields: &[syn::Field]) -> TokenStream {
        let ctxq: syn::Expr = parse_quote! { ctx };
        let lctxq: syn::Ident = parse_quote! { lctx };
        let fq = fields.iter().map(|f| ty::build_type(&ctxq, &f.ty));

        let field_names = fields
            .iter()
            .map(|f| ident_to_litstr(f.ident.as_ref().unwrap()));
        let field_layouts = fields.iter().map(|f| -> syn::Expr {
            let ty = ty::normalize_ty(&f.ty);
            parse_quote! { #lctxq.populate::<#ty>() }
        });

        let field_validates = fields.iter().map(|f| {
            let ident = f.ident.as_ref().unwrap();
            let ptr = parse_quote! {(&(*sptr).#ident) as *const _ as *const u8};
            ty::build_validate(&ptr, &f.ty)
        });

        let name = self.attrs.name().unwrap_or("");
        let packed = self.attrs.repr().as_struct().unwrap();
        let ident = &self.ident;

        let (impl_generics, ty_generics, where_clause) = self.generics.split_for_impl();

        quote! {
            #[automatically_derived]
            impl #impl_generics pinion::PinionData for #ident #ty_generics #where_clause {
                fn create_in_context<C: pinion::Context>(ctx: &mut C) -> C::BasicType {
                    let fields = [#(#fq,)*];
                    ctx.make_struct_type(&fields, #packed, #name)
                }

                fn get_layout(#lctxq: &mut pinion::layout_ctx::LayoutCtx) -> pinion::layout::BasicType {
                    let fields = [#((#field_names,#field_layouts),)*];
                    let struct_layout = pinion::layout::StructLayout::new(&fields);
                    pinion::layout::BasicType::Struct(struct_layout)
                }

                unsafe fn validate(ptr: *const u8) {
                    let sptr = ptr as *const Self;

                    #(#field_validates;)*
                }
            }
            #[automatically_derived]
            impl #impl_generics pinion::PinionStruct for #ident #ty_generics #where_clause {}
        }
    }

    fn derive_newtype(&self, fields: &[syn::Field]) -> TokenStream {
        assert_eq!(fields.len(), 1);

        let s_ty = &fields.first().unwrap().ty;

        let ctxq: syn::Expr = parse_quote! { ctx };
        let lctxq: syn::Ident = parse_quote! { lctx };

        let inner_data = ty::normalize_ty(s_ty);
        let ctxty = ty::build_type(&ctxq, s_ty);
        let validate_body = ty::build_validate(&parse_quote! { ptr }, s_ty);
        let ident = &self.ident;

        let (impl_generics, ty_generics, where_clause) = self.generics.split_for_impl();

        quote! {
            #[automatically_derived]
            impl #impl_generics pinion::PinionData for #ident #ty_generics #where_clause {
                fn create_in_context<C: pinion::Context>(ctx: &mut C) -> C::BasicType {
                    #ctxty
                }

                fn get_layout(lctx: &mut pinion::layout_ctx::LayoutCtx) -> pinion::layout::BasicType {
                    <#inner_data as pinion::PinionData>::get_layout(lctx)
                }

                unsafe fn validate(ptr: *const u8) {
                    #validate_body
                }
            }
            #[automatically_derived]
            impl #impl_generics pinion::PinionStruct for #ident #ty_generics #where_clause {}
        }
    }

    fn derive_enum(&self, enum_data: &syn::DataEnum) -> TokenStream {
        let (disc_width, has_c) = self.attrs.repr().as_enum().unwrap();
        let prim: syn::Type = match disc_width {
            1 => parse_quote! {u8},
            2 => parse_quote! {u16},
            4 => parse_quote! {u32},
            8 => parse_quote! {u64},
            _ => unreachable!(),
        };

        enum_data.variants.iter().for_each(|v| {
            if !matches!(v.fields, syn::Fields::Unit) {
                assert!(has_c);
                todo!("handle fieldy enums")
            }
        });

        let mut de_prev: (syn::Expr, isize) = (parse_quote! {0}, 0);
        let disc_expr = enum_data.variants.iter().map(|v| -> syn::Expr {
            if let Some((_, disc)) = &v.discriminant {
                de_prev = (disc.clone(), 0);
            }
            let (expr, count) = &de_prev;
            let count_lit = syn::LitInt::new(&count.to_string(), Span::call_site());
            let out = parse_quote! { (#expr) + #count_lit };
            de_prev.1 += 1;
            out
        });
        let disc_const_names: Vec<syn::Ident> = (0..enum_data.variants.len())
            .map(|i| syn::Ident::new(&format!("DISC_{}", i), Span::call_site()))
            .collect();

        let ident = &self.ident;
        let (impl_generics, ty_generics, where_clause) = self.generics.split_for_impl();

        quote! {
            #[automatically_derived]
            impl #impl_generics pinion::PinionData for #ident #ty_generics #where_clause {
                fn create_in_context<C: pinion::Context>(ctx: &mut C) -> C::BasicType {
                    <#prim as pinion::PinionData>::create_in_context(ctx)
                }

                fn get_layout(lctx: &mut pinion::layout_ctx::LayoutCtx) -> pinion::layout::BasicType {
                    <#prim as pinion::PinionData>::get_layout(lctx)
                }

                unsafe fn validate(ptr: *const u8) {
                    #(const #disc_const_names: #prim = #disc_expr;)*

                    let disc: #prim = *(ptr as *const #prim);
                    match disc {
                        #(#disc_const_names)|* => {},
                        _ => panic!(),
                    }
                }
            }
        }
    }
}

fn ident_to_litstr(ident: &syn::Ident) -> syn::LitStr {
    syn::LitStr::new(&ident.to_string(), ident.span())
}
