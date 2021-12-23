use proc_macro2::TokenStream;
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
            _ => panic!("can only derive for structs"),
        }
    }

    pub fn derive_named(&self, fields: &[syn::Field]) -> TokenStream {
        let ctxq: syn::Expr = parse_quote! { ctx };
        let fq = fields.iter().map(|f| ty::build_type(&ctxq, &f.ty));

        let field_names = fields
            .iter()
            .map(|f| ident_to_litstr(f.ident.as_ref().unwrap()));
        let field_layouts = fields.iter().map(|f| ty::build_layout(&f.ty));

        let name = self.attrs.name().unwrap_or("");
        let packed = self.attrs.packed();
        let ident = &self.ident;

        let once_cell: syn::Path = parse_quote! { pinion::rex::once_cell };
        let layout: syn::Path = parse_quote! { pinion::types::layout };

        let (impl_generics, ty_generics, where_clause) = self.generics.split_for_impl();

        quote! {
            #[automatically_derived]
            impl #impl_generics pinion::PinionData for #ident #ty_generics #where_clause {
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

                fn validate(&self) {
                    todo!();
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

        let layout_b = ty::build_layout(s_ty);
        let ctxty = ty::build_type(&ctxq, s_ty);
        let ident = &self.ident;

        let (impl_generics, ty_generics, where_clause) = self.generics.split_for_impl();

        quote! {
            #[automatically_derived]
            impl #impl_generics pinion::PinionData for #ident #ty_generics #where_clause {
                fn create_in_context<C: pinion::Context>(ctx: &mut C) -> C::BasicType {
                    #ctxty
                }

                fn get_layout() -> &'static pinion::types::layout::CycleCell {
                    #layout_b
                }

                fn validate(&self) {
                    todo!();
                }
            }
            impl #impl_generics pinion::PinionStruct for #ident #ty_generics #where_clause {}
        }
    }
}

fn ident_to_litstr(ident: &syn::Ident) -> syn::LitStr {
    syn::LitStr::new(&ident.to_string(), ident.span())
}
