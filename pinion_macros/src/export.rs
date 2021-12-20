use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::parse_quote;
use syn::spanned::Spanned;

use crate::func::FuncShape;

struct ExtFunc {
    name: syn::Ident,
    shape: FuncShape,
}

impl ExtFunc {
    fn build_shim(&self, carrier: &syn::Type) -> syn::ItemFn {
        let name = &self.name;
        let ret = self.shape.ret.as_ref().map(|ty| quote! { -> #ty });
        let params = self.shape.params.iter().enumerate().map(|(i, ty)| {
            let param_name = syn::Ident::new(&format!("arg{}", i), ty.span());
            quote! {
                #param_name: #ty
            }
        });
        let args = self
            .shape
            .params
            .iter()
            .enumerate()
            .map(|(i, ty)| syn::Ident::new(&format!("arg{}", i), ty.span()));

        let output = quote! {
            #[no_mangle]
            unsafe extern "C" fn #name ( #(#params),* ) #ret {
                #carrier::#name ( #(#args),* )
            }
        }
        .into();

        syn::parse(output).unwrap()
    }
}

pub fn build_export_funcs(target: syn::Type, impl_item: syn::ItemImpl) -> TokenStream2 {
    assert_eq!(impl_item.generics, parse_quote! {});

    let funcs: Vec<_> = impl_item
        .items
        .iter()
        .map(|p| match p {
            syn::ImplItem::Method(m) => {
                assert_eq!(m.sig.generics, syn::Generics::default());
                let shape = FuncShape::from_method(&m.sig, &impl_item.self_ty);
                ExtFunc {
                    name: m.sig.ident.clone(),
                    shape,
                }
            },
            _ => panic!("impl block can only contain methods"),
        })
        .collect();

    let shims = funcs.iter().map(|e| e.build_shim(&impl_item.self_ty));

    quote! {
        #impl_item

        #[doc(hidden)]
        struct #target {
            _hide: (), // TODO: never type
        }

        #(#shims)*
    }
}
