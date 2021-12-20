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
    fn build_shim(&self, self_ty: &syn::Type) -> syn::ItemFn {
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
                #self_ty::#name ( #(#args),* )
            }
        }
        .into();

        syn::parse(output).unwrap()
    }
}

pub fn build_export_funcs(carrier: syn::Type, impl_item: syn::ItemImpl) -> TokenStream2 {
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

    let ctx: syn::Expr = parse_quote! { ctx };

    let shims = funcs.iter().map(|e| e.build_shim(&impl_item.self_ty));
    let ctx_funcs = funcs.iter().map(|e| e.shape.build_type(&ctx));
    let func_names = funcs.iter().map(|e| &e.name);
    let func_names_str = funcs.iter().map(|e| e.name.to_string());

    quote! {
        #impl_item

        #[allow(non_camel_case_types)]
        #[allow(dead_code)]
        enum #carrier {
           #(#func_names),*
        }

        #[automatically_derived]
        impl pinion::PinionFuncCarrier for #carrier {
            fn get_all_funcs<C: pinion::Context>(ctx: &mut C) -> Vec<(&'static str, C::FunctionType)> {
                [#((#func_names_str, #ctx_funcs)),*].into()
            }
        }

        #(#shims)*
    }
}
