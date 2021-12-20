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
    fn build_shim(&self, ts: &syn::Type) -> TokenStream2 {
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

        quote! {
            #[no_mangle]
            #[doc(hidden)]
            unsafe extern "C" fn #name ( #(#params),* ) #ret {
                #ts::#name ( #(#args),* )
            }
        }
    }
}

pub fn build_export_funcs(
    args: syn::punctuated::Punctuated<syn::Ident, syn::Token![,]>, trait_item: syn::ItemTrait,
) -> TokenStream2 {
    assert_eq!(args.len(), 2);
    let carrier = &args[0];
    let macro_name = &args[1];

    assert_eq!(trait_item.generics, parse_quote! {});

    let tsi = syn::Ident::new(
        &format!("__TargetSelf__{}__{}", carrier, macro_name),
        proc_macro2::Span::call_site(),
    );
    let ts = parse_quote! { #tsi };

    let funcs: Vec<_> = trait_item
        .items
        .iter()
        .map(|p| match p {
            syn::TraitItem::Method(m) => {
                assert_eq!(m.sig.generics, syn::Generics::default());
                let shape = FuncShape::from_method(&m.sig, &ts);
                ExtFunc {
                    name: m.sig.ident.clone(),
                    shape,
                }
            },
            _ => panic!("trait can only contain methods"),
        })
        .collect();

    let ctx: syn::Expr = parse_quote! { ctx };

    let shims = funcs.iter().map(|e| e.build_shim(&ts));
    let ctx_funcs = funcs.iter().map(|e| e.shape.build_type(&ctx));
    let func_names = funcs.iter().map(|e| &e.name);
    let func_names_str = funcs.iter().map(|e| e.name.to_string());

    quote! {
        #trait_item

        #[allow(non_camel_case_types)]
        #[allow(dead_code)]
        enum #carrier {
           #(#func_names),*
        }

        #[automatically_derived]
        impl pinion::PinionFuncCarrier for #carrier {
            fn get_all_funcs<C: pinion::Context>(ctx: &mut C) -> Vec<(&'static str, C::FunctionType)> {
                type #ts = pinion::PinionOpaqueStruct;
                [#((#func_names_str, #ctx_funcs)),*].into()
            }
        }

        macro_rules! #macro_name {
            ($target:ty) => {
                #[doc(hidden)]
                #[allow(non_camel_case_types)]
                type #ts = $target;

                #(#shims)*
            };
        }
    }
}
