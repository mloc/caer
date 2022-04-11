use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::parse_quote;
use syn::spanned::Spanned;

use crate::func::FuncShape;
use crate::ty;

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
        let args: Vec<_> = self
            .shape
            .params
            .iter()
            .enumerate()
            .map(|(i, ty)| syn::Ident::new(&format!("arg{}", i), ty.span()))
            .collect();

        let validates = self.shape.params.iter().enumerate().map(|(i, ty)| {
            let arg = &args[i];
            let ptr = parse_quote! { &#arg as *const _ as *const u8 };
            ty::build_validate(&ptr, ty)
        });

        quote! {
            #[no_mangle]
            #[doc(hidden)]
            unsafe extern "C" fn #name ( #(#params),* ) #ret {
                #(#validates;)*
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

pub fn build_export_func(mut fn_item: syn::ItemFn) -> TokenStream2 {
    assert!(fn_item.sig.abi.is_none());
    assert!(fn_item.attrs.is_empty());
    fn_item.sig.abi = Some(parse_quote! {extern "C"});
    fn_item.attrs.push(parse_quote! { #[no_mangle] });

    let fnmeta_ident = make_fnmeta_ident(&fn_item.sig.ident);

    let param_lids = fn_item.sig.inputs.iter().map(|arg| {
        let param_ty = match arg {
            syn::FnArg::Receiver(_) => panic!("no self in export"),
            syn::FnArg::Typed(p) => &p.ty,
        };

        quote! {
            lctx.populate::<#param_ty>()
        }
    });

    let return_lid = match fn_item.sig.output {
        syn::ReturnType::Default => quote! {None},
        syn::ReturnType::Type(_, ref ty) => quote! { Some(lctx.populate::<#ty>)},
    };

    quote! {
        #fn_item

        #[doc(hidden)]
        struct #fnmeta_ident;

        impl pinion::PinionFunc for #fnmeta_ident {
            fn get_func_layout(lctx: &mut pinion::layout_ctx::LayoutCtx) -> pinion::layout::Func {
                pinion::layout::Func {
                    param_tys: vec![#(#param_lids,)*],
                    return_ty: #return_lid,
                }
            }
        }
    }
}

fn make_fnmeta_ident(base: &syn::Ident) -> syn::Ident {
    syn::Ident::new(&format!("__PINION_EXPORT_FN__{}", base), base.span())
}

fn extract_lifetimes(sig: &syn::Signature) -> Vec<syn::Lifetime> {
    let mut out = vec![];

    fn process_ty(ty: &syn::Type, out: &mut Vec<syn::Lifetime>) {
        match ty {
            syn::Type::Array(_) => panic!(),
            syn::Type::BareFn(_) => panic!(),
            syn::Type::Group(gty) => process_ty(&gty.elem, out),
            syn::Type::ImplTrait(_) => panic!(),
            syn::Type::Infer(_) => panic!(),
            syn::Type::Macro(_) => panic!(),
            syn::Type::Never(_) => panic!(),
            syn::Type::Paren(_) => panic!(),
            syn::Type::Path(_) => {},
            syn::Type::Ptr(_) => {},
            syn::Type::Reference(refty) => {
                if let Some(lt) = &refty.lifetime {
                    out.push(lt.clone());
                }
                process_ty(&refty.elem, out);
            },
            syn::Type::Slice(_) => todo!(),
            syn::Type::TraitObject(_) => panic!(),
            syn::Type::Tuple(_) => todo!(),
            syn::Type::Verbatim(_) => todo!(),
            _ => todo!(),
        }
    }

    for arg in &sig.inputs {
        match arg {
            syn::FnArg::Receiver(r) => {
                if let Some((_, Some(lt))) = &r.reference {
                    out.push(lt.clone());
                }
            },
            syn::FnArg::Typed(ty) => process_ty(&ty.ty, &mut out),
        }
    }

    out
}

#[derive(Debug)]
pub struct ModuleDef {
    ident: syn::Ident,
    comma: syn::token::Comma,
    bracket_token: syn::token::Bracket,
    func_idents: syn::punctuated::Punctuated<syn::Ident, syn::Token![,]>,
}

impl syn::parse::Parse for ModuleDef {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let funcs_inner;
        Ok(ModuleDef {
            ident: input.parse()?,
            comma: input.parse()?,
            bracket_token: syn::bracketed!(funcs_inner in input),
            func_idents: funcs_inner.parse_terminated(syn::Ident::parse)?,
        })
    }
}

pub fn build_module_export(def: ModuleDef) -> TokenStream2 {
    //panic!("{:#?}", def);
    let mod_id = &def.ident;
    let funcs = def.func_idents.iter().map(|id| {
        let fnmeta_ident = make_fnmeta_ident(id);
        let fn_str = id.to_string();
        quote! {
            (#fn_str, <#fnmeta_ident as pinion::PinionFunc>::get_func_layout(lctx))
        }
    });

    quote! {
        pub struct #mod_id;

        impl pinion::PinionModule for #mod_id {
            fn get_funcs(lctx: &mut pinion::layout_ctx::LayoutCtx) -> Vec<(&'static str, pinion::layout::Func)> {
                vec![ #(#funcs,)* ]
            }
        }
    }
}
