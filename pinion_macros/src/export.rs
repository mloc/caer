use std::borrow::Cow;

use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::parse_quote;

use crate::ty;

pub fn build_export_func(mut fn_item: syn::ItemFn) -> TokenStream2 {
    assert!(fn_item.sig.abi.is_none());
    assert!(!fn_item
        .attrs
        .iter()
        .any(|attr| attr.path != parse_quote! {doc}));
    fn_item.sig.abi = Some(parse_quote! {extern "C"});
    fn_item.attrs.push(parse_quote! { #[no_mangle] });

    let lctxq: syn::Ident = parse_quote! { lctx };
    let fnmeta_ident = make_fnmeta_ident(&fn_item.sig.ident);
    let vis = &fn_item.vis;
    let ident_str = fn_item.sig.ident.to_string();

    let param_bind_idents: Vec<_> = (0..fn_item.sig.inputs.len())
        .map(|i| syn::Ident::new(&format!("a{}", i), Span::call_site()))
        .collect();
    let param_bind_params = fn_item.sig.inputs.iter().enumerate().map(|(i, arg)| {
        let pat_ty = if let syn::FnArg::Typed(pat_ty) = arg {
            pat_ty
        } else {
            panic!("no self");
        };

        let ty = &pat_ty.ty;
        let ident = &param_bind_idents[i];
        quote! {
            #ident: impl pinion::PinionValueHolder<#ty, Reified = V>
        }
    });
    let param_lids = fn_item.sig.inputs.iter().map(|arg| {
        let param_ty = match arg {
            syn::FnArg::Receiver(_) => panic!("no self in export"),
            syn::FnArg::Typed(p) => &p.ty,
        };

        quote! {
            lctx.populate::<#param_ty>()
        }
    });
    let params_n = fn_item.sig.inputs.len();

    let return_ty = match fn_item.sig.output {
        syn::ReturnType::Default => parse_quote! {()},
        syn::ReturnType::Type(_, ref ty) => ty.clone(),
    };
    let return_lid = match fn_item.sig.output {
        syn::ReturnType::Default => quote! {None},
        syn::ReturnType::Type(_, ref ty) => {
            let layout = ty::populate_ty(&lctxq, ty);
            quote! { Some(#layout)}
        },
    };

    quote! {
        #fn_item

        #[doc(hidden)]
        #[allow(non_camel_case_types)]
        #vis struct #fnmeta_ident;

        impl pinion::PinionFuncInstance for #fnmeta_ident {
        }

        impl #fnmeta_ident {
            pub fn bind<V>(self, #(#param_bind_params),*) -> pinion::PinionCallBundle<#params_n, #fnmeta_ident, #return_ty, V> {
                pinion::PinionCallBundle::new([
                    #(#param_bind_idents.reify(),)*
                ])
            }
        }

        impl pinion::PinionFunc for #fnmeta_ident {
            fn get_func_layout<'ctx>(lctx: &mut pinion::layout_ctx::LayoutCtx<'ctx>) -> pinion::layout::Func<'ctx> {
                pinion::layout::Func {
                    name: #ident_str,
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

/*fn extract_lifetimes(sig: &syn::Signature) -> Vec<syn::Lifetime> {
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
}*/

#[derive(Debug)]
pub struct ModuleDef {
    ident: syn::Ident,
    _comma: syn::token::Comma,
    _bracket_token: syn::token::Bracket,
    func_idents: syn::punctuated::Punctuated<syn::Path, syn::Token![,]>,
}

impl syn::parse::Parse for ModuleDef {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let funcs_inner;
        let ident = input.parse()?;
        let _comma = input.parse()?;
        let _bracket_token = syn::bracketed!(funcs_inner in input);
        let func_idents = funcs_inner.parse_terminated(|x| {
            let x = syn::Path::parse(x);
            x
        })?;

        Ok(ModuleDef {
            ident,
            _comma,
            _bracket_token,
            func_idents,
        })
    }
}

fn get_last_id(path: &syn::Path) -> &syn::Ident {
    let last_seg = path.segments.last().unwrap();
    assert!(last_seg.arguments.is_empty());
    &last_seg.ident
}

pub fn build_module_export(def: ModuleDef) -> TokenStream2 {
    //panic!("{:#?}", def);
    let mod_id = &def.ident;
    let efuncs = def.func_idents.iter().map(|path| {
        let func_id = get_last_id(path);
        let fnmeta_ident = make_fnmeta_ident(func_id);
        let mut fnmeta_path = path.clone();
        fnmeta_path.segments.last_mut().unwrap().ident = fnmeta_ident;
        quote! {
            (Self::Funcs::#func_id, std::any::TypeId::of::<#fnmeta_path>(), <#fnmeta_path as pinion::PinionFunc>::get_func_layout(lctx))
        }
    });
    let func_methods = def.func_idents.iter().map(|path| {
        let func_id = get_last_id(path);
        let fnmeta_ident = make_fnmeta_ident(func_id);
        let mut fnmeta_path = path.clone();
        fnmeta_path.segments.last_mut().unwrap().ident = fnmeta_ident;
        quote! {
            pub fn #func_id() -> #fnmeta_path {
                #fnmeta_path
            }
        }
    });
    let mod_funcs_enum = syn::Ident::new(
        &format!("__PINION_EXPORT_FUNCS_ENUM__{}", mod_id),
        mod_id.span(),
    );
    let variants = def.func_idents.iter().map(get_last_id);
    let variants_names = def.func_idents.iter().map(|path| {
        let id = get_last_id(path);
        let id_str = id.to_string();
        quote! {
            Self::#id => #id_str
        }
    });

    let mod_tfuncs = syn::Ident::new(&format!("__PINION_EXPORT_FUNCS__{}", mod_id), mod_id.span());

    quote! {
        pub struct #mod_id;

        impl pinion::PinionModule for #mod_id {
            type Funcs = #mod_funcs_enum;
            type TFuncs = #mod_tfuncs;
            fn get_funcs<'ctx>(lctx: &mut pinion::layout_ctx::LayoutCtx<'ctx>) -> Vec<(Self::Funcs, std::any::TypeId, pinion::layout::Func<'ctx>)> {
                vec![ #(#efuncs,)* ]
            }
        }

        #[doc(hidden)]
        #[derive(Clone, Copy)]
        pub struct #mod_tfuncs;

        impl pinion::PinionModuleFuncs for #mod_tfuncs {}

        impl #mod_tfuncs {
            #(#func_methods)*
        }

        #[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
        #[doc(hidden)]
        #[allow(non_camel_case_types)]
        pub enum #mod_funcs_enum {
            #(#variants,)*
        }

        impl pinion::PinionModuleFuncsEnum for #mod_funcs_enum {
            fn get_name(self) -> &'static str {
                match self {
                    #(#variants_names,)*
                }
            }
        }
    }
}
