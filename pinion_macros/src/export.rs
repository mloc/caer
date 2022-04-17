use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::parse_quote;

pub fn build_export_func(mut fn_item: syn::ItemFn) -> TokenStream2 {
    assert!(fn_item.sig.abi.is_none());
    assert!(fn_item.attrs.is_empty());
    fn_item.sig.abi = Some(parse_quote! {extern "C"});
    fn_item.attrs.push(parse_quote! { #[no_mangle] });

    let fnmeta_ident = make_fnmeta_ident(&fn_item.sig.ident);
    let vis = &fn_item.vis;

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
        syn::ReturnType::Type(_, ref ty) => quote! { Some(lctx.populate::<#ty>())},
    };

    quote! {
        #fn_item

        #[doc(hidden)]
        #vis struct #fnmeta_ident;

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
    let funcs = def.func_idents.iter().map(|path| {
        let func_id = get_last_id(path);
        let fnmeta_ident = make_fnmeta_ident(func_id);
        let mut fnmeta_path = path.clone();
        fnmeta_path.segments.last_mut().unwrap().ident = fnmeta_ident;
        quote! {
            (Self::Funcs::#func_id, <#fnmeta_path as pinion::PinionFunc>::get_func_layout(lctx))
        }
    });
    let mod_funcs_enum =
        syn::Ident::new(&format!("__PINION_EXPORT_FUNCS__{}", mod_id), mod_id.span());
    let variants = def.func_idents.iter().map(get_last_id);
    let variants_names = def.func_idents.iter().map(|path| {
        let id = get_last_id(path);
        let id_str = id.to_string();
        quote! {
            Self::#id => #id_str
        }
    });

    quote! {
        pub struct #mod_id;

        impl pinion::PinionModule for #mod_id {
            type Funcs = #mod_funcs_enum;
            fn get_funcs(lctx: &mut pinion::layout_ctx::LayoutCtx) -> Vec<(Self::Funcs, pinion::layout::Func)> {
                vec![ #(#funcs,)* ]
            }
        }

        #[derive(Clone, Copy, Debug)]
        #[doc(hidden)]
        #[allow(non_camel_case_types)]
        pub enum #mod_funcs_enum {
            #(#[allow(non_camel_case_types)] #variants,)*
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
