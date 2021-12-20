use quote::ToTokens;
use syn::parse_quote;

use crate::ty;

#[derive(Debug, Clone)]
pub struct FuncShape {
    pub params: Vec<syn::Type>,
    pub ret: Option<syn::Type>,
}

impl FuncShape {
    fn ret_from_rty(rty: &syn::ReturnType) -> Option<syn::Type> {
        match rty {
            syn::ReturnType::Default => None,
            syn::ReturnType::Type(_, ty) => Some(*ty.clone()),
        }
    }

    pub fn from_method(sig: &syn::Signature, self_ty: &syn::Type) -> Self {
        let params = sig
            .inputs
            .iter()
            .map(|inp| match inp {
                syn::FnArg::Receiver(r) => {
                    assert!(r.attrs.is_empty());

                    let r_ref_amp = r.reference.as_ref().map(|r| r.0);
                    let r_ref_lt = r.reference.as_ref().map(|r| &r.1);
                    let r_mut = &r.mutability;

                    parse_quote! { #r_ref_amp #r_ref_lt #r_mut #self_ty }
                },
                syn::FnArg::Typed(p) => {
                    assert!(p.attrs.is_empty());
                    // pat must be just an ident
                    assert!(syn::parse::<syn::Ident>(p.pat.to_token_stream().into()).is_ok());

                    *p.ty.clone()
                },
            })
            .collect();

        Self {
            params,
            ret: Self::ret_from_rty(&sig.output),
        }
    }

    pub fn from_bare(bare: &syn::TypeBareFn) -> Self {
        let params = bare
            .inputs
            .iter()
            .map(|inp| {
                assert!(inp.attrs.is_empty());
                inp.ty.clone()
            })
            .collect();

        Self {
            params,
            ret: Self::ret_from_rty(&bare.output),
        }
    }

    pub fn build_type(&self, ctx: &syn::Expr) -> syn::Expr {
        let param_types = self.params.iter().map(|ty| ty::build_type(ctx, ty));
        let ret_type: syn::Expr = match &self.ret {
            Some(ty) => {
                let ty = ty::build_type(ctx, ty);
                parse_quote! {Some(#ty)}
            },
            None => parse_quote! {None},
        };

        parse_quote! {
            {
                let params = [#(#param_types,)*];
                let ret_ty = #ret_type;
                let func = #ctx.make_function_type(&params, ret_ty);
                #ctx.make_func_ptr_type(func)
            }
        }
    }
}
