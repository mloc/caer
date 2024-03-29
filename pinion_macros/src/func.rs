use syn::parse_quote;

use crate::ty;

#[derive(Debug, Clone)]
pub struct FuncShape {
    pub params: Vec<syn::Expr>,
    pub ret: Option<syn::Expr>,
}

impl FuncShape {
    fn ret_from_rty(lctx: &syn::Ident, rty: &syn::ReturnType) -> Option<syn::Expr> {
        match rty {
            syn::ReturnType::Default => None,
            syn::ReturnType::Type(_, ty) => Some(ty::populate_ty(lctx, ty)),
        }
    }

    /*pub fn from_method(sig: &syn::Signature, self_ty: &syn::Type) -> Self {
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
    }*/

    pub fn from_bare(lctx: &syn::Ident, bare: &syn::TypeBareFn) -> Self {
        let params = bare
            .inputs
            .iter()
            .map(|inp| ty::populate_ty(lctx, &inp.ty))
            .collect();

        Self {
            params,
            ret: Self::ret_from_rty(lctx, &bare.output),
        }
    }

    pub fn build_layout(&self) -> syn::Expr {
        let param_types = self.params.iter();
        let ret_type: syn::Expr = match &self.ret {
            Some(expr) => {
                parse_quote! {Some(#expr)}
            },
            None => parse_quote! {None},
        };

        parse_quote! {
            pinion::layout::Func {
                name: "",
                param_tys: vec![#(#param_types,)*],
                return_ty: #ret_type,
            }
        }
    }
}
