use syn::parse_quote;

use crate::func;

// TODO: rename. type builders

pub fn populate_ty(lctx: &syn::Ident, ty: &syn::Type) -> syn::Expr {
    match ty {
        syn::Type::Path(path) => {
            parse_quote! { #lctx.populate::<#path>() }
        },
        syn::Type::Reference(ptr) => {
            parse_quote! { #lctx.populate::<#ptr>() }
        },
        syn::Type::Ptr(ptr) => {
            parse_quote! { #lctx.populate::<#ptr>() }
        },
        syn::Type::BareFn(bare_fn) => {
            assert_eq!(bare_fn.abi, Some(parse_quote! { extern "C" }));
            let func_ty = func::FuncShape::from_bare(lctx, bare_fn).build_layout(lctx);
            parse_quote! {
                unsafe { #lctx.unchecked_populate_fn::<#bare_fn, _>(|#lctx| pinion::layout::Layout::FuncPtr(#func_ty)) }
            }
        },
        _ => todo!("can't synth type {:?}", ty),
    }
}

pub fn build_validate(ptr: &syn::Expr, ty: &syn::Type) -> syn::Expr {
    match ty {
        syn::Type::Path(_) | syn::Type::Reference(_) | syn::Type::Ptr(_) => {
            parse_quote! { <#ty as pinion::PinionData>::validate(#ptr) }
        },
        syn::Type::BareFn(_) => {
            parse_quote! { {} } // maybe validate ptr..?
        },
        _ => todo!("can't make validate for {:?}", ty),
    }
}
