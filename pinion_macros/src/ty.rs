use syn::parse_quote;

use crate::func;

// TODO: rename. type builders

pub fn build_type(ctx: &syn::Expr, ty: &syn::Type) -> syn::Expr {
    match ty {
        syn::Type::Path(path) => {
            parse_quote! { <#path as pinion::PinionData>::create_in_context(#ctx) }
        },
        syn::Type::Reference(ptr) => {
            parse_quote! { <#ptr as pinion::PinionData>::create_in_context(#ctx) }
        },
        syn::Type::Ptr(ptr) => {
            parse_quote! { <#ptr as pinion::PinionData>::create_in_context(#ctx) }
        },
        syn::Type::BareFn(bare_fn) => {
            assert_eq!(bare_fn.abi, Some(parse_quote! { extern "C" }));
            let func_ty = func::FuncShape::from_bare(bare_fn).build_type(ctx);
            parse_quote! {
                {
                    let func = #func_ty;
                    #ctx.make_func_ptr_type(func)
                }
            }
        },
        _ => todo!("can't synth type {:?}", ty),
    }
}

pub fn build_layout(ty: &syn::Type) -> syn::Expr {
    match ty {
        syn::Type::Path(path) => {
            parse_quote! { <#path as pinion::PinionData>::get_layout() }
        },
        syn::Type::Reference(ptr) => {
            parse_quote! { <#ptr as pinion::PinionData>::get_layout() }
        },
        syn::Type::Ptr(ptr) => {
            parse_quote! { <#ptr as pinion::PinionData>::get_layout() }
        },
        syn::Type::BareFn(_bare_fn) => {
            parse_quote! { <pinion::PinionFuncPtr as pinion::PinionData>::get_layout() }
        },
        _ => todo!("can't make layout {:?}", ty),
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
