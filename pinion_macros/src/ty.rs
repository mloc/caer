use syn::parse_quote;

use crate::func;

// TODO: rename. type builders

pub fn build_type(ctx: &syn::Expr, ty: &syn::Type) -> syn::Expr {
    match ty {
        syn::Type::Path(path) => {
            parse_quote! { <#path as pinion::PinionBasicType>::create_in_context(#ctx) }
        },
        syn::Type::Reference(ptr) => {
            parse_quote! { <#ptr as pinion::PinionBasicType>::create_in_context(#ctx) }
        },
        syn::Type::Ptr(ptr) => {
            parse_quote! { <#ptr as pinion::PinionBasicType>::create_in_context(#ctx) }
        },
        syn::Type::BareFn(bare_fn) => {
            assert_eq!(bare_fn.abi, Some(parse_quote! { extern "C" }));
            func::FuncShape::from_bare(bare_fn).build_type(ctx)
        },
        _ => todo!("can't synth type {:?}", ty),
    }
}

pub fn build_layout(ty: &syn::Type) -> syn::Expr {
    match ty {
        syn::Type::Path(path) => {
            parse_quote! { <#path as pinion::PinionBasicType>::get_layout() }
        },
        syn::Type::Reference(ptr) => {
            parse_quote! { <#ptr as pinion::PinionBasicType>::get_layout() }
        },
        syn::Type::Ptr(ptr) => {
            parse_quote! { <#ptr as pinion::PinionBasicType>::get_layout() }
        },
        syn::Type::BareFn(_bare_fn) => {
            parse_quote! { <pinion::PinionFuncPtr as pinion::PinionBasicType>::get_layout() }
        },
        _ => todo!("can't make layout {:?}", ty),
    }
}
