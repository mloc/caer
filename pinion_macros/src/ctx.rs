use crate::attr::DataAttributes;

pub struct DeriveCtx {
    pub vis: syn::Visibility,
    pub ident: syn::Ident,
    pub data: syn::Data,
    pub generics: syn::Generics,
    pub attrs: DataAttributes,
}

impl DeriveCtx {
    pub fn create(input: syn::DeriveInput) -> Self {
        let syn::DeriveInput {
            vis,
            ident,
            data,
            generics,
            attrs,
            ..
        } = input;

        let attrs = DataAttributes::from_attrs(&attrs).unwrap();

        Self {
            vis,
            ident,
            data,
            generics,
            attrs,
        }
    }
}
