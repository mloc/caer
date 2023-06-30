use anyhow::{anyhow, bail, Context, Result};
use syn::parse::{ParseStream, Parser};
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::{parenthesized, parse_quote, Attribute, Ident, Lit, Meta};

#[derive(Debug, Clone)]
pub enum Repr {
    Struct(StructRepr),
    Enum(EnumRepr),
}

#[derive(Debug, Clone)]
pub struct StructRepr {
    pub packed: bool,
}

#[derive(Debug, Clone)]
pub struct EnumRepr {
    pub disc_ty: syn::Type,
    pub has_c: bool,
}

impl Repr {
    pub fn as_struct(&self) -> Option<&StructRepr> {
        if let Self::Struct(s) = self {
            Some(s)
        } else {
            None
        }
    }

    pub fn as_enum(&self) -> Option<&EnumRepr> {
        if let Self::Enum(e) = self {
            Some(e)
        } else {
            None
        }
    }
}

pub struct DataAttributes {
    name: Option<String>,
    repr: Repr,
}

impl DataAttributes {
    pub fn from_attrs(attrs: &[Attribute]) -> Result<Self> {
        let mut name = None;
        let mut repr = None;

        for attr in attrs {
            if attr.path.is_ident("repr") {
                if repr.is_some() {
                    bail!("Found multiple repr() attributes");
                }
                repr = Some(Self::parse_repr(attr).context("failed to parse repr")?);
            } else if attr.path.is_ident("pinion") {
                let nv = if let Meta::NameValue(nv) =
                    attr.parse_args().context("failed to parse args to meta")?
                {
                    nv
                } else {
                    bail!("pinion attribute must be key=val");
                };

                let nv_ident = nv
                    .path
                    .get_ident()
                    .ok_or(anyhow!("pinion attribute key must be ident"))?;

                match (nv_ident.to_string().as_str(), nv.lit) {
                    ("name", Lit::Str(s)) => name = Some(s.value()),
                    _ => bail!("unknown key: {}", nv_ident),
                }
            }
        }

        Ok(Self {
            name,
            repr: repr.ok_or(anyhow!("Struct must have a repr"))?,
        })
    }

    fn parse_repr(attr: &Attribute) -> Result<Repr> {
        let parser = |ps: ParseStream<'_>| {
            let args;
            parenthesized!(args in ps);
            Punctuated::<Ident, Comma>::parse_separated_nonempty(&args)
        };

        let repr_parts =
            Parser::parse2(parser, attr.tokens.clone()).context("failed to parse punct")?;

        let mut has_c = false;
        let mut disc_ty = None;

        for part in repr_parts {
            match part.to_string().as_str() {
                "C" if !has_c => has_c = true,
                // TODO: hmmm
                "transparent" if !has_c => has_c = true,
                "u8" if disc_ty.is_none() => disc_ty = Some(parse_quote! {u8}),
                "u16" if disc_ty.is_none() => disc_ty = Some(parse_quote! {u16}),
                "u32" if disc_ty.is_none() => disc_ty = Some(parse_quote! {u32}),
                "u64" if disc_ty.is_none() => disc_ty = Some(parse_quote! {u64}),
                p => bail!("Bad part in repr: {}", p),
            }
        }

        Ok(match (has_c, disc_ty) {
            (true, None) => Repr::Struct(StructRepr { packed: false }),
            (has_c, Some(disc_ty)) => Repr::Enum(EnumRepr { disc_ty, has_c }),
            (false, None) => unreachable!(),
        })
    }

    pub fn name(&self) -> Option<&str> {
        self.name.as_deref()
    }

    pub fn repr(&self) -> &Repr {
        &self.repr
    }
}
