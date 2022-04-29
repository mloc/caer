use anyhow::{anyhow, bail, Context, Result};
use syn::parse::{ParseStream, Parser};
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::{parenthesized, Attribute, Ident, Lit, Meta};

#[derive(Debug, Clone, Copy)]
pub enum Repr {
    Struct { packed: bool },
    Enum { disc_width: i32, has_c: bool },
}

impl Repr {
    pub fn as_struct(self) -> Option<bool> {
        if let Self::Struct { packed } = self {
            Some(packed)
        } else {
            None
        }
    }

    pub fn as_enum(self) -> Option<(i32, bool)> {
        if let Self::Enum { disc_width, has_c } = self {
            Some((disc_width, has_c))
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
        let mut width = None;

        for part in repr_parts {
            match part.to_string().as_str() {
                "C" if !has_c => has_c = true,
                // TODO: hmmm
                "transparent" if !has_c => has_c = true,
                "u8" if width.is_none() => width = Some(1),
                "u16" if width.is_none() => width = Some(2),
                "u32" if width.is_none() => width = Some(4),
                "u64" if width.is_none() => width = Some(8),
                p => bail!("Bad part in repr: {}", p),
            }
        }

        Ok(match (has_c, width) {
            (true, None) => Repr::Struct { packed: false },
            (has_c, Some(w)) => Repr::Enum {
                disc_width: w,
                has_c,
            },
            (false, None) => unreachable!(),
        })
    }

    pub fn name(&self) -> Option<&str> {
        self.name.as_deref()
    }

    pub fn repr(&self) -> Repr {
        self.repr
    }
}
