use std::error::Error;

use quote::quote;
use syn::{parse_quote, Attribute, Lit, Meta};

#[derive(Debug, Clone, Copy)]
pub enum Repr {
    Struct { packed: bool },
    Enum { disc_width: i32 },
}

impl Repr {
    pub fn as_struct(self) -> Option<bool> {
        if let Self::Struct { packed } = self {
            Some(packed)
        } else {
            None
        }
    }

    pub fn as_enum(self) -> Option<i32> {
        if let Self::Enum { disc_width } = self {
            Some(disc_width)
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
    pub fn from_attrs(attrs: &[Attribute]) -> Result<Self, Box<dyn Error>> {
        let mut name = None;
        let mut repr = None;

        for attr in attrs {
            if attr.path.is_ident("repr") {
                if repr.is_some() {
                    return Err("Found multiple repr() attributes".into());
                }
                repr = Some(Self::parse_repr(attr)?);
            } else if attr.path.is_ident("pinion") {
                let nv = if let Meta::NameValue(nv) = attr.parse_args()? {
                    nv
                } else {
                    return Err("pinion attribute must be key=val".into());
                };

                let nv_ident = nv
                    .path
                    .get_ident()
                    .ok_or("pinion attribute key must be ident")?;

                match (nv_ident.to_string().as_str(), nv.lit) {
                    ("name", Lit::Str(s)) => name = Some(s.value()),
                    _ => return Err(format!("unknown key: {}", nv_ident).into()),
                }
            }
        }

        Ok(Self {
            name,
            repr: repr.ok_or("Struct must have a repr")?,
        })
    }

    fn parse_repr(attr: &Attribute) -> Result<Repr, Box<dyn Error>> {
        let cases = [
            (parse_quote! {repr(C)}, Repr::Struct { packed: false }),
            (
                parse_quote! {repr(C, packed)},
                Repr::Struct { packed: false },
            ),
            (parse_quote! {repr(u8)}, Repr::Enum { disc_width: 1 }),
            (parse_quote! {repr(u16)}, Repr::Enum { disc_width: 2 }),
            (parse_quote! {repr(u32)}, Repr::Enum { disc_width: 4 }),
            (parse_quote! {repr(u64)}, Repr::Enum { disc_width: 8 }),
        ];

        let meta = attr.parse_meta()?;

        for (ts, repr) in cases {
            if meta == ts {
                return Ok(repr);
            }
        }

        Err("unrecognized repr".into())
    }

    pub fn name(&self) -> Option<&str> {
        self.name.as_deref()
    }

    pub fn repr(&self) -> Repr {
        self.repr
    }
}
