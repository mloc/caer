use std::error::Error;

use syn::{parse_quote, Attribute, Lit, Meta};

pub struct StructAttributes {
    name: Option<String>,
    packed: bool,
}

impl StructAttributes {
    pub fn from_attrs(attrs: &[Attribute]) -> Result<Self, Box<dyn Error>> {
        let mut name = None;
        let mut packed = None;

        let repr_c = parse_quote! { #[repr(C)] };
        let repr_c_packed = parse_quote! { #[repr(C, packed)] };

        for attr in attrs {
            if *attr == repr_c {
                if packed.is_some() {
                    return Err("Found multiple repr() attributes".into());
                }
                packed = Some(false)
            } else if *attr == repr_c_packed {
                if packed.is_some() {
                    return Err("Found multiple repr() attributes".into());
                }
                packed = Some(true)
            } else if attr.path.is_ident("pinion") {
                attr.parse_meta()?;
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
            packed: packed.ok_or("Struct must be repr(C)")?,
        })
    }

    pub fn name(&self) -> Option<&str> {
        self.name.as_deref()
    }

    pub fn packed(&self) -> bool {
        self.packed
    }
}
