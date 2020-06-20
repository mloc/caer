use caer_runtime::op::BinaryOp;

use super::ty::{Complex, Primitive};

#[derive(Debug, Clone, Copy)]
pub enum HardBinary {
    StringConcat,
    FloatAdd,
}

impl HardBinary {
    pub fn from_in_ty(op: BinaryOp, in_ty: (Primitive, Primitive)) -> Option<Self> {
        match (op, in_ty) {
            (BinaryOp::Add, (Primitive::String, Primitive::String)) => {
                Some(Self::StringConcat)
            }
            (BinaryOp::Add, (Primitive::Float, Primitive::Float)) => {
                Some(Self::FloatAdd)
            }
            _ => None,
        }
    }

    pub fn out_ty(&self) -> Complex {
        match self {
            Self::StringConcat => Primitive::String.into(),
            Self::FloatAdd => Primitive::Float.into(),
        }
    }
}
