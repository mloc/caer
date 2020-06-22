use caer_runtime::op::BinaryOp;

use super::ty::{Complex, Primitive};

use inkwell::FloatPredicate;

#[derive(Debug, Clone, Copy)]
pub enum HardBinary {
    StringConcat,
    FloatAdd,
    FloatSub,
    FloatMul,
    FloatDiv,
    FloatMod,
    FloatPow,
    FloatCmp(FloatPredicate),
}

impl HardBinary {
    pub fn from_in_ty(op: BinaryOp, in_ty: (Primitive, Primitive)) -> Option<Self> {
        match (op, in_ty) {
            (BinaryOp::Add, (Primitive::String, Primitive::String)) => Some(Self::StringConcat),
            (BinaryOp::Add, (Primitive::Float, Primitive::Float)) => Some(Self::FloatAdd),
            (BinaryOp::Sub, (Primitive::Float, Primitive::Float)) => Some(Self::FloatSub),
            (BinaryOp::Mul, (Primitive::Float, Primitive::Float)) => Some(Self::FloatMul),
            (BinaryOp::Div, (Primitive::Float, Primitive::Float)) => Some(Self::FloatDiv),
            (BinaryOp::Mod, (Primitive::Float, Primitive::Float)) => Some(Self::FloatMod),
            //(BinaryOp::Pow, (Primitive::Float, Primitive::Float)) => Some(Self::FloatPow),

            (BinaryOp::Eq, (Primitive::Float, Primitive::Float)) => {
                Some(Self::FloatCmp(FloatPredicate::OEQ))
            }
            // if we have eq float lhs and non-float rhs, we can just return false
            // TODO: handle ints
            (BinaryOp::Eq, (Primitive::Float, _)) => {
                Some(Self::FloatCmp(FloatPredicate::PredicateFalse))
            }

            (BinaryOp::Ne, (Primitive::Float, Primitive::Float)) => {
                Some(Self::FloatCmp(FloatPredicate::ONE))
            }
            // ditto, but always return true
            (BinaryOp::Ne, (Primitive::Float, _)) => {
                Some(Self::FloatCmp(FloatPredicate::PredicateTrue))
            }

            (BinaryOp::Gt, (Primitive::Float, Primitive::Float)) => {
                Some(Self::FloatCmp(FloatPredicate::OGT))
            }
            (BinaryOp::Ge, (Primitive::Float, Primitive::Float)) => {
                Some(Self::FloatCmp(FloatPredicate::OGE))
            }
            (BinaryOp::Lt, (Primitive::Float, Primitive::Float)) => {
                Some(Self::FloatCmp(FloatPredicate::OLT))
            }
            (BinaryOp::Le, (Primitive::Float, Primitive::Float)) => {
                Some(Self::FloatCmp(FloatPredicate::OLE))
            }
            _ => None,
        }
    }

    pub fn out_ty(&self) -> Complex {
        match self {
            Self::StringConcat => Primitive::String.into(),

            Self::FloatAdd
            | Self::FloatSub
            | Self::FloatMul
            | Self::FloatDiv
            | Self::FloatMod
            | Self::FloatPow => Primitive::Float.into(),
            // actually int I guess, or bool
            // TODO: fix when int is a type again
            Self::FloatCmp(_) => Primitive::Float.into(),
        }
    }
}
