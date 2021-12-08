use serde::Serialize;

use crate::ty::{RefType, Type};

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
#[repr(u32)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le,
    Shl,
    Shr,
    BitAnd,
    BitOr,
    BitXor,
    Equiv,
    NotEquiv,
}

#[derive(Debug, Clone, Copy, Serialize)]
pub enum BitOp {
    And,
    Or,
    Xor,
    Shl,
    Shr,
}

#[derive(Debug, Clone, Copy, Serialize)]
pub enum HardBinary {
    StringConcat,

    FloatAdd,
    FloatSub,
    FloatMul,
    FloatDiv,
    FloatMod,
    FloatPow,
    FloatCmp(FloatPredicate),

    FloatBitOp(BitOp),
}

impl HardBinary {
    pub fn from_in_ty(op: BinaryOp, in_ty: (&Type, &Type)) -> Option<Self> {
        match (op, in_ty) {
            (BinaryOp::Add, (Type::Ref(RefType::String), Type::Ref(RefType::String))) => {
                Some(Self::StringConcat)
            },
            (BinaryOp::Add, (Type::Float, Type::Float)) => Some(Self::FloatAdd),
            (BinaryOp::Sub, (Type::Float, Type::Float)) => Some(Self::FloatSub),
            (BinaryOp::Mul, (Type::Float, Type::Float)) => Some(Self::FloatMul),
            (BinaryOp::Div, (Type::Float, Type::Float)) => Some(Self::FloatDiv),
            (BinaryOp::Mod, (Type::Float, Type::Float)) => Some(Self::FloatMod),
            (BinaryOp::Pow, (Type::Float, Type::Float)) => Some(Self::FloatPow),

            (BinaryOp::Eq, (Type::Float, Type::Float)) => Some(Self::FloatCmp(FloatPredicate::OEQ)),
            // if we have eq float lhs and non-float rhs, we can just return false
            // TODO: handle ints
            (BinaryOp::Eq, (Type::Float, _)) => {
                Some(Self::FloatCmp(FloatPredicate::PredicateFalse))
            },

            (BinaryOp::Ne, (Type::Float, Type::Float)) => Some(Self::FloatCmp(FloatPredicate::ONE)),
            // ditto, but always return true
            (BinaryOp::Ne, (Type::Float, _)) => Some(Self::FloatCmp(FloatPredicate::PredicateTrue)),

            (BinaryOp::Gt, (Type::Float, Type::Float)) => Some(Self::FloatCmp(FloatPredicate::OGT)),
            (BinaryOp::Ge, (Type::Float, Type::Float)) => Some(Self::FloatCmp(FloatPredicate::OGE)),
            (BinaryOp::Lt, (Type::Float, Type::Float)) => Some(Self::FloatCmp(FloatPredicate::OLT)),
            (BinaryOp::Le, (Type::Float, Type::Float)) => Some(Self::FloatCmp(FloatPredicate::OLE)),

            (BinaryOp::BitAnd, (Type::Float, Type::Float)) => Some(Self::FloatBitOp(BitOp::And)),
            (BinaryOp::BitOr, (Type::Float, Type::Float)) => Some(Self::FloatBitOp(BitOp::Or)),
            (BinaryOp::BitXor, (Type::Float, Type::Float)) => Some(Self::FloatBitOp(BitOp::Xor)),
            (BinaryOp::Shl, (Type::Float, Type::Float)) => Some(Self::FloatBitOp(BitOp::Shl)),
            (BinaryOp::Shr, (Type::Float, Type::Float)) => Some(Self::FloatBitOp(BitOp::Shr)),

            _ => None,
        }
    }

    pub fn out_ty(&self) -> Type {
        match self {
            Self::StringConcat => Type::Ref(RefType::String),

            Self::FloatAdd
            | Self::FloatSub
            | Self::FloatMul
            | Self::FloatDiv
            | Self::FloatMod
            | Self::FloatPow => Type::Float,
            // actually int I guess, or bool
            // TODO: fix when int is a type again
            Self::FloatCmp(_) => Type::Float,

            // these should be int output eventually
            // TODO: ints for bitops
            Self::FloatBitOp(_) => Type::Float,
        }
    }
}

// lifted from inkwell for now, TODO: move, reexpose?
#[derive(Debug, Clone, Copy, Serialize)]
pub enum FloatPredicate {
    OEQ,
    OGE,
    OGT,
    OLE,
    OLT,
    ONE,
    ORD,
    PredicateFalse,
    PredicateTrue,
    UEQ,
    UGE,
    UGT,
    ULE,
    ULT,
    UNE,
    UNO,
}
