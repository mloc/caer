// don't rearrange either
#[derive(Debug, Copy, Clone, PartialEq)]
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
    LogAnd,
    LogOr,
    BitAnd,
    BitOr,
    BitXor,
}
