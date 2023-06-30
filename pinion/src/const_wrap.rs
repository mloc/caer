use ordered_float::OrderedFloat;

use crate::layout_ctx::{LayoutCtx, LayoutId};
use crate::traits::PinionData;

pub trait PinionConstWrap: PinionData {
    fn const_wrap(&self, lctx: &mut LayoutCtx) -> ConstItem;
}

#[derive(Debug)]
pub struct ConstItem {
    pub layout_id: LayoutId,
    pub value: ConstValue,
}

impl ConstItem {
    pub fn make_for<T: PinionConstWrap>(lctx: &mut LayoutCtx, value: ConstValue) -> Self {
        Self {
            layout_id: lctx.populate::<T>(),
            value,
        }
    }
}

// TODO: open question: where do we desugar enums? should there be an Enum variant here, or should
// the derive desugar it into Struct? for now it's the latter, but revisit, since it puts the
// desugaring in two different places
#[derive(Debug)]
pub enum ConstValue {
    Prim(ConstPrim),
    Struct(Vec<ConstItem>),
    // Unit enum
    Enum(Box<ConstItem>),
    // Fieldy enum. (tag enum, field union)
    TaggedUnion(Box<(ConstItem, Option<ConstItem>)>),
}

#[derive(Debug)]
pub enum ConstPrim {
    Bool(bool),
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    UInt8(u8),
    UInt16(u16),
    UInt32(u32),
    UInt64(u64),
    Float32(f32),
    Float64(f64),
}

impl From<ConstPrim> for ConstValue {
    fn from(value: ConstPrim) -> Self {
        ConstValue::Prim(value)
    }
}

macro_rules! prim_types {
    ($($prim:ty => $var:ident),+ $(,)?) => {
        $(
            impl PinionConstWrap for $prim {
                fn const_wrap(&self, lctx: &mut LayoutCtx) -> ConstItem {
                    ConstItem::make_for::<Self>(lctx, ConstPrim::$var(self.clone().into()).into())
                }
            }
        )+
    };
}

prim_types! {
    bool => Bool,
    i8 => Int8,
    u8 => UInt8,
    i16 => Int16,
    u16 => UInt16,
    i32 => Int32,
    u32 => UInt32,
    i64 => Int64,
    u64 => UInt64,
    f32 => Float32,
    OrderedFloat<f32> => Float32,
    f64 => Float64,
    OrderedFloat<f64> => Float64,
}
