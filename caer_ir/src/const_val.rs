use caer_types::id::StringId;
use ordered_float::OrderedFloat;

#[derive(Debug)]
pub enum ConstVal {
    Null,
    Float(OrderedFloat<f32>),
    String(StringId),
}
