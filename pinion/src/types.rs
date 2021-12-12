use crate::{float_type, int_type, scalar_type};

scalar_type!(pub Void, void_type, VoidType);

int_type!(pub Bool, bool_type);
int_type!(pub Int8, i8_type);
int_type!(pub Int16, i16_type);
int_type!(pub Int32, i32_type);
int_type!(pub Int64, i64_type);

float_type!(pub Float16, f16_type);
float_type!(pub Float32, f32_type);
float_type!(pub Float64, f64_type);
