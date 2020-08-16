//! semi-generic inference engine w/ subtyping
mod infer;

pub use infer::{InferEngine, InferKey, InferUnifyError, InferValue, Rule};
