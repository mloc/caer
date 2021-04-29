use thiserror::Error;
use std::io;

#[derive(Error, Debug)]
pub enum GeneratorError {
    #[error("error writing output")]
    WriteError(#[from] io::Error),
    #[error("unknown generator error")]
    Unknown,
}
