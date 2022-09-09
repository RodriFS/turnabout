use std::fmt::Display;

#[derive(Debug)]
pub enum Error {
    TypeError(&'static str),
    RuntimeError(&'static str),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::TypeError(err) => write!(f, "TypeError: {}", err),
            Error::RuntimeError(err) => write!(f, "RuntimeError: {}", err),
        }
    }
}
