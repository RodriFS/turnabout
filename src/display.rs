use crate::types::Type;
use std::fmt::{Display, Formatter, Result};

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        // check https://github.com/rust-lang/rust/issues/45838 resolution
        match self.to_type_string() {
            Type::Str(v) => write!(f, "{}", v),
            _ => unreachable!(),
        }
    }
}
