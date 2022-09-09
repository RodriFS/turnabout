use std::cmp::Ordering;
use std::ops::Add;
use std::ops::Div;
use std::ops::Mul;
use std::ops::Neg;
use std::ops::Not;
use std::ops::Sub;

use crate::errors::Error;

#[derive(Debug, Clone)]
pub enum Type {
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    NaN,
    Unit,
}

impl Type {
    pub fn to_type_string(&self) -> Type {
        match self {
            Type::Int(v) => Type::Str(v.to_string()),
            Type::Float(v) => Type::Str(v.to_string()),
            Type::Bool(v) => Type::Str(v.to_string()),
            Type::NaN => Type::Str("NaN".to_string()),
            Type::Str(v) => Type::Str(v.to_string()),
            Type::Unit => Type::Str("()".to_string()),
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            Type::Int(_) => "Int",
            Type::Float(_) => "Float",
            Type::Bool(_) => "Bool",
            Type::NaN => "NaN",
            Type::Str(_) => "String",
            Type::Unit => "Unit",
        }
    }

    // For testing equality of enum variant, since Type implements PartialEq
    pub fn dbg(&self) -> String {
        format!("{:?}", self)
    }
}

impl Add for Type {
    type Output = Result<Self, Error>;

    fn add(self, other: Self) -> Self::Output {
        match self {
            Type::Int(lhs) => match other {
                Type::Int(rhs) => Ok(Type::Int(lhs + rhs)),
                Type::Float(rhs) => Ok(Type::Float(lhs as f64 + rhs)),
                Type::Str(rhs) => Ok(Type::Str(format!("{}{}", lhs, rhs))),
                _ => Ok(Type::NaN),
            },
            Type::Float(lhs) => match other {
                Type::Float(rhs) => Ok(Type::Float(lhs + rhs)),
                Type::Int(rhs) => Ok(Type::Float(lhs + rhs as f64)),
                Type::Str(rhs) => Ok(Type::Str(format!("{}{}", lhs, rhs))),
                _ => Ok(Type::NaN),
            },
            Type::Str(lhs) => match other.to_type_string() {
                Type::Str(rhs) => Ok(Type::Str(format!("{}{}", lhs, rhs))),
                _ => unreachable!(),
            },
            lhsi_type => match other {
                Type::Str(rhs) => Ok(Type::Str(format!("{}{}", lhsi_type.to_string(), rhs))),
                Type::Int(_) => Err(Error::TypeError("Can't cast Type into Int")),
                Type::Float(_) => Err(Error::TypeError("Can't cast Type into Float")),
                _ => Err(Error::TypeError("You can only add numbers")),
            },
        }
    }
}

impl Sub for Type {
    type Output = Result<Self, Error>;

    fn sub(self, other: Self) -> Self::Output {
        match self {
            Type::Int(lhs) => match other {
                Type::Int(rhs) => Ok(Type::Int(lhs - rhs)),
                Type::Float(rhs) => Ok(Type::Float(lhs as f64 - rhs)),
                _ => Err(Error::TypeError("Can't cast Type into Int")),
            },
            Type::Float(lhs) => match other {
                Type::Float(rhs) => Ok(Type::Float(lhs - rhs)),
                Type::Int(rhs) => Ok(Type::Float(lhs - rhs as f64)),
                _ => Err(Error::TypeError("Can't cast Type into Float")),
            },
            _ => match other {
                Type::Int(_) => Err(Error::TypeError("Can't cast Type into Int")),
                Type::Float(_) => Err(Error::TypeError("Can't cast Type into Float")),
                _ => Err(Error::TypeError("You can only subtract numbers")),
            },
        }
    }
}

impl Mul for Type {
    type Output = Result<Self, Error>;

    fn mul(self, other: Self) -> Self::Output {
        match self {
            Type::Int(lhs) => match other {
                Type::Int(rhs) => Ok(Type::Int(lhs * rhs)),
                Type::Float(rhs) => Ok(Type::Float(lhs as f64 * rhs)),
                _ => Err(Error::TypeError("Can't cast Type into Int")),
            },
            Type::Float(lhs) => match other {
                Type::Float(rhs) => Ok(Type::Float(lhs * rhs)),
                Type::Int(rhs) => Ok(Type::Float(lhs * rhs as f64)),
                _ => Err(Error::TypeError("Can't cast Type into Float")),
            },
            _ => match other {
                Type::Int(_) => Err(Error::TypeError("Can't cast Type into Int")),
                Type::Float(_) => Err(Error::TypeError("Can't cast Type into Float")),
                _ => Err(Error::TypeError("You can only multiply numbers")),
            },
        }
    }
}

impl Div for Type {
    type Output = Result<Self, Error>;

    fn div(self, other: Self) -> Self::Output {
        match self {
            Type::Int(lhs) => match other {
                Type::Int(rhs) => Ok(Type::Float(lhs as f64 / rhs as f64)),
                Type::Float(rhs) => Ok(Type::Float(lhs as f64 / rhs)),
                _ => Err(Error::TypeError("Can't cast Type into Int")),
            },
            Type::Float(lhs) => match other {
                Type::Float(rhs) => Ok(Type::Float(lhs / rhs)),
                Type::Int(rhs) => Ok(Type::Float(lhs / rhs as f64)),
                _ => Err(Error::TypeError("Can't cast Type into Float")),
            },
            _ => match other {
                Type::Int(_) => Err(Error::TypeError("Can't cast Type into Int")),
                Type::Float(_) => Err(Error::TypeError("Can't cast Type into Float")),
                _ => Err(Error::TypeError("You can only divide numbers")),
            },
        }
    }
}

impl Not for Type {
    type Output = Result<Self, Error>;

    fn not(self) -> Self::Output {
        match self {
            Type::Bool(v) => Ok(Type::Bool(!v)),
            _ => Err(Error::TypeError("Not a binary type")),
        }
    }
}

impl Neg for Type {
    type Output = Result<Self, Error>;

    fn neg(self) -> Self::Output {
        match self {
            Type::Int(v) => Ok(Type::Int(-v)),
            Type::Float(v) => Ok(Type::Float(-v)),
            _ => Err(Error::TypeError("Can't negate a type that is not a number")),
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Type::Int(lhs) => match other {
                Type::Int(rhs) => lhs == rhs,
                Type::Float(rhs) => *lhs as f64 == *rhs,
                _ => false,
            },
            Type::Float(lhs) => match other {
                Type::Int(rhs) => *lhs == *rhs as f64,
                Type::Float(rhs) => lhs == rhs,
                _ => false,
            },
            Type::NaN => match other {
                Type::NaN => true,
                _ => false,
            },
            Type::Str(lhs) => match other {
                Type::Str(rhs) => lhs == rhs,
                _ => false,
            },
            Type::Bool(lhs) => match other {
                Type::Bool(rhs) => lhs == rhs,
                _ => false,
            },
            Type::Unit => false,
        }
    }

    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
    }
}

impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self {
            Type::Int(lhs) => match other {
                Type::Int(rhs) => lhs.partial_cmp(rhs),
                Type::Float(rhs) => (*lhs as f64).partial_cmp(rhs),
                _ => None,
            },
            Type::Float(lhs) => match other {
                Type::Int(rhs) => lhs.partial_cmp(&(*rhs as f64)),
                Type::Float(rhs) => lhs.partial_cmp(rhs),
                _ => None,
            },
            Type::Str(lhs) => match other {
                Type::Str(rhs) => lhs.partial_cmp(rhs),
                _ => None,
            },
            Type::Bool(lhs) => match other {
                Type::Bool(rhs) => lhs.partial_cmp(rhs),
                _ => None,
            },
            Type::NaN => None,
            Type::Unit => None,
        }
    }
}
