use std::cmp::Ordering;
use std::ops::Add;
use std::ops::Div;
use std::ops::Mul;
use std::ops::Neg;
use std::ops::Not;
use std::ops::Sub;

#[derive(Debug)]
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

    pub fn name(&self) -> &str {
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
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        match self {
            Type::Int(lhs) => match other {
                Type::Int(rhs) => Type::Int(lhs + rhs),
                Type::Float(rhs) => Type::Float(lhs as f64 + rhs),
                Type::Str(rhs) => Type::Str(format!("{}{}", lhs, rhs)),
                _ => Type::NaN,
            },
            Type::Float(lhs) => match other {
                Type::Float(rhs) => Type::Float(lhs + rhs),
                Type::Int(rhs) => Type::Float(lhs + rhs as f64),
                Type::Str(rhs) => Type::Str(format!("{}{}", lhs, rhs)),
                _ => Type::NaN,
            },
            Type::Str(lhs) => match other.to_type_string() {
                Type::Str(rhs) => Type::Str(format!("{}{}", lhs, rhs)),
                _ => unreachable!(),
            },
            lhsi_type => match other {
                Type::Str(rhs) => Type::Str(format!("{}{}", lhsi_type.to_string(), rhs)),
                _ => panic!("You can only add numbers"),
            },
        }
    }
}

impl Sub for Type {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        match self {
            Type::Int(lhs) => match other {
                Type::Int(rhs) => Type::Int(lhs - rhs),
                Type::Float(rhs) => Type::Float(lhs as f64 - rhs),
                t => panic!("Can't cast {} into Int", t.name()),
            },
            Type::Float(lhs) => match other {
                Type::Float(rhs) => Type::Float(lhs - rhs),
                Type::Int(rhs) => Type::Float(lhs - rhs as f64),
                t => panic!("Can't cast {} into Float", t.name()),
            },
            _ => match other {
                Type::Int(_) => panic!("Can't cast {} into Int", self.name()),
                Type::Float(_) => panic!("Can't cast {} into Float", self.name()),
                _ => panic!("You can only subtract numbers"),
            },
        }
    }
}

impl Mul for Type {
    type Output = Self;

    fn mul(self, other: Self) -> Self::Output {
        match self {
            Type::Int(lhs) => match other {
                Type::Int(rhs) => Type::Int(lhs * rhs),
                Type::Float(rhs) => Type::Float(lhs as f64 * rhs),
                t => panic!("Can't cast {} into Int", t.name()),
            },
            Type::Float(lhs) => match other {
                Type::Float(rhs) => Type::Float(lhs * rhs),
                Type::Int(rhs) => Type::Float(lhs * rhs as f64),
                t => panic!("Can't cast {} into Float", t.name()),
            },
            _ => match other {
                Type::Int(_) => panic!("Can't cast {} into Int", self.name()),
                Type::Float(_) => panic!("Can't cast {} into Float", self.name()),
                _ => panic!("You can only multiply numbers"),
            },
        }
    }
}

impl Div for Type {
    type Output = Self;

    fn div(self, other: Self) -> Self::Output {
        match self {
            Type::Int(lhs) => match other {
                Type::Int(rhs) => Type::Float(lhs as f64 / rhs as f64),
                Type::Float(rhs) => Type::Float(lhs as f64 / rhs),
                t => panic!("Can't cast {} into Int", t.name()),
            },
            Type::Float(lhs) => match other {
                Type::Float(rhs) => Type::Float(lhs / rhs),
                Type::Int(rhs) => Type::Float(lhs / rhs as f64),
                t => panic!("Can't cast {} into Float", t.name()),
            },
            _ => match other {
                Type::Int(_) => panic!("Can't cast {} into Int", self.name()),
                Type::Float(_) => panic!("Can't cast {} into Float", self.name()),
                _ => panic!("You can only divide numbers"),
            },
        }
    }
}

impl Not for Type {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Type::Bool(v) => Type::Bool(!v),
            _ => panic!("Not a binary type"),
        }
    }
}

impl Neg for Type {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Type::Int(v) => Type::Int(-v),
            Type::Float(v) => Type::Float(-v),
            _ => panic!("Can't negate a type that is not a number"),
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
