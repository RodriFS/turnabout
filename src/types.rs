use std::ops::Add;
use std::ops::Sub;

#[derive(Debug)]
pub enum Type {
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
}

impl Add for Type {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        match self {
            Type::Int(lhs) => match other {
                Type::Int(rhs) => Type::Int(lhs + rhs),
                Type::Float(rhs) => Type::Float(lhs as f64 + rhs),
                _ => panic!("No implicit castings"),
            },
            Type::Float(lhs) => match other {
                Type::Float(rhs) => Type::Float(lhs + rhs),
                Type::Int(rhs) => Type::Float(lhs + rhs as f64),
                _ => panic!("No implicit castings"),
            },
            _ => unimplemented!(),
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
                _ => panic!("No implicit castings"),
            },
            Type::Float(lhs) => match other {
                Type::Float(rhs) => Type::Float(lhs - rhs),
                Type::Int(rhs) => Type::Float(lhs - rhs as f64),
                _ => panic!("No implicit castings"),
            },
            _ => unimplemented!(),
        }
    }
}
