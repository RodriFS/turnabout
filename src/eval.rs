use crate::{
    parser::{BinOperator, Expr},
    types::Type,
    utils::LiteralKind,
};

pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Self {
        Self {}
    }

    fn eval_literal(&self, expr: Expr) -> Type {
        match expr {
            Expr::Literal(LiteralKind::Float(v)) => Type::Float(v),
            Expr::Literal(LiteralKind::Int(v)) => Type::Int(v),
            Expr::Literal(LiteralKind::Str(v)) => Type::Str(v),
            Expr::Literal(LiteralKind::Bool(v)) => Type::Bool(v),
            _ => unimplemented!(),
        }
    }

    fn eval_binary(&self, op: BinOperator, left: Expr, right: Expr) -> Type {
        match op {
            BinOperator::Plus => self.eval(left) + self.eval(right),
            BinOperator::Minus => self.eval(left) - self.eval(right),
            BinOperator::Asterisk => self.eval(left) * self.eval(right),
            BinOperator::Slash => self.eval(left) / self.eval(right),
            BinOperator::GreaterThan => Type::Bool(self.eval(left) > self.eval(right)),
            BinOperator::GreaterThanEq => Type::Bool(self.eval(left) >= self.eval(right)),
            BinOperator::LessThan => Type::Bool(self.eval(left) < self.eval(right)),
            BinOperator::LessThanEq => Type::Bool(self.eval(left) <= self.eval(right)),
            BinOperator::Equal => Type::Bool(self.eval(left) == self.eval(right)),
            BinOperator::NotEqual => Type::Bool(self.eval(left) != self.eval(right)),
            _ => unimplemented!(),
        }
    }

    pub fn eval(&self, expr: Expr) -> Type {
        match expr {
            Expr::Binary {
                operator,
                left,
                right,
            } => self.eval_binary(operator, *left, *right),
            Expr::Grouping(expr) => self.eval(*expr),
            lit => self.eval_literal(lit),
        }
    }
}
