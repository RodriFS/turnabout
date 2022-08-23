use crate::{
    parser::{BinOperator, Expr, UnOperator},
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
            _ => unreachable!(),
        }
    }

    fn eval_unary(&self, op: UnOperator, right: Expr) -> Type {
        match op {
            UnOperator::Not => !self.eval(right),
            UnOperator::Negative => -self.eval(right),
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

    fn eval_sequence(&self, exprs: Vec<Box<Expr>>) -> Type {
        exprs
            .into_iter()
            .fold(Type::Unit, |_, expr| self.eval(*expr))
    }

    pub fn eval(&self, expr: Expr) -> Type {
        match expr {
            Expr::Program(expr) => self.eval(*expr),
            Expr::Sequence(exprs) => self.eval_sequence(exprs),
            Expr::Statement(expr) => self.eval(*expr),
            Expr::Grouping(expr) => self.eval(*expr),
            Expr::Unary { operator, right } => self.eval_unary(operator, *right),
            Expr::Binary {
                operator,
                left,
                right,
            } => self.eval_binary(operator, *left, *right),
            lit => self.eval_literal(lit),
        }
    }
}
