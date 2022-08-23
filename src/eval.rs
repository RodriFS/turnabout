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
            Expr::Unit => Type::Unit,
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
            Expr::Binary {
                operator,
                left,
                right,
            } => self.eval_binary(operator, *left, *right),
            Expr::Unary { operator, right } => self.eval_unary(operator, *right),
            lit => self.eval_literal(lit),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cursor::*;
    use crate::lexer::*;
    use crate::parser::*;
    use crate::types::Type::*;

    fn eval<'a>(buffer: &'a str) -> Type {
        let cursor = Cursor::new(&buffer);
        let mut lexer = Lexer::new(cursor);
        let tokens = lexer.read();
        let mut parser = Parser::new(tokens.into_iter());
        let ast = parser.parse();
        let interpreter = Interpreter::new();
        interpreter.eval(ast)
    }

    #[test]
    fn test_literal() {
        let int = eval("1");
        assert_eq!(int, Int(1));

        let float = eval("1.1");
        assert_eq!(float, Float(1.1));

        let boolean = eval("true");
        assert_eq!(boolean, Bool(true));

        let string = eval("\"hello world\"");
        assert_eq!(string, Str("hello world".to_string()));
    }

    #[test]
    fn test_unary() {
        let not = eval("!true");
        assert_eq!(not, Bool(false));

        let not_not = eval("!!true");
        assert_eq!(not_not, Bool(true));

        let mut minus = eval("-4");
        assert_eq!(minus, Int(-4));
        minus = eval("-4.1");
        assert_eq!(minus, Float(-4.1));

        let mut minus_minus = eval("--4");
        assert_eq!(minus_minus, Int(4));
        minus_minus = eval("--4.1");
        assert_eq!(minus_minus, Float(4.1));
    }

    #[test]
    fn test_binary_sum() {
        let mut sum = eval("1 + 1");
        assert_eq!(sum.dbg(), Int(2).dbg());
        sum = eval("1.1 + 1");
        assert_eq!(sum.dbg(), Float(2.1).dbg());
        sum = eval("1 + 1.1");
        assert_eq!(sum.dbg(), Float(2.1).dbg());
        sum = eval("1.1 + 1.1");
        assert_eq!(sum.dbg(), Float(2.2).dbg());
        sum = eval("\"hello\" + 1");
        assert_eq!(sum, Str("hello1".to_string()));
        sum = eval("\"hello\" + 1.1");
        assert_eq!(sum, Str("hello1.1".to_string()));
        sum = eval("\"hello\" + true");
        assert_eq!(sum, Str("hellotrue".to_string()));
        sum = eval("\"hello\" + \"world\"");
        assert_eq!(sum, Str("helloworld".to_string()));
        sum = eval("\"hello\" + (1.1 + true)");
        assert_eq!(sum, Str("helloNaN".to_string()));
        sum = eval("\"hello\" + ()");
        assert_eq!(sum, Str("hello()".to_string()));
        sum = eval("1 + \"hello\"");
        assert_eq!(sum, Str("1hello".to_string()));
        sum = eval("1.1 + \"hello\"");
        assert_eq!(sum, Str("1.1hello".to_string()));
        sum = eval("true + \"hello\"");
        assert_eq!(sum, Str("truehello".to_string()));
        sum = eval("(1.1 + true) + \"hello\"");
        assert_eq!(sum, Str("NaNhello".to_string()));
        sum = eval("() + \"hello\"");
        assert_eq!(sum, Str("()hello".to_string()));
    }

    #[test]
    fn test_binary_sub() {
        let mut sub = eval("2 - 1");
        assert_eq!(sub.dbg(), Int(1).dbg());
        sub = eval("2 - 1.2");
        assert_eq!(sub.dbg(), Float(0.8).dbg());
        sub = eval("2.1 - 1.0");
        assert_eq!(sub.dbg(), Float(1.1).dbg());
        sub = eval("2.2 - 1.1");
        assert_eq!(sub.dbg(), Float(1.1).dbg())
    }

    #[test]
    fn test_binary_mul() {
        let mut mul = eval("4 * 2");
        assert_eq!(mul.dbg(), Int(8).dbg());
        mul = eval("2 * 1.5");
        assert_eq!(mul.dbg(), Float(3.0).dbg());
        mul = eval("2.5 * 2");
        assert_eq!(mul.dbg(), Float(5.0).dbg());
        mul = eval("2.2 * 2.0");
        assert_eq!(mul.dbg(), Float(4.4).dbg())
    }

    #[test]
    fn test_binary_div() {
        let mut div = eval("4 / 2");
        assert_eq!(div.dbg(), Float(2.0).dbg());
        div = eval("3 / 1.5");
        assert_eq!(div.dbg(), Float(2.0).dbg());
        div = eval("2.5 / 2");
        assert_eq!(div.dbg(), Float(1.25).dbg());
        div = eval("2.2 / 2.0");
        assert_eq!(div.dbg(), Float(1.1).dbg())
    }

    #[test]
    fn test_sequence() {
        let result = eval("1+2; 2+2");
        assert_eq!(result, Int(4));
    }
}
