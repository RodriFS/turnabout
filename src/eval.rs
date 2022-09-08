use crate::{
    errors::Error,
    parser::{BinOperator, Expr, UnOperator},
    types::Type,
    utils::LiteralKind,
};

pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Self {
        Self {}
    }

    fn eval_literal(&self, expr: Expr) -> Result<Type, Error> {
        match expr {
            Expr::Literal(LiteralKind::Float(v)) => Ok(Type::Float(v)),
            Expr::Literal(LiteralKind::Int(v)) => Ok(Type::Int(v)),
            Expr::Literal(LiteralKind::Str(v)) => Ok(Type::Str(v)),
            Expr::Literal(LiteralKind::Bool(v)) => Ok(Type::Bool(v)),
            Expr::Unit => Ok(Type::Unit),
            _ => unreachable!(),
        }
    }

    fn eval_unary(&self, op: UnOperator, right: Expr) -> Result<Type, Error> {
        // Check why ? not working
        match op {
            UnOperator::Not => match self.eval(right) {
                Ok(v) => !v,
                Err(e) => Err(e),
            },
            UnOperator::Negative => match self.eval(right) {
                Ok(v) => -v,
                Err(e) => Err(e),
            },
        }
    }

    fn eval_binary(&self, op: BinOperator, left: Expr, right: Expr) -> Result<Type, Error> {
        match op {
            BinOperator::Plus => self.eval(left)? + self.eval(right)?,
            BinOperator::Minus => self.eval(left)? - self.eval(right)?,
            BinOperator::Asterisk => self.eval(left)? * self.eval(right)?,
            BinOperator::Slash => self.eval(left)? / self.eval(right)?,
            BinOperator::GreaterThan => Ok(Type::Bool(self.eval(left)? > self.eval(right)?)),
            BinOperator::GreaterThanEq => Ok(Type::Bool(self.eval(left)? >= self.eval(right)?)),
            BinOperator::LessThan => Ok(Type::Bool(self.eval(left)? < self.eval(right)?)),
            BinOperator::LessThanEq => Ok(Type::Bool(self.eval(left)? <= self.eval(right)?)),
            BinOperator::Equal => Ok(Type::Bool(self.eval(left)? == self.eval(right)?)),
            BinOperator::NotEqual => Ok(Type::Bool(self.eval(left)? != self.eval(right)?)),
            _ => unimplemented!(),
        }
    }

    fn eval_if(&self, pred: Expr, ant: Expr, cons: Option<Expr>) -> Result<Type, Error> {
        let predicate = self.eval(pred)?;
        match predicate {
            Type::Bool(true) => self.eval(ant),
            Type::Bool(false) => match cons {
                Some(expr) => self.eval(expr),
                None => Ok(Type::Unit),
            },
            _ => Err(Error::TypeError(
                "Predicate does not evaluate to a boolean value",
            )),
        }
    }

    fn eval_sequence(&self, exprs: Vec<Box<Expr>>) -> Result<Type, Error> {
        exprs
            .into_iter()
            .fold(Ok(Type::Unit), |_, expr| self.eval(*expr))
    }

    pub fn eval(&self, expr: Expr) -> Result<Type, Error> {
        match expr {
            Expr::Program(expr) => self.eval(*expr),
            Expr::Sequence(exprs) => self.eval_sequence(exprs),
            Expr::Grouping(expr) => self.eval(*expr),
            Expr::If { pred, ant, cons } => self.eval_if(*pred, *ant, cons.map(|e| *e)),
            Expr::Binary {
                operator,
                left,
                right,
            } => self.eval_binary(operator, *left, *right),
            Expr::Unary { operator, right } => self.eval_unary(operator, *right),
            literal => self.eval_literal(literal),
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
    use pretty_assertions::assert_eq;

    fn eval<'a>(buffer: &'a str) -> Type {
        let cursor = Cursor::new(&buffer);
        let mut lexer = Lexer::new(cursor);
        let tokens = lexer.read();
        let mut parser = Parser::new(tokens.into_iter());
        let ast = parser.parse();
        let interpreter = Interpreter::new();
        interpreter.eval(ast).unwrap()
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
    fn test_grouping() {
        let mut group = eval("(1 + 2)");
        assert_eq!(group, Int(3));
        group = eval("(1 + 2) * 3");
        assert_eq!(group, Int(9));
        group = eval("3 * (1 + 2)");
        assert_eq!(group, Int(9));
        group = eval("1 + (2 * 3) + 4");
        assert_eq!(group, Int(11));
        group = eval("2 * (1 + (3 * 4)) * (10)");
        assert_eq!(group, Int(260));
        group = eval("-(-10)");
        assert_eq!(group, Int(10));
    }

    #[test]
    fn test_sequence() {
        let result = eval("1+2; 2+2");
        assert_eq!(result, Int(4));
    }

    #[test]
    fn test_if() {
        let no_else_true = eval("if true 4");
        assert_eq!(no_else_true, Int(4));

        let with_else_true = eval("if true 4 else 1");
        assert_eq!(with_else_true, Int(4));

        let with_else_false = eval("if false 4 else 1");
        assert_eq!(with_else_false, Int(1));
    }
}
