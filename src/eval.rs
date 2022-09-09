use std::{cell::RefCell, rc::Rc};

use crate::{
    environment::Environment,
    errors::Error,
    parser::{BinOperator, Expr, UnOperator},
    types::Type,
    utils::LiteralKind,
};

type Env = Rc<RefCell<Environment>>;

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
            v => unreachable!("{:?}", v),
        }
    }

    fn eval_unary(&self, op: UnOperator, right: Expr, env: Env) -> Result<Type, Error> {
        // Check why ? not working
        match op {
            UnOperator::Not => match self.eval(right, env) {
                Ok(v) => !v,
                Err(e) => Err(e),
            },
            UnOperator::Negative => match self.eval(right, env) {
                Ok(v) => -v,
                Err(e) => Err(e),
            },
        }
    }

    fn eval_binary(
        &self,
        op: BinOperator,
        left: Expr,
        right: Expr,
        env: Env,
    ) -> Result<Type, Error> {
        match op {
            BinOperator::Plus => self.eval(left, env.clone())? + self.eval(right, env.clone())?,
            BinOperator::Minus => self.eval(left, env.clone())? - self.eval(right, env.clone())?,
            BinOperator::Asterisk => {
                self.eval(left, env.clone())? * self.eval(right, env.clone())?
            }
            BinOperator::Slash => self.eval(left, env.clone())? / self.eval(right, env.clone())?,
            BinOperator::GreaterThan => Ok(Type::Bool(
                self.eval(left, env.clone())? > self.eval(right, env.clone())?,
            )),
            BinOperator::GreaterThanEq => Ok(Type::Bool(
                self.eval(left, env.clone())? >= self.eval(right, env.clone())?,
            )),
            BinOperator::LessThan => Ok(Type::Bool(
                self.eval(left, env.clone())? < self.eval(right, env.clone())?,
            )),
            BinOperator::LessThanEq => Ok(Type::Bool(
                self.eval(left, env.clone())? <= self.eval(right, env.clone())?,
            )),
            BinOperator::Equal => Ok(Type::Bool(
                self.eval(left, env.clone())? == self.eval(right, env.clone())?,
            )),
            BinOperator::NotEqual => Ok(Type::Bool(
                self.eval(left, env.clone())? != self.eval(right, env.clone())?,
            )),
            BinOperator::Assignment => self.eval_assignment(left, right, env),
            _ => unimplemented!(),
        }
    }

    fn eval_if(&self, pred: Expr, ant: Expr, cons: Option<Expr>, env: Env) -> Result<Type, Error> {
        let predicate = self.eval(pred, env.clone())?;
        match predicate {
            Type::Bool(true) => self.eval(ant, env),
            Type::Bool(false) => match cons {
                Some(expr) => self.eval(expr, env),
                None => Ok(Type::Unit),
            },
            _ => Err(Error::TypeError(
                "Predicate does not evaluate to a boolean value",
            )),
        }
    }

    fn eval_block(&self, exprs: Vec<Box<Expr>>, env: Env) -> Result<Type, Error> {
        let env = Rc::new(RefCell::new(Environment::new(Some(env))));
        exprs
            .into_iter()
            .fold(Ok(Type::Unit), |_, expr| self.eval(*expr, env.clone()))
    }

    fn eval_variable(&self, name: String, env: Env) -> Result<Type, Error> {
        match env.borrow_mut().lookup(&name) {
            Some(v) => Ok(v),
            None => Err(Error::RuntimeError("Variable not declared")),
        }
    }

    fn eval_declaration(&self, expr: Expr, env: Env) -> Result<Type, Error> {
        match expr {
            Expr::Binary {
                operator: BinOperator::Assignment,
                left,
                right,
            } => match *left {
                Expr::Variable(name) => {
                    let value = self.eval(*right, env.clone())?;
                    env.borrow_mut().vars.insert(name, value);
                    Ok(Type::Unit)
                }
                _ => Err(Error::TypeError(
                    "Left hand of assignment is not a variable name",
                )),
            },
            _ => unreachable!(),
        }
    }

    fn eval_assignment(&self, left: Expr, right: Expr, env: Env) -> Result<Type, Error> {
        match left {
            Expr::Variable(name) => {
                let value = self.eval(right, env.clone())?;
                env.borrow_mut().assign(name, value)?;
                Ok(Type::Unit)
            }
            _ => {
                return Err(Error::TypeError(
                    "Left hand of assignment is not a variable name",
                ))
            }
        }
    }

    pub fn eval(&self, expr: Expr, env: Env) -> Result<Type, Error> {
        match expr {
            Expr::Program(expr) => self.eval(*expr, env),
            Expr::Block(exprs) => self.eval_block(exprs, env),
            Expr::Grouping(expr) => self.eval(*expr, env),
            Expr::If { pred, ant, cons } => self.eval_if(*pred, *ant, cons.map(|e| *e), env),
            Expr::Binary {
                operator,
                left,
                right,
            } => self.eval_binary(operator, *left, *right, env),
            Expr::Variable(name) => self.eval_variable(name, env),
            Expr::Declaration(expr) => self.eval_declaration(*expr, env),
            Expr::Unary { operator, right } => self.eval_unary(operator, *right, env),
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
        let env = Environment::new(None);
        interpreter.eval(ast, Rc::new(RefCell::new(env))).unwrap()
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
    fn test_block() {
        let main = eval("1+2; 2+2");
        assert_eq!(main, Int(4));

        let block = eval("{ 1+1; { 2+2; } }");
        assert_eq!(block, Int(4))
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
