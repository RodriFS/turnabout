use crate::{
    lexer::{Token, TokenType},
    utils::LiteralKind,
};
use std::iter::Peekable;

#[derive(Debug, PartialEq)]
pub enum BinOperator {
    Assignment,
    GreaterThan,
    LessThan,
    GreaterThanEq,
    LessThanEq,
    Equal,
    NotEqual,
    Plus,
    Minus,
    Asterisk,
    Slash,
}

pub enum Fixity {
    Left,
    Right,
}

impl BinOperator {
    fn precendence(&self) -> usize {
        use BinOperator::*;
        match self {
            Assignment => 1,
            GreaterThan | LessThan | GreaterThanEq | LessThanEq | Equal | NotEqual => 2,
            Plus | Minus => 3,
            Asterisk | Slash => 4,
        }
    }

    fn fixity(&self) -> Fixity {
        use BinOperator::*;
        match self {
            Assignment => Fixity::Right,
            _ => Fixity::Left,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum UnOperator {
    Not,
    Negative,
}

fn convert_un_operator(token: &TokenType) -> Option<UnOperator> {
    let op = match token {
        TokenType::Not => UnOperator::Not,
        TokenType::Minus => UnOperator::Negative,
        _ => return None,
    };
    Some(op)
}

fn convert_bin_operator(token: &TokenType) -> Option<BinOperator> {
    let op = match token {
        TokenType::Assignment => BinOperator::Assignment,
        TokenType::GreaterThan => BinOperator::GreaterThan,
        TokenType::LessThan => BinOperator::LessThan,
        TokenType::GreaterThanEq => BinOperator::GreaterThanEq,
        TokenType::LessThanEq => BinOperator::LessThanEq,
        TokenType::Equal => BinOperator::Equal,
        TokenType::NotEqual => BinOperator::NotEqual,
        TokenType::Plus => BinOperator::Plus,
        TokenType::Minus => BinOperator::Minus,
        TokenType::Asterisk => BinOperator::Asterisk,
        TokenType::Slash => BinOperator::Slash,
        _ => return None,
    };
    Some(op)
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Literal(LiteralKind),
    Lambda {
        name: String,
        vars: Vec<String>,
        body: Box<Expr>,
    },
    FnCall {
        func: Box<Expr>,
        args: Box<Expr>,
    },
    If {
        cond: Box<Expr>,
        then: Box<Expr>,
        els: Option<Box<Expr>>,
    },
    Unary {
        operator: UnOperator,
        right: Box<Expr>,
    },
    Binary {
        operator: BinOperator,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Grouping(Box<Expr>),
    Sequence(Vec<Box<Expr>>),
    Statement(Box<Expr>),
    Program(Box<Expr>),
    Unit,
    Ignore(TokenType),
}

impl<'a> Expr {
    fn make_unary(operator: UnOperator, right: Expr) -> Expr {
        Expr::Unary {
            operator,
            right: Box::new(right),
        }
    }

    fn make_binary(operator: BinOperator, left: Expr, right: Expr) -> Expr {
        Expr::Binary {
            operator,
            left: Box::new(left),
            right: Box::new(right),
        }
    }
}

pub struct Parser<I: Iterator<Item = Token>> {
    tokens: Peekable<I>,
}

impl<'a, I: Iterator<Item = Token>> Parser<I> {
    pub fn new(tokens: I) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    fn expect(&mut self, expected: TokenType) {
        if let Some(token) = self.peek() {
            if token.ttype != expected {
                panic!("Token not expected")
            }
        }
    }

    fn check(&mut self, expected: TokenType) -> bool {
        if let Some(token) = self.peek() {
            return token.ttype == expected;
        }
        false
    }

    fn next(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    fn peek_un_operator(&mut self) -> Option<UnOperator> {
        self.peek().and_then(|t| convert_un_operator(&t.ttype))
    }

    fn peek_bin_operator(&mut self) -> Option<BinOperator> {
        self.peek().and_then(|t| convert_bin_operator(&t.ttype))
    }

    fn take_un_operator(&mut self) -> Option<UnOperator> {
        self.next().and_then(|t| convert_un_operator(&t.ttype))
    }

    fn parse_primary(&mut self) -> Expr {
        match self.next() {
            Some(Token {
                ttype: TokenType::Literal { kind },
            }) => Expr::Literal(kind),
            Some(Token {
                ttype: TokenType::LeftParen,
            }) => {
                if self.check(TokenType::RightParen) {
                    self.next();
                    return Expr::Unit;
                }
                let expr = self.parse_expression();
                dbg!(&expr);
                self.expect(TokenType::RightParen);
                self.next();
                Expr::Grouping(Box::new(expr))
            }
            Some(Token { ttype: rest }) => Expr::Ignore(rest),
            None => panic!("Unknown Error"),
        }
    }

    fn parse_unary(&mut self) -> Expr {
        if self.peek_un_operator().is_some() {
            let operator = self.take_un_operator().unwrap();
            let right = self.parse_unary();
            return Expr::make_unary(operator, right);
        }
        self.parse_primary()
    }

    fn parse_binary(&mut self, min_prec: usize) -> Expr {
        let mut left = self.parse_unary();

        while let Some(op) = self.peek_bin_operator() {
            let prec = op.precendence();
            if prec < min_prec {
                break;
            }

            self.next();

            let prec_adjustment = match op.fixity() {
                Fixity::Right => 0,
                Fixity::Left => 1,
            };

            let right = self.parse_binary(prec + prec_adjustment);

            left = Expr::make_binary(op, left, right);
        }

        left
    }

    fn parse_expression(&mut self) -> Expr {
        let bin = self.parse_binary(0);
        if self.check(TokenType::Semicolon) {
            self.next();
        }
        Expr::Statement(Box::new(bin))
    }

    pub fn parse(&mut self) -> Expr {
        let mut exprs = vec![];
        while let Expr::Statement(stmt) = self.parse_expression() {
            exprs.push(stmt);
            if self.check(TokenType::EOF) {
                break;
            }
        }
        let expr;
        if exprs.len() == 1 {
            expr = Expr::Program(exprs.pop().unwrap());
        } else {
            expr = Expr::Program(Box::new(Expr::Sequence(exprs)));
        }
        self.expect(TokenType::EOF);
        expr
    }
}

#[cfg(test)]
mod tests {
    use crate::cursor::*;
    use crate::lexer::*;
    use crate::parser::{LiteralKind::*, *};

    fn parse<'a>(buffer: &'a str) -> Expr {
        let cursor = Cursor::new(&buffer);
        let mut lexer = Lexer::new(cursor);
        let tokens = lexer.read();
        let mut parser = Parser::new(tokens.into_iter());
        parser.parse()
    }

    #[test]
    fn test_unary() {
        let one = parse("!true;");
        assert_eq!(
            one,
            Expr::Program(Box::new(Expr::Unary {
                operator: UnOperator::Not,
                right: Box::new(Expr::Literal(Bool(true))),
            })),
        );

        let two = parse("!-false;");
        assert_eq!(
            two,
            Expr::Program(Box::new(Expr::Unary {
                operator: UnOperator::Not,
                right: Box::new(Expr::Unary {
                    operator: UnOperator::Negative,
                    right: Box::new(Expr::Literal(Bool(false)))
                })
            }))
        );
    }

    #[test]
    fn test_binary() {
        let assoc = parse("1 + 2 + 3;");
        assert_eq!(
            assoc,
            Expr::Program(Box::new(Expr::Binary {
                operator: BinOperator::Plus,
                left: Box::new(Expr::Binary {
                    operator: BinOperator::Plus,
                    left: Box::new(Expr::Literal(Int(1))),
                    right: Box::new(Expr::Literal(Int(2)))
                }),
                right: Box::new(Expr::Literal(Int(3))),
            }))
        );

        let prec1 = parse("1 * 2 + 3;");
        assert_eq!(
            prec1,
            Expr::Program(Box::new(Expr::Binary {
                operator: BinOperator::Plus,
                left: Box::new(Expr::Binary {
                    operator: BinOperator::Asterisk,
                    left: Box::new(Expr::Literal(Int(1))),
                    right: Box::new(Expr::Literal(Int(2))),
                }),
                right: Box::new(Expr::Literal(Int(3))),
            }))
        );

        let prec2 = parse("3 + 1 * 2;");
        assert_eq!(
            prec2,
            Expr::Program(Box::new(Expr::Binary {
                operator: BinOperator::Plus,
                left: Box::new(Expr::Literal(Int(3))),
                right: Box::new(Expr::Binary {
                    operator: BinOperator::Asterisk,
                    left: Box::new(Expr::Literal(Int(1))),
                    right: Box::new(Expr::Literal(Int(2))),
                }),
            }))
        );
    }
}
