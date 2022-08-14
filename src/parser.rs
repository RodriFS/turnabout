use crate::lexer::{Token, TokenType};
use std::iter::{self, Peekable};

#[derive(Debug, PartialEq)]
pub enum NumberType {
    Float(f64),
    Int(i64),
}

#[derive(Debug, PartialEq)]
pub enum Operator {
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

fn convert_operator(token: TokenType) -> Option<Operator> {
    let op = match token {
        TokenType::Assignment => Operator::Assignment,
        TokenType::GreaterThan => Operator::GreaterThan,
        TokenType::LessThan => Operator::LessThan,
        TokenType::GreaterThanEq => Operator::GreaterThanEq,
        TokenType::LessThanEq => Operator::LessThanEq,
        TokenType::Equal => Operator::Equal,
        TokenType::NotEqual => Operator::NotEqual,
        TokenType::Plus => Operator::Plus,
        TokenType::Minus => Operator::Minus,
        TokenType::Asterisk => Operator::Asterisk,
        TokenType::Slash => Operator::Slash,
        _ => panic!("Not an operator"),
    };
    Some(op)
}

fn get_token_string<'a>(source: &'a str, current_pos: usize, len: usize) -> &'a str {
    let data = &source[current_pos..current_pos + len];
    data
}

#[derive(Debug, PartialEq)]
pub enum ASTToken<'a> {
    Number(NumberType),
    String(&'a str),
    Boolean(bool),
    Identifier(&'a str),
    Lambda {
        name: &'a str,
        vars: Vec<&'a str>,
        body: Box<ASTToken<'a>>,
    },
    FnCall {
        func: Box<ASTToken<'a>>,
        args: Box<ASTToken<'a>>,
    },
    If {
        cond: Box<ASTToken<'a>>,
        then: Box<ASTToken<'a>>,
        els: Option<Box<ASTToken<'a>>>,
    },
    Unary {
        operator: Operator,
        right: Box<ASTToken<'a>>,
    },
    Binary {
        operator: Operator,
        left: Box<ASTToken<'a>>,
        right: Box<ASTToken<'a>>,
    },
    Grouping(Box<ASTToken<'a>>),
    Ignore(TokenType),
}

pub struct Parser<'a, I: Iterator<Item = Token>> {
    source: &'a str,
    tokens: Peekable<I>,
    current_pos: usize,
}

impl<'a, I: Iterator<Item = Token>> Parser<'a, I> {
    pub fn new(source: &'a str, tokens: I) -> Self {
        Self {
            source,
            tokens: tokens.peekable(),
            current_pos: 0,
        }
    }

    fn next(&mut self) -> Option<Token> {
        if let Some(token) = self.tokens.next() {
            self.current_pos += token.len;
            return Some(token);
        }
        None
    }

    fn peek(&mut self) -> Option<&Token> {
        while self.tokens.peek().is_some() {
            if matches!(
                self.tokens.peek().unwrap().ttype,
                TokenType::Whitespace | TokenType::Semicolon | TokenType::LineBreak
            ) {
                self.next();
            } else {
                break;
            }
        }
        self.tokens.peek()
    }

    fn parse_while<P>(&mut self, predicate: P)
    where
        P: Fn(&TokenType) -> bool,
    {
        while match self.tokens.peek() {
            Some(t) => predicate(&t.ttype),
            None => false,
        } {
            self.tokens.next();
        }
    }

    fn peek_operator<F>(&mut self, filter_fn: F) -> Option<Operator>
    where
        F: FnOnce(&&Token) -> bool,
    {
        if self.peek().filter(filter_fn).is_none() {
            return None;
        }

        let op = self.peek().unwrap();
        convert_operator(op.ttype)
    }

    fn take_operator(&mut self) -> Option<Operator> {
        if self.peek().is_none() {
            return None;
        }

        let op = self.next().unwrap();
        convert_operator(op.ttype)
    }

    fn primary(&mut self) -> Option<ASTToken<'a>> {
        let source = self.source;
        let current_pos = self.current_pos;
        let token = match self.next() {
            Some(Token {
                ttype: TokenType::Digit,
                len,
            }) => {
                let literal = get_token_string(source, current_pos, len);
                if literal.contains('.') {
                    let float = literal.parse::<f64>().unwrap();
                    ASTToken::Number(NumberType::Float(float))
                } else {
                    let int = literal.parse::<i64>().unwrap();
                    ASTToken::Number(NumberType::Int(int))
                }
            }
            Some(Token {
                ttype: TokenType::Literal,
                len,
            }) => match get_token_string(source, current_pos, len) {
                "true" => ASTToken::Boolean(true),
                "false" => ASTToken::Boolean(false),
                value => ASTToken::String(value),
            },
            Some(Token {
                ttype: TokenType::Identifier,
                len,
            }) => ASTToken::Identifier(get_token_string(source, current_pos, len)),
            Some(Token {
                ttype: TokenType::LeftParen,
                len: _,
            }) => match self.expression() {
                Some(expr) => {
                    self.parse_while(|t| t == &TokenType::RightParen);
                    ASTToken::Grouping(Box::new(expr))
                }
                None => return None,
            },
            Some(Token {
                ttype: rest,
                len: _,
            }) => ASTToken::Ignore(rest),
            None => return None,
        };
        Some(token)
    }

    fn unary(&mut self) -> Option<ASTToken<'a>> {
        if self
            .peek_operator(|token| matches!(token.ttype, TokenType::Not | TokenType::Minus))
            .is_some()
        {
            let operator = self.take_operator().unwrap();
            return self.unary().and_then(|right| {
                Some(ASTToken::Unary {
                    operator,
                    right: Box::new(right),
                })
            });
        }
        self.primary()
    }

    fn factor(&mut self) -> Option<ASTToken<'a>> {
        self.unary().and_then(|expr| {
            if self
                .peek_operator(|token| {
                    matches!(token.ttype, TokenType::Slash | TokenType::Asterisk)
                })
                .is_some()
            {
                let operator = self.take_operator().unwrap();
                match self.unary() {
                    Some(right) => Some(ASTToken::Binary {
                        operator,
                        left: Box::new(expr),
                        right: Box::new(right),
                    }),
                    _ => Some(expr),
                }
            } else {
                Some(expr)
            }
        })
    }

    fn term(&mut self) -> Option<ASTToken<'a>> {
        self.factor().and_then(|expr| {
            if self
                .peek_operator(|token| matches!(token.ttype, TokenType::Minus | TokenType::Plus))
                .is_some()
            {
                let operator = self.take_operator().unwrap();
                match self.factor() {
                    Some(right) => Some(ASTToken::Binary {
                        operator,
                        left: Box::new(expr),
                        right: Box::new(right),
                    }),
                    _ => Some(expr),
                }
            } else {
                Some(expr)
            }
        })
    }

    fn comparison(&mut self) -> Option<ASTToken<'a>> {
        self.term().and_then(|expr| {
            if self
                .peek_operator(|token| {
                    matches!(
                        token.ttype,
                        TokenType::GreaterThan
                            | TokenType::GreaterThanEq
                            | TokenType::LessThan
                            | TokenType::LessThanEq
                    )
                })
                .is_some()
            {
                let operator = self.take_operator().unwrap();
                match self.term() {
                    Some(right) => Some(ASTToken::Binary {
                        operator,
                        left: Box::new(expr),
                        right: Box::new(right),
                    }),
                    _ => Some(expr),
                }
            } else {
                Some(expr)
            }
        })
    }

    fn equality(&mut self) -> Option<ASTToken<'a>> {
        self.comparison().and_then(|expr| {
            if self
                .peek_operator(|token| {
                    matches!(token.ttype, TokenType::Equal | TokenType::NotEqual)
                })
                .is_some()
            {
                let operator = self.take_operator().unwrap();
                match self.comparison() {
                    Some(right) => Some(ASTToken::Binary {
                        operator,
                        left: Box::new(expr),
                        right: Box::new(right),
                    }),
                    _ => Some(expr),
                }
            } else {
                Some(expr)
            }
        })
    }

    fn expression(&mut self) -> Option<ASTToken<'a>> {
        self.equality()
    }

    pub fn parse(&'a mut self) -> impl Iterator<Item = ASTToken> + 'a {
        iter::from_fn(|| {
            if self.tokens.peek().is_some() {
                let expr = self.expression();
                expr
            } else {
                None
            }
        })
        .filter(|token| match token {
            ASTToken::Ignore(_) => false,
            _ => true,
        })
    }
}
