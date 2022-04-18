use crate::cursor::Cursor;

use self::TokenType::*;
use std::iter::{self};

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Token {
    pub ttype: TokenType,
    pub len: usize,
}

impl Token {
    fn new(ttype: TokenType, len: usize) -> Self {
        Self { ttype, len }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    Whitespace,
    LineBreak,
    Identifier,
    Digit,
    Literal,
    LineComment,
    NotEqual,
    Not,
    LeftParen,
    RightParen,
    Asterisk,
    Plus,
    Comma,
    Minus,
    Slash,
    Semicolon,
    LessThanEq,
    LessThan,
    Equal,
    Assignment,
    GreaterThan,
    GreaterThanEq,
    LeftSqBracket,
    RightSqBracket,
    CircAccent,
    Underscore,
    LeftCuBracket,
    RightCuBracket,
    Unexpected {
        line_nr: usize,
        col: usize,
        symbol: char,
    },
    UnterminatedLiteral {
        line_nr: usize,
        col: usize,
    },
}

pub struct Lexer<'a> {
    cursor: Cursor<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(cursor: Cursor<'a>) -> Self {
        Self { cursor }
    }

    fn parse_while<P>(&mut self, predicate: P)
    where
        P: Fn(&char) -> bool,
    {
        while match self.cursor.peek() {
            Some(c) => predicate(c),
            None => false,
        } {
            self.cursor.next();
        }
    }

    fn parse_whitespace(&mut self) -> TokenType {
        self.parse_while(|c| c.is_whitespace());
        Whitespace
    }

    fn parse_identifier(&mut self) -> TokenType {
        self.parse_while(|c| c.is_alphanumeric());
        Identifier
    }

    fn parse_digit(&mut self) -> TokenType {
        self.parse_while(|c| c.is_digit(10) || *c == '.');
        Digit
    }

    fn parse_literal(&mut self) -> TokenType {
        let col = self.cursor.get_column();
        self.parse_while(|c| *c != '"' && *c != '\n');
        self.cursor.next();
        match self.cursor.get_last_char() {
            Some('"') => Literal,
            Some('\n') => UnterminatedLiteral {
                line_nr: self.cursor.get_line_number() - 1,
                col,
            },
            _ => UnterminatedLiteral {
                line_nr: self.cursor.get_line_number(),
                col,
            },
        }
    }

    fn parse_line_comment(&mut self) -> TokenType {
        self.parse_while(|c| *c != '\n');
        LineComment
    }

    fn consume(&mut self, token: TokenType) -> TokenType {
        self.cursor.next();
        token
    }

    pub fn read(&'a mut self) -> Vec<Token> {
        iter::from_fn(|| {
            if let Some(c) = self.cursor.next() {
                let token = match c {
                    '\n' => LineBreak,
                    c if c.is_whitespace() => self.parse_whitespace(),
                    c if c.is_alphabetic() => self.parse_identifier(),
                    c if c.is_digit(10) => self.parse_digit(),
                    '"' => self.parse_literal(),
                    '(' => LeftParen,
                    ')' => RightParen,
                    '*' => Asterisk,
                    '+' => Plus,
                    ',' => Comma,
                    '-' => Minus,
                    ';' => Semicolon,
                    '[' => LeftSqBracket,
                    ']' => RightSqBracket,
                    '^' => CircAccent,
                    '{' => LeftCuBracket,
                    '}' => RightCuBracket,
                    '/' => match self.cursor.peek() {
                        Some('/') => self.parse_line_comment(),
                        _ => Slash,
                    },
                    '!' => match self.cursor.peek() {
                        Some('=') => {
                            self.cursor.next();
                            NotEqual
                        }
                        _ => Not,
                    },
                    '<' => match self.cursor.peek() {
                        Some('=') => self.consume(LessThanEq),
                        _ => LessThan,
                    },
                    '=' => match self.cursor.peek() {
                        Some('=') => self.consume(Equal),
                        _ => Assignment,
                    },
                    '>' => match self.cursor.peek() {
                        Some('=') => self.consume(GreaterThanEq),
                        _ => GreaterThan,
                    },
                    '_' => match self.cursor.peek() {
                        Some(c) if c.is_whitespace() => Underscore,
                        Some(c) if c.is_alphabetic() => self.parse_identifier(),
                        Some(c) if c.is_digit(10) => self.parse_identifier(),
                        _ => Underscore,
                    },
                    _ => Unexpected {
                        symbol: c,
                        line_nr: self.cursor.get_line_number(),
                        col: self.cursor.get_column(),
                    },
                };
                Some(Token::new(token, self.cursor.get_len_consumed()))
            } else {
                None
            }
        })
        .collect()
    }
}

#[test]
fn test_read() {
    #[allow(unused_assignments)]
    let mut tokens: Vec<Token> = vec![];

    tokens = Lexer::new(Cursor::new("     ")).read();
    assert_eq!(tokens, vec![Token::new(Whitespace, 5)]);

    tokens = Lexer::new(Cursor::new("abcde")).read();
    assert_eq!(tokens, vec![Token::new(Identifier, 5)]);

    tokens = Lexer::new(Cursor::new("12345")).read();
    assert_eq!(tokens, vec![Token::new(Digit, 5)]);

    tokens = Lexer::new(Cursor::new(r#""hello world""#)).read();
    assert_eq!(tokens, vec![Token::new(Literal, 13)]);

    tokens = Lexer::new(Cursor::new(r#""hello world"#)).read();
    assert_eq!(
        tokens,
        vec![Token::new(UnterminatedLiteral { line_nr: 1, col: 1 }, 12)]
    );

    tokens = Lexer::new(Cursor::new("(")).read();
    assert_eq!(tokens, vec![Token::new(LeftParen, 1)]);

    tokens = Lexer::new(Cursor::new(")")).read();
    assert_eq!(tokens, vec![Token::new(RightParen, 1)]);

    tokens = Lexer::new(Cursor::new("*")).read();
    assert_eq!(tokens, vec![Token::new(Asterisk, 1)]);

    tokens = Lexer::new(Cursor::new("+")).read();
    assert_eq!(tokens, vec![Token::new(Plus, 1)]);

    tokens = Lexer::new(Cursor::new(",")).read();
    assert_eq!(tokens, vec![Token::new(Comma, 1)]);

    tokens = Lexer::new(Cursor::new("-")).read();
    assert_eq!(tokens, vec![Token::new(Minus, 1)]);

    tokens = Lexer::new(Cursor::new(";")).read();
    assert_eq!(tokens, vec![Token::new(Semicolon, 1)]);

    tokens = Lexer::new(Cursor::new("[")).read();
    assert_eq!(tokens, vec![Token::new(LeftSqBracket, 1)]);

    tokens = Lexer::new(Cursor::new("]")).read();
    assert_eq!(tokens, vec![Token::new(RightSqBracket, 1)]);

    tokens = Lexer::new(Cursor::new("^")).read();
    assert_eq!(tokens, vec![Token::new(CircAccent, 1)]);

    tokens = Lexer::new(Cursor::new("{")).read();
    assert_eq!(tokens, vec![Token::new(LeftCuBracket, 1)]);

    tokens = Lexer::new(Cursor::new("}")).read();
    assert_eq!(tokens, vec![Token::new(RightCuBracket, 1)]);

    tokens = Lexer::new(Cursor::new("/")).read();
    assert_eq!(tokens, vec![Token::new(Slash, 1)]);

    tokens = Lexer::new(Cursor::new("// comment")).read();
    assert_eq!(tokens, vec![Token::new(LineComment, 10)]);

    tokens = Lexer::new(Cursor::new("!")).read();
    assert_eq!(tokens, vec![Token::new(Not, 1)]);

    tokens = Lexer::new(Cursor::new("!=")).read();
    assert_eq!(tokens, vec![Token::new(NotEqual, 2)]);

    tokens = Lexer::new(Cursor::new("<")).read();
    assert_eq!(tokens, vec![Token::new(LessThan, 1)]);

    tokens = Lexer::new(Cursor::new("<=")).read();
    assert_eq!(tokens, vec![Token::new(LessThanEq, 2)]);

    tokens = Lexer::new(Cursor::new("=")).read();
    assert_eq!(tokens, vec![Token::new(Assignment, 1)]);

    tokens = Lexer::new(Cursor::new("==")).read();
    assert_eq!(tokens, vec![Token::new(Equal, 2)]);

    tokens = Lexer::new(Cursor::new(">")).read();
    assert_eq!(tokens, vec![Token::new(GreaterThan, 1)]);

    tokens = Lexer::new(Cursor::new(">=")).read();
    assert_eq!(tokens, vec![Token::new(GreaterThanEq, 2)]);

    tokens = Lexer::new(Cursor::new("_")).read();
    assert_eq!(tokens, vec![Token::new(Underscore, 1)]);

    tokens = Lexer::new(Cursor::new("_abcde")).read();
    assert_eq!(tokens, vec![Token::new(Identifier, 6)]);

    tokens = Lexer::new(Cursor::new("_12345")).read();
    assert_eq!(tokens, vec![Token::new(Identifier, 6)]);

    tokens = Lexer::new(Cursor::new("&")).read();
    assert_eq!(
        tokens,
        vec![Token::new(
            Unexpected {
                line_nr: 1,
                col: 1,
                symbol: '&'
            },
            1
        )]
    );

    tokens = Lexer::new(Cursor::new(
        r###"   abcde 12345 "hello world" ( ) * + , - ; [ ] ^
      { } / ! != < <= = == > >= _ _abcde _12345 & // hello"###,
    ))
    .read();
    assert_eq!(
        tokens,
        vec![
            Token::new(Whitespace, 3),
            Token::new(Identifier, 5),
            Token::new(Whitespace, 1),
            Token::new(Digit, 5),
            Token::new(Whitespace, 1),
            Token::new(Literal, 13),
            Token::new(Whitespace, 1),
            Token::new(LeftParen, 1),
            Token::new(Whitespace, 1),
            Token::new(RightParen, 1),
            Token::new(Whitespace, 1),
            Token::new(Asterisk, 1),
            Token::new(Whitespace, 1),
            Token::new(Plus, 1),
            Token::new(Whitespace, 1),
            Token::new(Comma, 1),
            Token::new(Whitespace, 1),
            Token::new(Minus, 1),
            Token::new(Whitespace, 1),
            Token::new(Semicolon, 1),
            Token::new(Whitespace, 1),
            Token::new(LeftSqBracket, 1),
            Token::new(Whitespace, 1),
            Token::new(RightSqBracket, 1),
            Token::new(Whitespace, 1),
            Token::new(CircAccent, 1),
            Token::new(LineBreak, 1),
            Token::new(Whitespace, 6),
            Token::new(LeftCuBracket, 1),
            Token::new(Whitespace, 1),
            Token::new(RightCuBracket, 1),
            Token::new(Whitespace, 1),
            Token::new(Slash, 1),
            Token::new(Whitespace, 1),
            Token::new(Not, 1),
            Token::new(Whitespace, 1),
            Token::new(NotEqual, 2),
            Token::new(Whitespace, 1),
            Token::new(LessThan, 1),
            Token::new(Whitespace, 1),
            Token::new(LessThanEq, 2),
            Token::new(Whitespace, 1),
            Token::new(Assignment, 1),
            Token::new(Whitespace, 1),
            Token::new(Equal, 2),
            Token::new(Whitespace, 1),
            Token::new(GreaterThan, 1),
            Token::new(Whitespace, 1),
            Token::new(GreaterThanEq, 2),
            Token::new(Whitespace, 1),
            Token::new(Underscore, 1),
            Token::new(Whitespace, 1),
            Token::new(Identifier, 6),
            Token::new(Whitespace, 1),
            Token::new(Identifier, 6),
            Token::new(Whitespace, 1),
            Token::new(
                Unexpected {
                    line_nr: 2,
                    col: 49,
                    symbol: '&'
                },
                1
            ),
            Token::new(Whitespace, 1),
            Token::new(LineComment, 8)
        ]
    );

    tokens = Lexer::new(Cursor::new(r###""hello" "world "###)).read();
    assert_eq!(
        tokens,
        vec![
            Token::new(Literal, 7),
            Token::new(Whitespace, 1),
            Token::new(UnterminatedLiteral { line_nr: 1, col: 9 }, 7),
        ]
    );

    tokens = Lexer::new(Cursor::new("\"hello\n")).read();
    assert_eq!(
        tokens,
        vec![Token::new(UnterminatedLiteral { line_nr: 1, col: 1 }, 7)]
    );

    tokens = Lexer::new(Cursor::new(r###""hello"###)).read();
    assert_eq!(
        tokens,
        vec![Token::new(UnterminatedLiteral { line_nr: 1, col: 1 }, 6)]
    );
}
