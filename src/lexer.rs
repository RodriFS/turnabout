use std::iter::{self, Peekable};
use std::str::Chars;
use self::TokenType::*;

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
  pub ttype: TokenType,
  pub len: usize,
}

impl Token {
  fn new(ttype: TokenType, len: usize) -> Self {
    Self {
      ttype,
      len
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Whitespace,
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
    Unexpected,
}

pub struct Cursor<'a> {
    input: Peekable<Chars<'a>>,
    pos: usize,
}

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.chars().peekable(),
            pos: 0,
        }
    }

    fn next(&mut self) -> Option<char> {
        let next = self.input.next();
        if let Some(c)  = next {
          self.pos += c.len_utf8();
        }
        next
    }

    fn peek(&mut self) -> Option<&char> {
        self.input.peek()
    }

    fn parse_while<P>(&mut self, predicate: P) -> usize
    where
        P: Fn(&char) -> bool,
    {
        let mut start = 1;
        while match self.peek() {
            Some(c) => predicate(c),
            None => false,
        } {
            let next = self.next();
            if let Some(c) = next {
              start += c.len_utf8();
            }
        }
        start
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
        self.parse_while(|c| c.is_digit(10) || c == &'.');
        Digit
    }

    fn parse_literal(&mut self) -> TokenType {
        self.parse_while(|c| c != &'"');
        self.next();
        // TODO: Report non terminated string literal
        Literal
    }

    fn parse_line_comment(&mut self) -> TokenType {
        self.parse_while(|c| c != &'\n');
        LineComment
    }

    fn consume(&mut self, token: TokenType) -> TokenType {
        self.next();
        token
    }

    pub fn read(&'a mut self) -> impl Iterator<Item = Token> + 'a {
        iter::from_fn(|| {
            let prev_pos = self.pos;
            if let Some(c) = self.next() {
                let token= match c {
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
                    '/' => match self.peek() {
                        Some('/') => self.parse_line_comment(),
                        _ => Slash
                    }
                    '!' => match self.peek() {
                        Some('=') => self.consume(NotEqual),
                        _ => Not
                    },
                    '<' => match self.peek() {
                        Some('=') => self.consume(LessThanEq),
                        _ => LessThan
                    },
                    '=' => match self.peek() {
                        Some('=') => self.consume(Equal),
                        _ => Assignment
                    },
                    '>' => match self.peek() {
                        Some('=') => self.consume(GreaterThanEq),
                        _ => GreaterThan
                    },
                    '_' => match self.peek() {
                        Some(c) if c.is_whitespace() => Underscore,
                        Some(c) if c.is_alphabetic() => self.parse_identifier(),
                        Some(c) if c.is_digit(10) => self.parse_identifier(),
                        _ => Underscore
                    },
                    _ => Unexpected,
                };
                Some(Token::new(token, self.pos - prev_pos))
            } else {
                None
            }
        })
    }
}

#[test]
fn test_read() {
    #[allow(unused_assignments)]
    let mut tokens: Vec<Token> = vec![];

    tokens = Cursor::new("     ").read().collect();
    assert_eq!(tokens, vec![Token::new(Whitespace, 5)]);

    tokens = Cursor::new("abcde").read().collect();
    assert_eq!(tokens, vec![Token::new(Identifier,5)]);

    tokens = Cursor::new("12345").read().collect();
    assert_eq!(tokens, vec![Token::new(Digit, 5)]);

    tokens = Cursor::new("\"hello world\"").read().collect();
    assert_eq!(tokens, vec![Token::new(Literal, 13)]);

    tokens = Cursor::new("(").read().collect();
    assert_eq!(tokens, vec![Token::new(LeftParen, 1)]);

    tokens = Cursor::new(")").read().collect();
    assert_eq!(tokens, vec![Token::new(RightParen, 1)]);

    tokens = Cursor::new("*").read().collect();
    assert_eq!(tokens, vec![Token::new(Asterisk, 1)]);

    tokens = Cursor::new("+").read().collect();
    assert_eq!(tokens, vec![Token::new(Plus, 1)]);

    tokens = Cursor::new(",").read().collect();
    assert_eq!(tokens, vec![Token::new(Comma, 1)]);

    tokens = Cursor::new("-").read().collect();
    assert_eq!(tokens, vec![Token::new(Minus, 1)]);

    tokens = Cursor::new(";").read().collect();
    assert_eq!(tokens, vec![Token::new(Semicolon, 1)]);

    tokens = Cursor::new("[").read().collect();
    assert_eq!(tokens, vec![Token::new(LeftSqBracket, 1)]);

    tokens = Cursor::new("]").read().collect();
    assert_eq!(tokens, vec![Token::new(RightSqBracket, 1)]);

    tokens = Cursor::new("^").read().collect();
    assert_eq!(tokens, vec![Token::new(CircAccent, 1)]);

    tokens = Cursor::new("{").read().collect();
    assert_eq!(tokens, vec![Token::new(LeftCuBracket, 1)]);

    tokens = Cursor::new("}").read().collect();
    assert_eq!(tokens, vec![Token::new(RightCuBracket, 1)]);

    tokens = Cursor::new("/").read().collect();
    assert_eq!(tokens, vec![Token::new(Slash, 1)]);

    tokens = Cursor::new("// comment").read().collect();
    assert_eq!(tokens, vec![Token::new(LineComment, 10)]);

    tokens = Cursor::new("!").read().collect();
    assert_eq!(tokens, vec![Token::new(Not, 1)]);

    tokens = Cursor::new("!=").read().collect();
    assert_eq!(tokens, vec![Token::new(NotEqual, 2)]);

    tokens = Cursor::new("<").read().collect();
    assert_eq!(tokens, vec![Token::new(LessThan, 1)]);

    tokens = Cursor::new("<=").read().collect();
    assert_eq!(tokens, vec![Token::new(LessThanEq, 2)]);

    tokens = Cursor::new("=").read().collect();
    assert_eq!(tokens, vec![Token::new(Assignment, 1)]);

    tokens = Cursor::new("==").read().collect();
    assert_eq!(tokens, vec![Token::new(Equal, 2)]);

    tokens = Cursor::new(">").read().collect();
    assert_eq!(tokens, vec![Token::new(GreaterThan, 1)]);

    tokens = Cursor::new(">=").read().collect();
    assert_eq!(tokens, vec![Token::new(GreaterThanEq, 2)]);

    tokens = Cursor::new("_").read().collect();
    assert_eq!(tokens, vec![Token::new(Underscore, 1)]);

    tokens = Cursor::new("_abcde").read().collect();
    assert_eq!(tokens, vec![Token::new(Identifier, 6)]);

    tokens = Cursor::new("_12345").read().collect();
    assert_eq!(tokens, vec![Token::new(Identifier, 6)]);

    tokens = Cursor::new("&").read().collect();
    assert_eq!(tokens, vec![Token::new(Unexpected, 1)]);

    tokens = Cursor::new(
r###"   abcde 12345 "hello world" ( ) * + , - ; [ ] ^
      { } / ! != < <= = == > >= _ _abcde _12345 & // hello"###
    ).read().collect();
    assert_eq!(tokens, vec![
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
      Token::new(Whitespace, 7),
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
      Token::new(Unexpected, 1),
      Token::new(Whitespace, 1),
      Token::new(LineComment, 8)
    ]);
}
