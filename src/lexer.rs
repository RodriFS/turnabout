use crate::{cursor::Cursor, utils::LiteralKind};

use self::TokenType::*;
use std::iter;

#[derive(Debug, PartialEq)]
pub struct Token {
    pub ttype: TokenType,
}

impl Token {
    pub fn new(ttype: TokenType) -> Self {
        Self { ttype }
    }
}

#[derive(Debug, PartialEq)]
pub enum TokenType {
    Whitespace,
    LineBreak,
    Identifier,
    Literal {
        kind: LiteralKind,
    },
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

    fn get_symbol<F>(&mut self, predicate_fn: F) -> &'a str
    where
        F: Fn(&char) -> bool,
    {
        let from = self.cursor.get_column() - 1;
        self.parse_while(predicate_fn);
        let to = self.cursor.get_column();
        self.cursor.get_in_range(from..to)
    }

    fn parse_identifier(&mut self) -> TokenType {
        let literal = self.get_symbol(|c| c.is_alphanumeric());
        match literal {
            "true" => Literal {
                kind: LiteralKind::Bool(true),
            },
            "false" => Literal {
                kind: LiteralKind::Bool(false),
            },
            // TODO separate identifiers from keywords and store them in the enum
            v => {
                dbg!(v);
                Identifier
            }
        }
    }

    fn parse_digit(&mut self) -> TokenType {
        let literal = self.get_symbol(|c| c.is_digit(10) || *c == '.');
        match literal {
            f if f.contains('.') => Literal {
                kind: LiteralKind::Float(f.parse::<f64>().unwrap()),
            },
            int => Literal {
                kind: LiteralKind::Int(int.parse::<i64>().unwrap()),
            },
        }
    }

    fn parse_literal(&mut self) -> TokenType {
        let column_nr = self.cursor.get_column();
        let literal = self.get_symbol(|c| *c != '"' && *c != '\n');
        self.cursor.next();
        dbg!(literal, self.cursor.get_last_char());
        match self.cursor.get_last_char() {
            Some('"') => Literal {
                kind: LiteralKind::Str(literal[1..].to_string()),
            },
            Some('\n') => UnterminatedLiteral {
                line_nr: self.cursor.get_line_number() - 1,
                col: column_nr,
            },
            _ => UnterminatedLiteral {
                line_nr: self.cursor.get_line_number(),
                col: column_nr,
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
                Some(Token::new(token))
            } else {
                None
            }
        })
        .filter(|t| {
            !matches!(
                t.ttype,
                TokenType::Whitespace | TokenType::LineBreak | TokenType::Semicolon
            )
        })
        .collect()
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::*;

    fn read(string: &str) -> Vec<Token> {
        Lexer::new(Cursor::new(string)).read()
    }

    macro_rules! mk_tokens {
        ($($x: expr),*) => {{
            let mut vector = Vec::new();
            $(vector.push(Token::new($x));)*
            vector
        }}
    }

    #[test]
    fn test_read() {
        #[allow(unused_assignments)]
        let mut tokens: Vec<Token> = vec![];

        tokens = read("     ");
        assert_eq!(tokens, vec![]);

        tokens = read("abcde");
        assert_eq!(tokens, mk_tokens![Identifier]);

        tokens = read("12345");
        assert_eq!(
            tokens,
            mk_tokens![Literal {
                kind: LiteralKind::Int(12345)
            }]
        );

        tokens = read(r#""hello world""#);
        assert_eq!(
            tokens,
            mk_tokens![Literal {
                kind: LiteralKind::Str("hello world".to_string())
            }]
        );

        tokens = read(r#""hello world"#);
        assert_eq!(
            tokens,
            mk_tokens![UnterminatedLiteral { line_nr: 1, col: 1 }]
        );

        tokens = read("(");
        assert_eq!(tokens, mk_tokens![LeftParen]);

        tokens = read(")");
        assert_eq!(tokens, mk_tokens![RightParen]);

        tokens = read("*");
        assert_eq!(tokens, mk_tokens![Asterisk]);

        tokens = read("+");
        assert_eq!(tokens, mk_tokens![Plus]);

        tokens = read(",");
        assert_eq!(tokens, mk_tokens![Comma]);

        tokens = read("-");
        assert_eq!(tokens, mk_tokens![Minus]);

        tokens = read(";");
        assert_eq!(tokens, vec![]);

        tokens = read("[");
        assert_eq!(tokens, mk_tokens![LeftSqBracket]);

        tokens = read("]");
        assert_eq!(tokens, mk_tokens![RightSqBracket]);

        tokens = read("^");
        assert_eq!(tokens, mk_tokens![CircAccent]);

        tokens = read("{");
        assert_eq!(tokens, mk_tokens![LeftCuBracket]);

        tokens = read("}");
        assert_eq!(tokens, mk_tokens![RightCuBracket]);

        tokens = read("/");
        assert_eq!(tokens, mk_tokens![Slash]);

        tokens = read("// comment");
        assert_eq!(tokens, mk_tokens![LineComment]);

        tokens = read("!");
        assert_eq!(tokens, mk_tokens![Not]);

        tokens = read("!=");
        assert_eq!(tokens, mk_tokens![NotEqual]);

        tokens = read("<");
        assert_eq!(tokens, mk_tokens![LessThan]);

        tokens = read("<=");
        assert_eq!(tokens, mk_tokens![LessThanEq]);

        tokens = read("=");
        assert_eq!(tokens, mk_tokens![Assignment]);

        tokens = read("==");
        assert_eq!(tokens, mk_tokens![Equal]);

        tokens = read(">");
        assert_eq!(tokens, mk_tokens![GreaterThan]);

        tokens = read(">=");
        assert_eq!(tokens, mk_tokens![GreaterThanEq]);

        tokens = read("_");
        assert_eq!(tokens, mk_tokens![Underscore]);

        tokens = read("_abcde");
        assert_eq!(tokens, mk_tokens![Identifier]);

        tokens = read("_12345");
        assert_eq!(tokens, mk_tokens![Identifier]);

        tokens = read("&");
        assert_eq!(
            tokens,
            mk_tokens![Unexpected {
                line_nr: 1,
                col: 1,
                symbol: '&'
            }]
        );

        tokens = read(
            r###"   abcde 12345 "hello world" ( ) * + , - ; [ ] ^
      { } / ! != < <= = == > >= _ _abcde _12345 & // hello"###,
        );
        assert_eq!(
            tokens,
            mk_tokens![
                Identifier,
                Literal {
                    kind: LiteralKind::Int(12345)
                },
                Literal {
                    kind: LiteralKind::Str("hello world".to_string())
                },
                LeftParen,
                RightParen,
                Asterisk,
                Plus,
                Comma,
                Minus,
                LeftSqBracket,
                RightSqBracket,
                CircAccent,
                LeftCuBracket,
                RightCuBracket,
                Slash,
                Not,
                NotEqual,
                LessThan,
                LessThanEq,
                Assignment,
                Equal,
                GreaterThan,
                GreaterThanEq,
                Underscore,
                Identifier,
                Identifier,
                Unexpected {
                    line_nr: 2,
                    col: 49,
                    symbol: '&'
                },
                LineComment
            ]
        );

        tokens = read(r###""hello" "world "###);
        assert_eq!(
            tokens,
            mk_tokens![
                Literal {
                    kind: LiteralKind::Str("hello".to_string())
                },
                UnterminatedLiteral { line_nr: 1, col: 9 }
            ]
        );

        tokens = read("\"hello\n");
        assert_eq!(
            tokens,
            mk_tokens![UnterminatedLiteral { line_nr: 1, col: 1 }]
        );

        tokens = read(r###""hello"###);
        assert_eq!(
            tokens,
            mk_tokens![UnterminatedLiteral { line_nr: 1, col: 1 }]
        );

        tokens = read("!true");
        assert_eq!(
            tokens,
            mk_tokens![
                Not,
                Literal {
                    kind: LiteralKind::Bool(true)
                }
            ]
        )
    }
}
