use crate::lexer::{Token, TokenType};
use std::iter::{self};

#[derive(Debug, PartialEq)]
pub enum NumberType {
    Float(f64),
    Int(i64),
}

#[derive(Debug, PartialEq)]
pub enum KeywordType {
    If,
    Else,
    Let,
    Fn,
    True,
    False
}

#[derive(Debug, PartialEq)]
pub enum ASTToken<'a> {
    Operator(TokenType),
    Punctuation(TokenType),
    Number(NumberType),
    String(&'a str),
    Keyword(KeywordType),
    Identifier(&'a str),
    Ignore(TokenType)
}

pub struct Parser<'a, I: Iterator<Item = Token>> {
    source: &'a str,
    tokens: I,
    curren_pos: usize
}

impl<'a, I: Iterator<Item = Token>> Parser<'a, I> {
    pub fn new(source: &'a str, tokens: I) -> Self {
        Self { source, tokens, curren_pos: 0 }
    }


    fn get_token_string(&self, len: usize) -> &'a str {
        &self.source[self.curren_pos..self.curren_pos + len]
    }

    fn parse_digit(&self, token: Token) -> Option<ASTToken<'a>> {
        let number = self.get_token_string(token.len);
        if number.contains(".") {
            if let Ok(parsed_number) = number.parse::<f64>() {
                return Some(ASTToken::Number(NumberType::Float(parsed_number)))
            }
        } else {
            if let Ok(parsed_number) = number.parse::<i64>() {
                return Some(ASTToken::Number(NumberType::Int(parsed_number)))
            }
        }
        None
    }

    fn parse_literal(&self, token: Token) -> Option<ASTToken<'a>> {
        return Some(ASTToken::String(self.get_token_string(token.len)))
    }

    fn parse_identifier(&self, token: Token) -> Option<ASTToken<'a>> {
        let value = self.get_token_string(token.len);
        match value {
            "if" => Some(ASTToken::Keyword(KeywordType::If)),
            "else" => Some(ASTToken::Keyword(KeywordType::Else)),
            "let" => Some(ASTToken::Keyword(KeywordType::Let)),
            "fn" => Some(ASTToken::Keyword(KeywordType::Fn)),
            "true" => Some(ASTToken::Keyword(KeywordType::True)),
            "false" => Some(ASTToken::Keyword(KeywordType::False)),
            _ => Some(ASTToken::Identifier(value))
        }
    }

    pub fn parse(&'a mut self) -> impl Iterator<Item = ASTToken> + 'a {
        iter::from_fn(|| {
            if let Some(token) = self.tokens.next() {
                let ast_token = match token.ttype {
                    // Operators
                    TokenType::Not => Some(ASTToken::Operator(TokenType::Not)),
                    TokenType::Plus => Some(ASTToken::Operator(TokenType::Plus)),
                    TokenType::Minus => Some(ASTToken::Operator(TokenType::Minus)),
                    TokenType::Slash => Some(ASTToken::Operator(TokenType::Slash)),
                    TokenType::Equal => Some(ASTToken::Operator(TokenType::Equal)),
                    TokenType::NotEqual => Some(ASTToken::Operator(TokenType::NotEqual)),
                    TokenType::Asterisk => Some(ASTToken::Operator(TokenType::Asterisk)),
                    TokenType::LessThan => Some(ASTToken::Operator(TokenType::LessThan)),
                    TokenType::LessThanEq => Some(ASTToken::Operator(TokenType::LessThanEq)),
                    TokenType::Assignment => Some(ASTToken::Operator(TokenType::Assignment)),
                    TokenType::CircAccent => Some(ASTToken::Operator(TokenType::CircAccent)),
                    TokenType::GreaterThan => Some(ASTToken::Operator(TokenType::GreaterThan)),
                    TokenType::GreaterThanEq => Some(ASTToken::Operator(TokenType::GreaterThanEq)),
                    // Punctuation
                    TokenType::Comma => Some(ASTToken::Punctuation(TokenType::Comma)),
                    TokenType::Semicolon => Some(ASTToken::Punctuation(TokenType::Semicolon)),
                    TokenType::LeftParen => Some(ASTToken::Punctuation(TokenType::LeftParen)),
                    TokenType::RightParen => Some(ASTToken::Punctuation(TokenType::RightParen)),
                    TokenType::LeftSqBracket => {
                        Some(ASTToken::Punctuation(TokenType::LeftSqBracket))
                    }
                    TokenType::LeftCuBracket => {
                        Some(ASTToken::Punctuation(TokenType::LeftCuBracket))
                    }
                    TokenType::RightSqBracket => {
                        Some(ASTToken::Punctuation(TokenType::RightSqBracket))
                    }
                    TokenType::RightCuBracket => {
                        Some(ASTToken::Punctuation(TokenType::RightCuBracket))
                    },
                    // Number
                    TokenType::Digit => self.parse_digit(token),
                    // String
                    TokenType::Literal => self.parse_literal(token),
                    // Keyword & Identifier
                    TokenType::Identifier => self.parse_identifier(token),
                    _ => Some(ASTToken::Ignore(token.ttype))
                };
                self.curren_pos += token.len;
                ast_token
            } else {
                None
            }
        }).filter(|token| match token {
            ASTToken::Ignore(_) => false,
            _ => true
        })
    }
}
