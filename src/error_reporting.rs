use crate::lexer::{Token, TokenType};
use crate::utils::LooseAnyExt;

pub fn print_symbol_error(symbol: &char, line_nr: &usize, col: &usize, line: &str) {
    println!(
        "\x1b[31mUnexpected character '{}' at {}:{}\x1b[0m",
        symbol, line_nr, col
    );
    println!("{} | {}\n", line_nr, line);
}

pub fn print_unterminated_literal_error(line_nr: &usize, col: &usize, line: &str) {
    println!("\x1b[31mUnterminated literal at {}:{}\x1b[0m", line_nr, col);
    println!("{} | {}\n", line_nr, line);
}

pub fn report_lexer_errors<'a>(source: &'a str, tokens: Vec<Token>) -> bool {
    tokens.iter().loose_any(|token: &Token| match token.ttype {
        TokenType::Unexpected {
            line_nr,
            col,
            symbol,
        } => {
            let line = source
                .lines()
                .skip(line_nr - 1)
                .next()
                .unwrap_or("Unexpected Error");
            print_symbol_error(&symbol, &line_nr, &col, line);
            true
        }
        TokenType::UnterminatedLiteral { line_nr, col } => {
            let line = source
                .lines()
                .skip(line_nr - 1)
                .next()
                .unwrap_or("Unexpected Error");
            print_unterminated_literal_error(&line_nr, &col, line);
            true
        }
        _ => false,
    })
}

#[test]
fn empty_source() {
    let errors = report_lexer_errors("", vec![]);
    assert!(!errors);
}

#[test]
fn correct_source() {
    let source = "let myvar = 4;";
    let tokens = vec![
        Token::new(TokenType::Identifier, 3),
        Token::new(TokenType::Whitespace, 1),
        Token::new(TokenType::Identifier, 5),
        Token::new(TokenType::Whitespace, 1),
        Token::new(TokenType::Assignment, 1),
        Token::new(TokenType::Whitespace, 1),
        Token::new(TokenType::Digit, 1),
        Token::new(TokenType::Semicolon, 1),
    ];
    let errors = report_lexer_errors(source, tokens);
    assert!(!errors);
}

#[test]
fn incorrect_source_unexpected_character() {
    let source = "let @";
    let tokens = vec![
        Token::new(TokenType::Identifier, 3),
        Token::new(TokenType::Whitespace, 1),
        Token::new(
            TokenType::Unexpected {
                line_nr: 1,
                col: 5,
                symbol: '@',
            },
            1,
        ),
    ];
    let errors = report_lexer_errors(source, tokens);
    assert!(errors);
}

#[test]
fn incorrect_source_unterminated_literal() {
    let source = "\"hello";
    let tokens = vec![Token::new(
        TokenType::UnterminatedLiteral { line_nr: 1, col: 6 },
        6,
    )];
    let errors = report_lexer_errors(source, tokens);
    assert!(errors);
}
