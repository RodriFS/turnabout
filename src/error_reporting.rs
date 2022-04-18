use crate::lexer::{Token, TokenType};
use crate::utils::FoldIfNoneExt;

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

pub fn report_lexer_errors<'a>(
    source: &'a str,
    tokens:Vec<Token>,
) -> bool {
    tokens.iter().fold(false, |_, token: &Token| match token.ttype {
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
