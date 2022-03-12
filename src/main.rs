use turnabout::lexer::{Cursor, Token};
use turnabout::error_reporting::report_lexer_errors;
use std::io;

fn main() -> Result<(), std::io::Error> {
    loop {
        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer)?;
        let mut cursor = Cursor::new(&buffer);
        let tokens = cursor.read();
        let tokens = report_lexer_errors(&buffer, tokens);
        println!("{:?}", tokens.collect::<Vec<Token>>());
    }
}
