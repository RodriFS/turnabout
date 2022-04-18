use std::fs;
use std::io;
use std::str;
use turnabout::cursor::Cursor;
use turnabout::error_reporting::report_lexer_errors;
use turnabout::lexer::Lexer;
use turnabout::parser::{ASTToken, Parser};

#[derive(Debug)]
enum Error {
    IOError(io::Error),
    Utf8Error(str::Utf8Error),
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Error {
        Error::IOError(e)
    }
}

impl From<str::Utf8Error> for Error {
    fn from(e: str::Utf8Error) -> Error {
        Error::Utf8Error(e)
    }
}

fn read(buffer: &str) {
    let cursor = Cursor::new(&buffer);
    let mut lexer = Lexer::new(cursor);
    let tokens = lexer.read();

    let lexer_errors = report_lexer_errors(&buffer, tokens.clone());
    if lexer_errors {
        return;
    }
    let mut parser = Parser::new(&buffer, tokens.into_iter());
    let ast_tokens = parser.parse();
    println!("{:?}", ast_tokens.collect::<Vec<ASTToken>>());
}

fn main() -> Result<(), Error> {
    let file = std::env::args().nth(1);
    if let Some(path) = file {
        let bytes = fs::read(path)?;
        let buffer = str::from_utf8(&bytes)?;
        read(buffer);
        return Ok(());
    }
    loop {
        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer)?;
        read(&buffer);
    }
}
