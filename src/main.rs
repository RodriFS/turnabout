use std::fs;
use std::io;
use std::str;
use turnabout::error_reporting::report_lexer_errors;
use turnabout::lexer::{Lexer, Token};
use turnabout::cursor::{Cursor};

#[derive(Debug)]
enum Error {
    IOError(io::Error),
    Utf8Error(str::Utf8Error)
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

fn main() -> Result<(), Error> {
    let file = std::env::args().nth(1);
    if let Some(path) = file {
        let bytes = fs::read(path)?;
        let buffer = str::from_utf8(&bytes)?;
        let cursor = Cursor::new(buffer);
        let mut lexer = Lexer::new(cursor);
        let tokens = lexer.read();
        let tokens = report_lexer_errors(&buffer, tokens);
        println!("{:?}", tokens.collect::<Vec<Token>>());
        return Ok(());
    }
    loop {
        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer)?;
        let cursor = Cursor::new(&buffer);
        let mut lexer = Lexer::new(cursor);
        let tokens = lexer.read();
        let tokens = report_lexer_errors(&buffer, tokens);
        println!("{:?}", tokens.collect::<Vec<Token>>());
    }
}
