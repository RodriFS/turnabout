use std::fs;
use std::io;
use std::str;
use turnabout::cursor::Cursor;
use turnabout::error_reporting::report_lexer_errors;
use turnabout::eval::Interpreter;
use turnabout::lexer::Lexer;
use turnabout::parser::{Expr, Parser};

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

fn read<'a>(buffer: &str) -> Result<Vec<Expr>, String> {
    let cursor = Cursor::new(&buffer);
    let mut lexer = Lexer::new(cursor);
    let tokens = lexer.read();
    let lexer_errors = report_lexer_errors(&buffer, &tokens);
    if lexer_errors {
        return Err("Lexer error".to_string());
    }
    let mut parser = Parser::new(tokens.into_iter());
    let ast = parser.parse().collect();
    Ok(ast)
}

fn eval(mut ast: Vec<Expr>) {
    let interpreter = Interpreter::new();
    let result = interpreter.eval(ast.pop().unwrap());
    println!("{}", result);
}

fn main() -> Result<(), Error> {
    let file = std::env::args().nth(1);
    if let Some(path) = file {
        let bytes = fs::read(path)?;
        let buffer = str::from_utf8(&bytes)?;
        eval(read(buffer).unwrap());
        return Ok(());
    }
    loop {
        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer)?;
        if &buffer == "\n" {
            continue;
        }
        eval(read(&buffer).unwrap());
    }
}
