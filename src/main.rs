use std::cell::RefCell;
use std::fs;
use std::io;
use std::rc::Rc;
use std::str;
use turnabout::cursor::Cursor;
use turnabout::environment::Environment;
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

fn read<'a>(buffer: &str) -> Result<Expr, String> {
    let cursor = Cursor::new(&buffer);
    let mut lexer = Lexer::new(cursor);
    let tokens = lexer.read();
    let lexer_errors = report_lexer_errors(&buffer, &tokens);
    if lexer_errors {
        return Err("Lexer error".to_string());
    }
    let mut parser = Parser::new(tokens.into_iter());
    let ast = parser.parse();
    Ok(ast)
}

fn eval(interpreter: &Interpreter, env: Rc<RefCell<Environment>>, ast: Expr) {
    let result = interpreter.eval(ast, env);
    match result {
        Ok(t) => println!("{}", t),
        Err(e) => println!("{}", e),
    }
}

fn main() -> Result<(), Error> {
    let file = std::env::args().nth(1);
    let interpreter = Interpreter::new();
    let env = Rc::new(RefCell::new(Environment::new(None)));
    if let Some(path) = file {
        let bytes = fs::read(path)?;
        let buffer = str::from_utf8(&bytes)?;
        eval(&interpreter, env, read(buffer).unwrap());
        return Ok(());
    }

    loop {
        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer)?;
        if &buffer == "\n" {
            continue;
        }
        eval(&interpreter, env.clone(), read(&buffer).unwrap());
    }
}
