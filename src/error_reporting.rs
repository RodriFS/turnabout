use crate::lexer::{Token, TokenType};

pub fn report_lexer_errors<'a>(source: &'a str, tokens: impl Iterator<Item = Token> + 'a) -> impl Iterator<Item = Token> + 'a {
  let mut pos = 0;
  let errors = tokens.inspect(move |token| {
    if token.ttype == TokenType::Unexpected {
      let start = source [..pos].find(|c| c == '\n').unwrap_or(0);
      let end = source[start..].find(|c| c == '\n').unwrap_or(source.len());
      let line_count = source [..pos].chars().fold(0, |mut a, c| {if c == '\n' { a += 1; a } else { a }});

      println!("{} | {}",line_count, &source[start..end]);
      println!("\x1b[31m{:1$}^ Unexpected token\x1b[0m", ' ', pos + line_count.to_string().len() + 3);
      println!("{}", pos);
    }
    pos += token.len;
  });
  return errors;
}
