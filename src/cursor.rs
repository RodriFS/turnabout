use std::iter::Peekable;
use std::str::{Chars};

#[derive(Clone)]
pub struct Cursor<'a> {
    input: Peekable<Chars<'a>>,
    // from the start of the source to the position of the iterator
    char_count: usize,
    // from the start of the last time get_line_number is called, to the next time
    word_count: usize,
    line_nr: usize,
    line_start: usize,
    last_char: Option<char>,
}

impl Iterator for Cursor<'_> {
  type Item = char;

  fn next(&mut self) -> Option<char> {
    match self.input.next() {
      Some(c) => {
        let char_length = c.len_utf8();
        self.last_char.replace(c);
        self.char_count += char_length;
        self.word_count += char_length;
        if c == '\n' {
          self.line_nr += 1;
          self.line_start = self.char_count;
        }
        Some(c)
      },
      _ => None
    }
}
}

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.chars().peekable(),
            char_count: 0,
            word_count: 0,
            line_nr: 0,
            line_start: 0,
            last_char: None
        }
    }

    pub fn peek(&mut self) -> Option<&char> {
      self.input.peek()
    }

    pub fn get_len_consumed(&mut self) -> usize {
        let len = self.word_count;
        self.word_count = 0;
        len
    }

    pub fn get_line_number(&self) -> usize {
      self.line_nr + 1
    }

    pub fn get_column(&self) -> usize {
      self.char_count - self.line_start
    }

    pub fn get_last_char(&self) -> Option<char> {
      self.last_char
    }
}
