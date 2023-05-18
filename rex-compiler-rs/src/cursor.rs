use std::iter::Peekable;
use std::str::Chars;

pub trait Cursor: Clone {
    type Item;

    fn next(&mut self) -> Option<Self::Item>;
    fn peek(&mut self) -> Option<&Self::Item>;
    fn position(&self) -> usize;
}

#[derive(Clone)]
pub struct CharCursor<'a> {
    iter: Peekable<Chars<'a>>,
    position: usize,
}

mod implementation {
    use crate::cursor::{CharCursor, Cursor};

    impl<'a> CharCursor<'a> {
        pub fn new(code: &'a str) -> Self {
            CharCursor {
                iter: code.chars().peekable(),
                position: 0,
            }
        }
    }

    impl<'a> Cursor for CharCursor<'a> {
        type Item = char;

        fn next(&mut self) -> Option<char> {
            let next = self.iter.next();
            self.position += 1;
            next
        }

        fn peek(&mut self) -> Option<&char> {
            self.iter.peek()
        }

        fn position(&self) -> usize {
            self.position
        }
    }
}
