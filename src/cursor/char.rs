use std::iter::Peekable;
use std::str::Chars;

#[derive(Clone)]
pub struct CharCursor<'a> {
    iter: Peekable<Chars<'a>>,
    position: usize
}

mod implementation {
    use crate::cursor::char::CharCursor;
    use crate::cursor::Cursor;

    impl<'a> CharCursor<'a> {

        #[inline]
        pub fn new(code: &'a str) -> Self {
            CharCursor {
                iter: code.chars().peekable(),
                position: 0
            }
        }

    }

    impl<'a> Cursor for CharCursor<'a> {
        type Item = char;

        #[inline]
        fn next(&mut self) -> Option<char> {
            let next = self.iter.next();
            self.position += 1;
            next
        }

        #[inline]
        fn peek(&mut self) -> Option<&char> {
            self.iter.peek()
        }

        #[inline]
        fn position(&self) -> usize {
            self.position
        }
    }
}