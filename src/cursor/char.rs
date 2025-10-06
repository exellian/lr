use std::collections::VecDeque;
use std::str::Chars;

#[derive(Clone)]
pub struct CharCursor<'a> {
    iter: Chars<'a>,
    lookahead: VecDeque<char>,
    position: usize,
}

mod implementation {
    use crate::cursor::char::CharCursor;
    use crate::cursor::Cursor;
    use std::collections::VecDeque;

    impl<'a> CharCursor<'a> {
        #[inline]
        pub fn new(code: &'a str) -> Self {
            CharCursor {
                iter: code.chars(),
                lookahead: VecDeque::new(),
                position: 0,
            }
        }
    }

    impl<'a> Cursor for CharCursor<'a> {
        type Item = char;

        #[inline]
        fn next(&mut self) -> Option<Self::Item> {
            let next = if let Some(ch) = self.lookahead.pop_front() {
                Some(ch)
            } else {
                self.iter.next()
            };
            if next.is_some() {
                self.position += 1;
            }
            next
        }

        #[inline]
        fn peek(&mut self, k: usize) -> Option<&Self::Item> {
            while self.lookahead.len() <= k {
                match self.iter.next() {
                    Some(ch) => self.lookahead.push_back(ch),
                    None => break,
                }
            }
            self.lookahead.get(k)
        }

        #[inline]
        fn position(&self) -> usize {
            self.position
        }
    }
}
