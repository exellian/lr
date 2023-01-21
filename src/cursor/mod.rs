mod char;

pub trait Cursor {
    type Item;

    fn next(&mut self) -> Option<Self::Item>;
    fn peek(&mut self, k: usize) -> Option<&Self::Item>;
    fn position(&self) -> usize;
}