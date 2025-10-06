use crate::cursor::Cursor;

pub trait Peek<C: Cursor> {
    fn peek(token: &C::Item) -> bool;
}
