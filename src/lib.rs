mod cursor;
mod parse;
mod parser;
mod peek;
mod span;

pub use cursor::Cursor;
pub use parse::Parse;
pub use parser::Parser;
pub use peek::Peek;
pub use span::{Span, SpanOwned};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        #[derive(Clone)]
        struct Lexer;
        impl Cursor for Lexer {
            type Item = ();
            fn next(&mut self) -> Option<Self::Item> {
                todo!()
            }
            fn peek(&mut self) -> Option<&Self::Item> {
                todo!()
            }
            fn position(&self) -> usize {
                todo!()
            }
        }
        let parser = Parser { cursor: Lexer };
        struct Ident {
            x: (),
        }
        impl Token for Ident {}
        parser.peek(Ident, 0);
        //let result = add(2, 2);
        //assert_eq!(result, 4);
    }
}
