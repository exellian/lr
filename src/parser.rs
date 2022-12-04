use crate::cursor::Cursor;

pub struct Parser<C: Cursor> {
    cursor: C
}

mod implementation {
    use crate::cursor::Cursor;
    use crate::parse::Parse;
    use crate::parser::Parser;

    impl<C: Cursor + Clone> Parser<C> {

        #[inline]
        pub fn new(cursor: C) -> Self {
            Parser {
                cursor
            }
        }

        #[inline]
        pub fn parse<P: Parse<Parser=Self>>(self) -> Result<(Self, P), P::Error> {
            P::parse(self)
        }

        #[inline]
        pub fn opt_parse<P: Parse<Parser=Self>>(self) -> (Self, Option<P>) {
            let snapshot = self.snapshot();
            match snapshot.parse::<P>() {
                Ok((parser, res)) => (parser, Some(res)),
                Err(_) => (self, None)
            }
        }

        #[inline]
        pub fn parse_wc<P: Parse<Parser=C>>(self) ->  Result<(Self, P), P::Error> {
            let (cursor, token) = P::parse(self.cursor)?;
            Ok((Self::new(cursor), token))
        }

        #[inline]
        pub fn opt_parse_wc<P: Parse<Parser=C>>(self) ->  (Self, Option<P>) {
            let snapshot = self.snapshot();
            match snapshot.parse_wc::<P>() {
                Ok((parser, res)) => (parser, Some(res)),
                Err(_) => (self, None)
            }
        }

        #[inline]
        fn snapshot(&self) -> Self {
            Self::new(self.cursor.clone())
        }

    }
}