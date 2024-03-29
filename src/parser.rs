use crate::cursor::Cursor;

pub struct Parser<C: Cursor> {
    pub(crate) cursor: C,
}

mod implementation {
    use crate::cursor::Cursor;
    use crate::parse::Parse;
    use crate::parser::Parser;
    use crate::peek::Peek;

    impl<C: Cursor + Clone> Parser<C> {
        #[inline]
        pub fn new(cursor: C) -> Self {
            Parser { cursor }
        }

        #[inline]
        pub fn peek<T: Peek>(&mut self, token: T, k: usize) -> bool {
            todo!()
        }

        #[inline]
        pub fn parse<P: Parse<Parser = Self>>(self) -> Result<(Self, P), P::Error> {
            P::parse(self)
        }

        pub fn opt_parse<P: Parse<Parser = Self>>(self) -> (Self, Option<P>) {
            let snapshot = self.snapshot();
            match snapshot.parse::<P>() {
                Ok((parser, res)) => (parser, Some(res)),
                Err(_) => (self, None),
            }
        }

        #[inline]
        pub fn parse_token<P: Parse<Parser = C>>(self) -> Result<(Self, P), P::Error> {
            let (cursor, token) = P::parse(self.cursor)?;
            Ok((Self::new(cursor), token))
        }

        #[inline]
        pub fn opt_parse_token<P: Parse<Parser = C>>(self) -> (Self, Option<P>) {
            let snapshot = self.snapshot();
            match snapshot.parse_token::<P>() {
                Ok((parser, res)) => (parser, Some(res)),
                Err(_) => (self, None),
            }
        }

        #[inline]
        fn snapshot(&self) -> Self {
            Self::new(self.cursor.clone())
        }
    }
}
