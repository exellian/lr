use std::ops::Range;

#[derive(Eq, Hash)]
pub struct Span<'a> {
    pub code: &'a str,
    pub range: Range<usize>
}

#[derive(Clone, Debug)]
pub struct SpanOwned {
    value: String,
    range: Range<usize>
}

mod implementation {
    use std::borrow::Borrow;
    use std::fmt::{Debug, Formatter};
    use std::ops::Range;
    use crate::span::{Span, SpanOwned};

    impl<'a> Span<'a> {

        pub fn new(code: &'a str, range: Range<usize>) -> Self {
            Span {
                code,
                range
            }
        }

        pub fn value(&self) -> &str {
            &self.code[self.range.clone()]
        }

    }

    impl<'a> ToOwned for Span<'a> {
        type Owned = SpanOwned;

        fn to_owned(&self) -> Self::Owned {
            SpanOwned {
                value: self.value().to_string(),
                range: self.range.clone()
            }
        }
    }

    impl<'a> Borrow<Span<'a>> for SpanOwned {
        fn borrow(&self) -> &Span<'a> {
            todo!()
        }
    }

    impl<'a> PartialEq for Span<'a> {
        fn eq(&self, other: &Self) -> bool {
            self.value() == other.value()
        }
    }

    impl<'a> Debug for Span<'a> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.value())
        }
    }
}