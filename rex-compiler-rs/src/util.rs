use std::ops::Range;

#[derive(Clone)]
pub struct Span<'a> {
    code: &'a str,
    range: Range<usize>
}

#[derive(Clone, Debug)]
pub struct SpanOwned {
    value: String,
    range: Range<usize>
}

mod implementation {
    use std::fmt::{Debug, Formatter};
    use std::ops::Range;
    use crate::util::{Span, SpanOwned};

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

        pub fn owned(&self) -> SpanOwned {
            SpanOwned {
                value: self.value().to_string(),
                range: self.range.clone()
            }
        }
    }

    impl<'a> Debug for Span<'a> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.value())
        }
    }
}
