use std::collections::{HashMap, HashSet};
use std::ops::Range;

#[derive(Clone, Eq, Hash)]
pub struct Span<'a> {
    pub code: &'a str,
    pub range: Range<usize>
}

#[derive(Clone, Debug)]
pub struct SpanOwned {
    value: String,
    range: Range<usize>
}

pub struct Multimap<K, V> {
    inner: HashMap<K, HashSet<V>>
}
impl<K, V> Multimap<K, V> where V: PartialEq {

    pub fn insert(&mut self, val: V) {
        self.inner.
    }
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
