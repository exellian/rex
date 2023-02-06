use std::collections::{BTreeMap, hash_map, HashMap, HashSet};
use std::hash::Hash;
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

pub struct HashMultimap<K, V> {
    inner: HashMap<K, HashSet<V>>
}
impl<K, V> HashMultimap<K, V> where K: Eq + Hash, V:  Eq + Hash {

    #[inline]
    pub fn new() -> Self {
        HashMultimap {
            inner: HashMap::new()
        }
    }

    pub fn union(&mut self, other: HashMultimap<K, V>) {
        for (key, values) in other.inner {
            match self.inner.get_mut(&key) {
                None => {
                    self.inner.insert(key, values);
                }
                Some(set) => {
                    set.extend(values);
                }
            }
        }
    }

    pub fn union_with_map(&mut self, other: BTreeMap<K, V>) {
        for (key, value) in other {
            match self.inner.get_mut(&key) {
                None => {
                    self.inner.insert(key, HashSet::from([value]));
                }
                Some(set) => {
                    set.insert(value);
                }
            }
        }
    }

    #[inline]
    pub fn get(&self, key: &K) -> Option<&HashSet<V>> {
        self.inner.get(key)
    }

    pub fn extend<I>(&mut self, other: impl IntoIterator<Item=(K, I)>) where I: IntoIterator<Item=V> {
        for (key, values) in other.into_iter() {
            let mut tmp = None;
            let set = match self.inner.get_mut(&key) {
                None => {
                    tmp = Some(HashSet::new());
                    tmp.as_mut().unwrap()
                }
                Some(set) => set,
            };
            for value in values.into_iter() {
                set.insert(value);
            }
            if let Some(set) = tmp {
                self.inner.insert(key, set);
            }
        }
    }

    pub fn insert(&mut self, key: K, val: V) {
        match self.inner.get_mut(&key) {
            None => {
                self.inner.insert(key, HashSet::from([val]));
            }
            Some(set) => {
                set.insert(val);
            }
        }
    }
}

impl<K, V> IntoIterator for HashMultimap<K, V> {
    type Item = (K, HashSet<V>);
    type IntoIter = hash_map::IntoIter<K, HashSet<V>>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

impl<K, V> From<BTreeMap<K, V>> for HashMultimap<K, V> where K: Hash + Eq, V: Hash + Eq {
    fn from(value: BTreeMap<K, V>) -> Self {
        HashMultimap {
            inner: value.into_iter()
                .map(|(k, v)| (k, HashSet::from([v])))
                .collect(),
        }
    }
}

impl<const N: usize, K, V, I> From<[(K, I);N]> for HashMultimap<K, V> where K: Hash + Eq, V: Hash + Eq, I: IntoIterator<Item=V> {

    fn from(value: [(K, I); N]) -> Self {
        let mut res = HashMultimap::new();
        res.extend(value);
        res
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
