use crate::cursor::Cursor;

pub trait Parse where Self: Sized {
    type Error;
    type Token;

    fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error>;
}

pub trait Token where Self: Sized {
    type Error;
    type Token;

    fn parse<C: Cursor<Item=Self::Token>>(cursor: C) -> Result<(C, Self), Self::Error>;
}

pub struct Parser<C> where C: Cursor {
    cursor: C,
}
mod implementation {
    use crate::cursor::Cursor;
    use crate::parser::{Parse, Parser, Token};

    impl<C> Parser<C> where C: Cursor {

        #[inline]
        pub fn new(cursor: C) -> Self {
            Parser {
                cursor
            }
        }

        #[inline]
        pub fn parse<P: Parse<Token=C::Item>>(self) -> Result<(Self, P), P::Error> {
            P::parse(self)
        }

        #[inline]
        pub fn opt_parse<P: Parse<Token=C::Item>>(self) -> (Self, Option<P>) {
            let snapshot = self.snapshot();
            match snapshot.parse::<P>() {
                Ok((parser, res)) => (parser, Some(res)),
                Err(_) => (self, None)
            }
        }

        #[inline]
        pub fn parse_token<T: Token<Token=C::Item>>(self) ->  Result<(Self, T), T::Error> {
            let (cursor, token) = T::parse(self.cursor)?;
            Ok((Self::new(cursor), token))
        }

        #[inline]
        pub fn opt_parse_token<T: Token<Token=C::Item>>(self) ->  (Self, Option<T>) {
            let snapshot = self.snapshot();
            match snapshot.parse_token::<T>() {
                Ok((parser, res)) => (parser, Some(res)),
                Err(_) => (self, None)
            }
        }

        #[inline]
        pub fn finished(&mut self) -> bool {
            let next = self.cursor.peek();
            next.is_none()
        }

        pub fn peek(&mut self) -> Option<&C::Item> {
            self.cursor.peek()
        }

        #[inline]
        fn snapshot(&self) -> Self {
            Self::new(self.cursor.clone())
        }
    }

}
