pub mod lex {
    use crate::cursor::CharCursor;
    use crate::util::Span;

    #[derive(Clone, Debug)]
    pub enum Error {
        UnexpectedEof,
        UnexpectedToken
    }

    #[derive(Clone, Debug)]
    pub struct Punct<'a> {
        pub span: Span<'a>,
        pub ch: char
    }

    #[derive(Clone, Debug)]
    pub struct Text<'a> {
        pub span: Span<'a>
    }

    #[derive(Clone, Debug)]
    pub struct Whitespace<'a> {
        pub span: Span<'a>,
    }

    #[derive(Clone, Debug)]
    pub enum StringDelimiter {
        Single,
        Double
    }

    #[derive(Clone, Debug)]
    pub struct StrLiteral<'a> {
        pub span: Span<'a>,
        pub delimiter: StringDelimiter
    }

    #[derive(Clone, Debug)]
    pub struct IntLiteral<'a> {
        pub span: Span<'a>
    }

    #[derive(Clone, Debug)]
    pub struct FloatLiteral<'a> {
        pub span: Span<'a>
    }

    #[derive(Clone, Debug)]
    pub enum Literal<'a> {
        Str(StrLiteral<'a>),
        Int(IntLiteral<'a>),
        Float(FloatLiteral<'a>)
    }

    #[derive(Clone, Debug)]
    pub enum Token<'a> {
        Punct(Punct<'a>),
        Text(Text<'a>),
        Literal(Literal<'a>),
        Whitespace(Whitespace<'a>)
    }

    #[derive(Clone)]
    pub struct Lexer<'a> {
        code: &'a str,
        cursor: CharCursor<'a>,
        peeked: Option<Result<Token<'a>, Error>>,
        position: usize
    }

    mod implementation {
        use crate::cursor::{CharCursor, Cursor};
        use crate::parser;
        use crate::rex::lex::{Error, FloatLiteral, IntLiteral, Lexer, Literal, Punct, StringDelimiter, StrLiteral, Text, Token, Whitespace};
        use crate::util::Span;

        struct StrLiteralContext {
            delimiter: StringDelimiter,
            next_char_escaped: bool,
        }

        struct NumLiteralContext {
            contains_point: bool
        }

        enum LexerContext {
            StrLiteral(StrLiteralContext),
            Whitespace,
            NumLiteralOrDot,
            NumLiteral(NumLiteralContext),
            Text
        }

        impl<'a> Token<'a> {

            pub fn span(&self) -> Span<'a> {
                match self {
                    Token::Punct(punct) => punct.span.clone(),
                    Token::Text(text) => text.span.clone(),
                    Token::Literal(lit) => match lit {
                        Literal::Str(str) => str.span.clone(),
                        Literal::Int(i) => i.span.clone(),
                        Literal::Float(f) => f.span.clone()
                    }
                    Token::Whitespace(ws) => ws.span.clone()
                }
            }
        }

        impl<'a> parser::Token for Punct<'a> {
            type Error = Error;
            type Token = Result<Token<'a>, Self::Error>;

            fn parse<C: Cursor<Item=Self::Token>>(mut cursor: C) -> Result<(C, Self), Self::Error> {
                if let Some(next) = cursor.next() {
                    if let Token::Punct(punct) = next? {
                        return Ok((cursor, punct));
                    }
                }
                Err(Error::UnexpectedToken)
            }
        }

        impl<'a> parser::Token for Text<'a> {
            type Error = Error;
            type Token = Result<Token<'a>, Self::Error>;

            fn parse<C: Cursor<Item=Self::Token>>(mut cursor: C) -> Result<(C, Self), Self::Error> {
                if let Some(next) = cursor.next() {
                    if let Token::Text(text) = next? {
                        return Ok((cursor, text));
                    }
                }
                Err(Error::UnexpectedToken)
            }
        }

        impl<'a> parser::Token for Whitespace<'a> {
            type Error = Error;
            type Token = Result<Token<'a>, Self::Error>;

            fn parse<C: Cursor<Item=Self::Token>>(mut cursor: C) -> Result<(C, Self), Self::Error> {
                if let Some(next) = cursor.next() {
                    if let Token::Whitespace(ws) = next? {
                        return Ok((cursor, ws));
                    }
                }
                Err(Error::UnexpectedToken)
            }
        }

        impl<'a> parser::Token for StrLiteral<'a> {
            type Error = Error;
            type Token = Result<Token<'a>, Self::Error>;

            fn parse<C: Cursor<Item=Self::Token>>(mut cursor: C) -> Result<(C, Self), Self::Error> {
                if let Some(next) = cursor.next() {
                    if let Token::Literal(Literal::Str(lit)) = next? {
                        return Ok((cursor, lit));
                    }
                }
                Err(Error::UnexpectedToken)
            }
        }

        impl<'a> parser::Token for IntLiteral<'a> {
            type Error = Error;
            type Token = Result<Token<'a>, Self::Error>;

            fn parse<C: Cursor<Item=Self::Token>>(mut cursor: C) -> Result<(C, Self), Self::Error> {
                if let Some(next) = cursor.next() {
                    if let Token::Literal(Literal::Int(lit)) = next? {
                        return Ok((cursor, lit));
                    }
                }
                Err(Error::UnexpectedToken)
            }
        }

        impl<'a> parser::Token for FloatLiteral<'a> {
            type Error = Error;
            type Token = Result<Token<'a>, Self::Error>;

            fn parse<C: Cursor<Item=Self::Token>>(mut cursor: C) -> Result<(C, Self), Self::Error> {
                if let Some(next) = cursor.next() {
                    if let Token::Literal(Literal::Float(lit)) = next? {
                        return Ok((cursor, lit));
                    }
                }
                Err(Error::UnexpectedToken)
            }
        }

        impl<'a> Lexer<'a> {
            
            pub const SINGLE_QUOTE: char = '\'';
            pub const DOUBLE_QUOTE: char = '"';
            pub const DECIMAL_0: char = '0';
            pub const DECIMAL_1: char = '1';
            pub const DECIMAL_2: char = '2';
            pub const DECIMAL_3: char = '3';
            pub const DECIMAL_4: char = '4';
            pub const DECIMAL_5: char = '5';
            pub const DECIMAL_6: char = '6';
            pub const DECIMAL_7: char = '7';
            pub const DECIMAL_8: char = '8';
            pub const DECIMAL_9: char = '9';
            pub const TAB: char = '\u{0009}';
            pub const LF: char = '\u{000a}';
            pub const FF: char = '\u{000c}';
            pub const CR: char = '\u{000d}';
            pub const SPACE: char = '\u{0020}';
            pub const DOT: char = '.';
            pub const COMMA: char = ',';
            pub const PLUS: char = '+';
            pub const MINUS: char = '-';
            pub const UNDERSCORE: char = '_';
            pub const MUL: char = '*';
            pub const DIV: char = '/';
            pub const MOD: char = '%';
            pub const EQ: char = '=';
            pub const AND: char = '&';
            pub const OR: char = '|';
            pub const XOR: char = '^';
            pub const LT: char = '<';
            pub const GT: char = '>';
            pub const NEG: char = '!';
            pub const PAREN_LEFT: char = '(';
            pub const PAREN_RIGHT: char = ')';
            pub const BRACE_LEFT: char = '{';
            pub const BRACE_RIGHT: char = '}';
            pub const BRACKET_LEFT: char = '[';
            pub const BRACKET_RIGHT: char = ']';

            #[inline]
            pub fn new(code: &'a str) -> Self {
                Lexer {
                    code,
                    cursor: CharCursor::new(code),
                    peeked: None,
                    position: 0
                }
            }

            #[inline]
            fn is_text(ch: char) -> bool {
                !Self::is_decimal(ch) &&
                    !Self::is_whitespace(ch) &&
                    !Self::is_punct(ch) &&
                    !Self::is_quote(ch)
            }

            #[inline]
            fn is_quote(ch: char) -> bool {
                ch == Self::SINGLE_QUOTE ||
                    ch == Self::DOUBLE_QUOTE
            }

            #[inline]
            fn is_punct(ch: char) -> bool {
                ch == Self::COMMA ||
                    ch == Self::DOT ||
                    ch == Self::PLUS ||
                    ch == Self::MINUS ||
                    ch == Self::MUL ||
                    ch == Self::DIV ||
                    ch == Self::MOD ||
                    ch == Self::EQ ||
                    ch == Self::AND ||
                    ch == Self::OR ||
                    ch == Self::XOR ||
                    ch == Self::LT ||
                    ch == Self::GT ||
                    ch == Self::NEG ||
                    ch == Self::PAREN_LEFT ||
                    ch == Self::PAREN_RIGHT ||
                    ch == Self::BRACKET_LEFT ||
                    ch == Self::BRACKET_RIGHT ||
                    ch == Self::BRACE_LEFT ||
                    ch == Self::BRACE_RIGHT
            }

            #[inline]
            fn is_decimal(ch: char) -> bool {
                ch == Self::DECIMAL_0 ||
                    ch == Self::DECIMAL_1 ||
                    ch == Self::DECIMAL_2 ||
                    ch == Self::DECIMAL_3 ||
                    ch == Self::DECIMAL_4 ||
                    ch == Self::DECIMAL_5 ||
                    ch == Self::DECIMAL_6 ||
                    ch == Self::DECIMAL_7 ||
                    ch == Self::DECIMAL_8 ||
                    ch == Self::DECIMAL_9
            }

            #[inline]
            fn is_whitespace(ch: char) -> bool {
                ch == Self::TAB || /* TAB */
                    ch == Self::LF || /* LF */
                    ch == Self::FF || /* FF */
                    ch == Self::CR || /* CR */
                    ch == Self::SPACE /* SPACE */
            }

            fn _next(&mut self) -> Option<<Lexer<'a> as Cursor>::Item> {
                let mut token = Vec::new();
                let start_position = self.cursor.position();
                let mut last = self.cursor.next()?;
                token.push(last);

                let mut context = match last {
                    Self::DOUBLE_QUOTE => LexerContext::StrLiteral(StrLiteralContext {
                        delimiter: StringDelimiter::Double,
                        next_char_escaped: false
                    }),
                    Self::SINGLE_QUOTE => LexerContext::StrLiteral(StrLiteralContext {
                        delimiter: StringDelimiter::Single,
                        next_char_escaped: false
                    }),
                    Self::DECIMAL_0 |
                    Self::DECIMAL_1 |
                    Self::DECIMAL_2 |
                    Self::DECIMAL_3 |
                    Self::DECIMAL_4 |
                    Self::DECIMAL_5 |
                    Self::DECIMAL_6 |
                    Self::DECIMAL_7 |
                    Self::DECIMAL_8 |
                    Self::DECIMAL_9 => LexerContext::NumLiteral(NumLiteralContext {
                        contains_point: false
                    }),
                    Self::DOT // A point can be a Punct or a start of a float number. Therefore we have to look at
                    // the next character to find out
                    => LexerContext::NumLiteralOrDot,
                    Self::TAB | /* TAB */
                    Self::LF | /* LF */
                    Self::FF | /* FF */
                    Self::CR | /* CR */
                    Self::SPACE /* SPACE */ =>
                        LexerContext::Whitespace,
                    Self::COMMA |
                    Self::PLUS |
                    Self::MINUS |
                    Self::MUL |
                    Self::DIV |
                    Self::MOD |
                    Self::EQ |
                    Self::AND |
                    Self::OR |
                    Self::XOR |
                    Self::LT |
                    Self::GT |
                    Self::NEG |
                    Self::PAREN_LEFT |
                    Self::PAREN_RIGHT |
                    Self::BRACE_LEFT |
                    Self::BRACE_RIGHT |
                    Self::BRACKET_LEFT |
                    Self::BRACKET_RIGHT => return Some(Ok(Token::Punct(Punct {
                        span: Span::new(self.code, start_position..start_position + 1),
                        ch: last
                    }))),
                    _ => LexerContext::Text
                };

                loop {
                    match &mut context {
                        LexerContext::StrLiteral(ctx) => {
                            let next1 = self.cursor.peek().cloned();
                            if let Some(ch) = next1 {
                                if ch == '\\' {
                                    ctx.next_char_escaped = true;
                                } else if !ctx.next_char_escaped && ch == ctx.delimiter.close_char() {
                                    // move stream to peeked token
                                    token.push(self.cursor.next().unwrap());
                                    return Some(Ok(Token::Literal(Literal::Str(StrLiteral {
                                        span: Span::new(self.code, start_position..start_position + token.len()),
                                        delimiter: ctx.delimiter.clone(),
                                    }))))
                                } else {
                                    ctx.next_char_escaped = false;
                                }
                            }
                        }
                        LexerContext::Whitespace => {
                            let next1 = self.cursor.peek().cloned();
                            if next1.is_none() || !Self::is_whitespace(next1.unwrap()) {
                                return Some(Ok(Token::Whitespace(Whitespace {
                                    span: Span::new(self.code, start_position..start_position + token.len()),
                                })))
                            }
                        }
                        LexerContext::NumLiteralOrDot => {
                            let next1 = self.cursor.peek().cloned();
                            if next1.is_none() || !Self::is_decimal(next1.unwrap()) {
                                return Some(Ok(Token::Punct(Punct {
                                    span: Span::new(self.code, start_position..start_position + 1),
                                    ch: Self::DOT
                                })))
                            } else {
                                // Make a context switch to a number
                                context = LexerContext::NumLiteral(NumLiteralContext {
                                    contains_point: true
                                })
                            }
                        }
                        LexerContext::NumLiteral(ctx) => {
                            let next1 = self.cursor.peek().cloned();
                            if next1.is_none() ||
                                (ctx.contains_point && !Self::is_decimal(next1.unwrap())) ||
                                (!ctx.contains_point && !(Self::is_decimal(next1.unwrap()) || next1.unwrap() == Self::DOT))
                            {
                                return if ctx.contains_point {
                                    Some(Ok(Token::Literal(Literal::Float(FloatLiteral {
                                        span: Span::new(self.code, start_position..start_position + token.len())
                                    }))))
                                } else {
                                    Some(Ok(Token::Literal(Literal::Int(IntLiteral {
                                        span: Span::new(self.code, start_position..start_position + token.len())
                                    }))))
                                }
                            } else {
                                if let Some(c) = next1 {
                                    if c == Self::DOT {
                                        ctx.contains_point = true;
                                    }
                                }
                            }
                        }
                        LexerContext::Text => {
                            let next1 = self.cursor.peek().cloned();
                            if next1.is_none() || !Self::is_text(next1.unwrap()) {
                                return Some(Ok(Token::Text(Text {
                                    span: Span::new(self.code, start_position..start_position + token.len())
                                })))
                            }
                        }
                    }
                    last = match self.cursor.next() {
                        Some(ch) => ch,
                        None => return Some(Err(Error::UnexpectedEof))
                    };
                    token.push(last);
                }
            }
        }

        impl<'a> Cursor for Lexer<'a> {
            type Item = Result<Token<'a>, Error>;

            #[inline]
            fn next(&mut self) -> Option<Self::Item> {
                self.position += 1;
                if let Some(res) = self.peeked.take() {
                    return Some(res);
                }
                self._next()
            }

            #[inline]
            fn peek(&mut self) -> Option<&Self::Item> {
                if self.peeked.is_none() {
                    self.peeked = Some(self._next()?);
                }
                self.peeked.as_ref()
            }

            #[inline]
            fn position(&self) -> usize {
                self.position
            }
        }

        impl StringDelimiter {
            fn close_char(&self) -> char {
                match self {
                    StringDelimiter::Single => Lexer::SINGLE_QUOTE,
                    StringDelimiter::Double => Lexer::DOUBLE_QUOTE
                }
            }
        }
    }
}

pub mod parse {
    use std::marker::PhantomData;
    use crate::rex::lex;
    use crate::rex::lex::Text;
    use crate::rex::parse::primitive::Empty;
    use crate::rex::parse::scope::Scope;
    use crate::rex::parse::typ::Type;
    use crate::util::{Span, SpanOwned};

    #[derive(Debug)]
    pub enum Error {
        Lexer(lex::Error),
        UnexpectedToken(SpanOwned),
        UnexpectedEof,
        TypeError(Type, Type),
        UnsupportedOperation
    }

    pub mod primitive {
        use std::marker::PhantomData;
        use crate::rex::lex::{FloatLiteral, IntLiteral, Punct, StrLiteral, Text};

        #[derive(Debug)]
        pub struct Ident<'a> {
            pub text: Text<'a>
        }
        #[derive(Debug)]
        pub struct BraceLeft<'a> {
            punct: Punct<'a>
        }
        #[derive(Debug)]
        pub struct BraceRight<'a> {
            punct: Punct<'a>
        }
        #[derive(Debug)]
        pub struct BracketLeft<'a> {
            punct: Punct<'a>
        }
        #[derive(Debug)]
        pub struct BracketRight<'a> {
            punct: Punct<'a>
        }
        #[derive(Debug)]
        pub struct ParenLeft<'a> {
            punct: Punct<'a>
        }
        #[derive(Debug)]
        pub struct ParenRight<'a> {
            punct: Punct<'a>
        }
        #[derive(Debug)]
        pub struct Add<'a> {
            punct: Punct<'a>
        }
        #[derive(Debug)]
        pub struct Sub<'a> {
            punct: Punct<'a>
        }
        #[derive(Debug)]
        pub struct Mul<'a> {
            punct: Punct<'a>
        }
        #[derive(Debug)]
        pub struct Div<'a> {
            punct: Punct<'a>
        }
        #[derive(Debug)]
        pub struct Mod<'a> {
            punct: Punct<'a>
        }
        #[derive(Debug)]
        pub struct Bang<'a> {
            punct: Punct<'a>,
        }
        #[derive(Debug)]
        pub struct Lt<'a> {
            punct: Punct<'a>
        }
        #[derive(Debug)]
        pub struct Gt<'a> {
            punct: Punct<'a>
        }
        #[derive(Debug)]
        pub struct Le<'a> {
            punct0: Punct<'a>,
            punct1: Punct<'a>
        }
        #[derive(Debug)]
        pub struct Ge<'a> {
            punct0: Punct<'a>,
            punct1: Punct<'a>
        }
        #[derive(Debug)]
        pub struct And<'a> {
            punct: Punct<'a>
        }
        #[derive(Debug)]
        pub struct AndAnd<'a> {
            punct0: Punct<'a>,
            punct1: Punct<'a>
        }
        #[derive(Debug)]
        pub struct Or<'a> {
            punct: Punct<'a>
        }
        #[derive(Debug)]
        pub struct OrOr<'a> {
            punct0: Punct<'a>,
            punct1: Punct<'a>
        }
        #[derive(Debug)]
        pub struct Xor<'a> {
            punct: Punct<'a>
        }
        #[derive(Debug)]
        pub struct Eq<'a> {
            punct: Punct<'a>
        }
        #[derive(Debug)]
        pub struct EqEq<'a> {
            punct0: Punct<'a>,
            punct1: Punct<'a>
        }
        #[derive(Debug)]
        pub struct Ne<'a> {
            punct0: Punct<'a>,
            punct1: Punct<'a>
        }
        #[derive(Debug)]
        pub struct Comma<'a> {
            punct: Punct<'a>
        }
        #[derive(Debug)]
        pub struct Dot<'a> {
            punct: Punct<'a>
        }
        #[derive(Debug)]
        pub struct If<'a> {
            text: Text<'a>
        }
        #[derive(Debug)]
        pub struct Else<'a> {
            text: Text<'a>
        }
        #[derive(Debug)]
        pub struct For<'a> {
            text: Text<'a>
        }
        #[derive(Debug)]
        pub struct In<'a> {
            text: Text<'a>
        }
        #[derive(Debug)]
        pub enum Lit<'a> {
            Str(LitStr<'a>),
            Int(LitInt<'a>),
            Float(LitFloat<'a>),
            Bool(LitBool<'a>),
        }
        #[derive(Debug)]
        pub struct LitStr<'a> {
            pub lit: StrLiteral<'a>
        }
        #[derive(Debug)]
        pub struct LitInt<'a> {
            pub lit: IntLiteral<'a>,
            pub value: usize
        }
        #[derive(Debug)]
        pub struct LitFloat<'a> {
            pub lit: FloatLiteral<'a>,
            pub value: f64
        }
        #[derive(Debug)]
        pub struct LitBool<'a> {
            pub text: Text<'a>,
            pub value: bool
        }

        #[derive(Debug)]
        pub struct Empty<'a> {
            _a: PhantomData<&'a ()>
        }

        mod implementation {
            use std::marker::PhantomData;
            use crate::cursor::Cursor;
            use crate::parser::{Parse, Parser};
            use crate::rex::lex;
            use crate::rex::lex::Lexer;
            use crate::rex::parse::Error;
            use crate::rex::parse::primitive::{Add, And, AndAnd, BraceLeft, BraceRight, BracketLeft, BracketRight, Comma, Div, Dot, Else, Empty, Eq, EqEq, For, Ge, Gt, Ident, If, In, Le, Lit, LitBool, LitFloat, LitInt, LitStr, Lt, Mod, Mul, Ne, Bang, Or, OrOr, ParenLeft, ParenRight, Sub, Xor};

            impl<'a> PartialEq for Ident<'a> {
                fn eq(&self, other: &Self) -> bool {
                    self.text.span.value() == other.text.span.value()
                }
            }

            fn check_chars<P>(str: &str, predicate: P) -> bool where P: Fn(char) -> bool {
                str.chars().all(predicate)
            }

            fn is_ident_char(c: char) -> bool {
                let val = c as u32;
                (val >= '0' as u32 && val <= '9' as u32) ||
                    (val >= 'a' as u32 && val <= 'z' as u32) ||
                    (val >= 'A' as u32 && val <= 'Z' as u32) ||
                    (val == Lexer::UNDERSCORE as u32)
            }

            fn is_keyword(value: &str) -> bool {
                value == "for" ||
                    value == "if" ||
                    value == "else" ||
                    value == "in" ||
                    value == "true" ||
                    value == "false"
            }

            impl<'a> Parse for Ident<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, text) = parser.parse_token::<lex::Text>()?;
                    let value = text.span.value();
                    return if !is_keyword(value) && check_chars(value, is_ident_char) {
                        Ok((parser, Ident {
                            text
                        }))
                    } else {
                        Err(Error::UnexpectedToken(text.span.owned()))
                    }
                }
            }

            impl<'a> Parse for BracketLeft<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, punct) = parser.parse_token::<lex::Punct>()?;
                    return if punct.ch == Lexer::BRACKET_LEFT {
                        Ok((parser, BracketLeft {
                            punct
                        }))
                    } else {
                        Err(Error::UnexpectedToken(punct.span.owned()))
                    }
                }
            }

            impl<'a> Parse for BracketRight<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, punct) = parser.parse_token::<lex::Punct>()?;
                    return if punct.ch == Lexer::BRACKET_RIGHT {
                        Ok((parser, BracketRight {
                            punct
                        }))
                    } else {
                        Err(Error::UnexpectedToken(punct.span.owned()))
                    }
                }
            }

            impl<'a> Parse for BraceLeft<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, punct) = parser.parse_token::<lex::Punct>()?;
                    return if punct.ch == Lexer::BRACE_LEFT {
                        Ok((parser, BraceLeft {
                            punct
                        }))
                    } else {
                        Err(Error::UnexpectedToken(punct.span.owned()))
                    }
                }
            }

            impl<'a> Parse for BraceRight<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, punct) = parser.parse_token::<lex::Punct>()?;
                    return if punct.ch == Lexer::BRACE_RIGHT {
                        Ok((parser, BraceRight {
                            punct
                        }))
                    } else {
                        Err(Error::UnexpectedToken(punct.span.owned()))
                    }
                }
            }

            impl<'a> Parse for ParenLeft<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, punct) = parser.parse_token::<lex::Punct>()?;
                    return if punct.ch == Lexer::PAREN_LEFT {
                        Ok((parser, ParenLeft {
                            punct
                        }))
                    } else {
                        Err(Error::UnexpectedToken(punct.span.owned()))
                    }
                }
            }

            impl<'a> Parse for ParenRight<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, punct) = parser.parse_token::<lex::Punct>()?;
                    return if punct.ch == Lexer::PAREN_RIGHT {
                        Ok((parser, ParenRight {
                            punct
                        }))
                    } else {
                        Err(Error::UnexpectedToken(punct.span.owned()))
                    }
                }
            }

            impl<'a> Parse for Add<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, punct) = parser.parse_token::<lex::Punct>()?;
                    return if punct.ch == Lexer::PLUS {
                        Ok((parser, Add {
                            punct
                        }))
                    } else {
                        Err(Error::UnexpectedToken(punct.span.owned()))
                    }
                }
            }

            impl<'a> Parse for Sub<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, punct) = parser.parse_token::<lex::Punct>()?;
                    return if punct.ch == Lexer::MINUS {
                        Ok((parser, Sub {
                            punct
                        }))
                    } else {
                        Err(Error::UnexpectedToken(punct.span.owned()))
                    }
                }
            }

            impl<'a> Parse for Mul<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, punct) = parser.parse_token::<lex::Punct>()?;
                    return if punct.ch == Lexer::MUL {
                        Ok((parser, Mul {
                            punct
                        }))
                    } else {
                        Err(Error::UnexpectedToken(punct.span.owned()))
                    }
                }
            }

            impl<'a> Parse for Div<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, punct) = parser.parse_token::<lex::Punct>()?;
                    return if punct.ch == Lexer::DIV {
                        Ok((parser, Div {
                            punct
                        }))
                    } else {
                        Err(Error::UnexpectedToken(punct.span.owned()))
                    }
                }
            }

            impl<'a> Parse for Mod<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, punct) = parser.parse_token::<lex::Punct>()?;
                    return if punct.ch == Lexer::MOD {
                        Ok((parser, Mod {
                            punct
                        }))
                    } else {
                        Err(Error::UnexpectedToken(punct.span.owned()))
                    }
                }
            }

            impl<'a> Parse for Bang<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, punct) = parser.parse_token::<lex::Punct>()?;
                    return if punct.ch == Lexer::NEG {
                        Ok((parser, Bang {
                            punct
                        }))
                    } else {
                        Err(Error::UnexpectedToken(punct.span.owned()))
                    }
                }
            }

            impl<'a> Parse for Lt<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, punct) = parser.parse_token::<lex::Punct>()?;
                    return if punct.ch == Lexer::LT {
                        Ok((parser, Lt {
                            punct
                        }))
                    } else {
                        Err(Error::UnexpectedToken(punct.span.owned()))
                    }
                }
            }

            impl<'a> Parse for Gt<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, punct) = parser.parse_token::<lex::Punct>()?;
                    return if punct.ch == Lexer::GT {
                        Ok((parser, Gt {
                            punct
                        }))
                    } else {
                        Err(Error::UnexpectedToken(punct.span.owned()))
                    }
                }
            }

            impl<'a> Parse for Le<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, punct0) = parser.parse_token::<lex::Punct>()?;
                    let (parser, punct1) = parser.parse_token::<lex::Punct>()?;
                    return if punct0.ch == Lexer::LT && punct1.ch == Lexer::EQ {
                        Ok((parser, Le {
                            punct0,
                            punct1
                        }))
                    } else {
                        Err(Error::UnexpectedToken(punct0.span.owned()))
                    }
                }
            }

            impl<'a> Parse for Ge<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, punct0) = parser.parse_token::<lex::Punct>()?;
                    let (parser, punct1) = parser.parse_token::<lex::Punct>()?;
                    return if punct0.ch == Lexer::GT && punct1.ch == Lexer::EQ {
                        Ok((parser, Ge {
                            punct0,
                            punct1
                        }))
                    } else {
                        Err(Error::UnexpectedToken(punct0.span.owned()))
                    }
                }
            }

            impl<'a> Parse for And<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, punct) = parser.parse_token::<lex::Punct>()?;
                    return if punct.ch == Lexer::AND {
                        Ok((parser, And {
                            punct
                        }))
                    } else {
                        Err(Error::UnexpectedToken(punct.span.owned()))
                    }
                }
            }

            impl<'a> Parse for AndAnd<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, punct0) = parser.parse_token::<lex::Punct>()?;
                    let (parser, punct1) = parser.parse_token::<lex::Punct>()?;
                    return if punct0.ch == Lexer::AND && punct1.ch == Lexer::AND {
                        Ok((parser, AndAnd {
                            punct0,
                            punct1
                        }))
                    } else {
                        Err(Error::UnexpectedToken(punct0.span.owned()))
                    }
                }
            }

            impl<'a> Parse for Or<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, punct) = parser.parse_token::<lex::Punct>()?;
                    return if punct.ch == Lexer::OR {
                        Ok((parser, Or {
                            punct
                        }))
                    } else {
                        Err(Error::UnexpectedToken(punct.span.owned()))
                    }
                }
            }

            impl<'a> Parse for OrOr<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, punct0) = parser.parse_token::<lex::Punct>()?;
                    let (parser, punct1) = parser.parse_token::<lex::Punct>()?;
                    return if punct0.ch == Lexer::OR && punct1.ch == Lexer::OR {
                        Ok((parser, OrOr {
                            punct0,
                            punct1
                        }))
                    } else {
                        Err(Error::UnexpectedToken(punct0.span.owned()))
                    }
                }
            }

            impl<'a> Parse for Xor<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, punct) = parser.parse_token::<lex::Punct>()?;
                    return if punct.ch == Lexer::XOR {
                        Ok((parser, Xor {
                            punct
                        }))
                    } else {
                        Err(Error::UnexpectedToken(punct.span.owned()))
                    }
                }
            }

            impl<'a> Parse for Eq<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, punct) = parser.parse_token::<lex::Punct>()?;
                    return if punct.ch == Lexer::EQ {
                        Ok((parser, Eq {
                            punct
                        }))
                    } else {
                        Err(Error::UnexpectedToken(punct.span.owned()))
                    }
                }
            }

            impl<'a> Parse for EqEq<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, punct0) = parser.parse_token::<lex::Punct>()?;
                    let (parser, punct1) = parser.parse_token::<lex::Punct>()?;
                    return if punct0.ch == Lexer::EQ && punct1.ch == Lexer::EQ {
                        Ok((parser, EqEq {
                            punct0,
                            punct1
                        }))
                    } else {
                        Err(Error::UnexpectedToken(punct0.span.owned()))
                    }
                }
            }

            impl<'a> Parse for Ne<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, punct0) = parser.parse_token::<lex::Punct>()?;
                    let (parser, punct1) = parser.parse_token::<lex::Punct>()?;
                    return if punct0.ch == Lexer::NEG && punct1.ch == Lexer::EQ {
                        Ok((parser, Ne {
                            punct0,
                            punct1
                        }))
                    } else {
                        Err(Error::UnexpectedToken(punct0.span.owned()))
                    }
                }
            }

            impl<'a> Parse for Comma<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, punct) = parser.parse_token::<lex::Punct>()?;
                    return if punct.ch == Lexer::COMMA {
                        Ok((parser, Comma {
                            punct
                        }))
                    } else {
                        Err(Error::UnexpectedToken(punct.span.owned()))
                    }
                }
            }

            impl<'a> Parse for Dot<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, punct) = parser.parse_token::<lex::Punct>()?;
                    return if punct.ch == Lexer::DOT {
                        Ok((parser, Dot {
                            punct
                        }))
                    } else {
                        Err(Error::UnexpectedToken(punct.span.owned()))
                    }
                }
            }

            impl<'a> Parse for If<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, text) = parser.parse_token::<lex::Text>()?;
                    return if text.span.value() == "if" {
                        Ok((parser, If {
                            text
                        }))
                    } else {
                        Err(Error::UnexpectedToken(text.span.owned()))
                    }
                }
            }

            impl<'a> Parse for Else<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, text) = parser.parse_token::<lex::Text>()?;
                    return if text.span.value() == "else" {
                        Ok((parser, Else {
                            text
                        }))
                    } else {
                        Err(Error::UnexpectedToken(text.span.owned()))
                    }
                }
            }

            impl<'a> Parse for For<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, text) = parser.parse_token::<lex::Text>()?;
                    return if text.span.value() == "for" {
                        Ok((parser, For {
                            text
                        }))
                    } else {
                        Err(Error::UnexpectedToken(text.span.owned()))
                    }
                }
            }

            impl<'a> Parse for In<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, text) = parser.parse_token::<lex::Text>()?;
                    return if text.span.value() == "in" {
                        Ok((parser, In {
                            text
                        }))
                    } else {
                        Err(Error::UnexpectedToken(text.span.owned()))
                    }
                }
            }

            impl<'a> Parse for Lit<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, lit_str) = parser.opt_parse::<LitStr>();
                    match lit_str {
                        Some(lit_str) => Ok((parser, Lit::Str(lit_str))),
                        None => {
                            let (parser, lit_int) = parser.opt_parse::<LitInt>();
                            match lit_int {
                                Some(lit_int) => Ok((parser, Lit::Int(lit_int))),
                                None => {
                                    let (parser, lit_bool) = parser.opt_parse::<LitBool>();
                                    match lit_bool {
                                        Some(lit_bool) => Ok((parser, Lit::Bool(lit_bool))),
                                        None => {
                                            let (parser, lit_float) = parser.parse::<LitFloat>()?;
                                            Ok((parser, Lit::Float(lit_float)))
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            impl<'a> Parse for LitStr<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, lit) = parser.parse_token::<lex::StrLiteral>()?;
                    Ok((parser, LitStr {
                        lit
                    }))
                }
            }

            impl<'a> Parse for LitInt<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, lit) = parser.parse_token::<lex::IntLiteral>()?;
                    let value = lit.span.value().parse::<usize>().unwrap();
                    Ok((parser, LitInt {
                        lit,
                        value
                    }))
                }
            }

            impl<'a> Parse for LitFloat<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, lit) = parser.parse_token::<lex::FloatLiteral>()?;
                    let value = lit.span.value().parse::<f64>().unwrap();
                    Ok((parser, LitFloat {
                        lit,
                        value
                    }))
                }
            }

            impl<'a> Parse for LitBool<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    let (parser, text) = parser.parse_token::<lex::Text>()?;
                    let value = match text.span.value() {
                        "true" => true,
                        "false" => false,
                        _ => return Err(Error::UnexpectedToken(text.span.owned()))
                    };
                    Ok((parser, LitBool {
                        text,
                        value
                    }))
                }
            }


            impl<'a> Parse for Empty<'a> {
                type Error = Error;
                type Token = Result<lex::Token<'a>, lex::Error>;

                fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                    Ok((parser, Empty {
                        _a: PhantomData
                    }))
                }
            }

        }
    }

    #[derive(Debug)]
    pub struct View<'a> {
        pub root: Option<NodeOrBlock<'a>>
    }
    #[derive(Debug)]
    pub enum NodeOrBlock<'a> {
        Node(Node<'a>),
        Block(Block<'a>)
    }
    #[derive(Debug)]
    pub enum Node<'a> {
        Text(TextNode<'a>),
        Tag(TagNode<'a>)
    }
    #[derive(Debug)]
    pub struct TextNode<'a> {
        pub text: Text<'a>
    }
    #[derive(Debug)]
    pub struct TagNode<'a> {
        pub lt: primitive::Lt<'a>,
        pub name: primitive::Ident<'a>,
        pub attributes: Vec<Attribute<'a>>,
        pub div: Option<primitive::Div<'a>>,
        pub gt: primitive::Gt<'a>,
        pub block: Option<TagBlock<'a>>
    }
    #[derive(Debug)]
    pub struct TagBlock<'a> {
        pub children: Vec<NodeOrBlock<'a>>,
        pub lt: primitive::Lt<'a>,
        pub div: primitive::Div<'a>,
        pub name: primitive::Ident<'a>,
        pub gt: primitive::Gt<'a>,
    }
    #[derive(Debug)]
    pub struct Attribute<'a> {
        pub name: primitive::Ident<'a>,
        pub eq: primitive::Eq<'a>,
        pub value: AttributeValue<'a>
    }
    #[derive(Debug)]
    pub enum AttributeValue<'a> {
        StrLit(primitive::LitStr<'a>),
        Block(Block<'a>)
    }
    #[derive(Debug)]
    pub struct Block<'a> {
        pub left: primitive::BraceLeft<'a>,
        pub expr: Box<Expr<'a>>,
        pub right: primitive::BraceRight<'a>
    }
    // Order of expr members is important
    // it is also the order in which expr a tried to parsed
    #[derive(Debug)]
    pub enum Expr<'a> {
        If(If<'a>),
        For(For<'a>),
        UnaryAp(UnaryAp<'a>),
        Lit(primitive::Lit<'a>),
        Var(Var<'a>),
        Node(Node<'a>),
        Empty(Empty<'a>),
        Group(Group<'a, Expr<'a>>),
        BinaryAp(BinaryAp<'a>),
        SelectorAp(SelectorAp<'a>),
        Ap(Ap<'a>),

    }
    #[derive(Debug)]
    pub struct Group<'a, E> {
        pub left: primitive::ParenLeft<'a>,
        pub expr: Box<E>,
        pub right: primitive::ParenRight<'a>
    }
    #[derive(Debug)]
    pub struct BinaryAp<'a> {
        pub typ: Type,
        pub left: Box<Expr<'a>>,
        pub right: BinaryApRight<'a>
    }
    #[derive(Debug)]
    pub struct BinaryApRight<'a> {
        pub op: BinaryOp<'a>,
        pub right: Box<Expr<'a>>
    }
    #[derive(Debug)]
    pub enum BinaryOp<'a> {
        Multiplied(primitive::Mul<'a>),
        Divided(primitive::Div<'a>),
        Plus(primitive::Add<'a>),
        Minus(primitive::Sub<'a>),
        Modulo(primitive::Mod<'a>),
        And(primitive::AndAnd<'a>),
        Or(primitive::OrOr<'a>),
        BitAnd(primitive::And<'a>),
        BitOr(primitive::Or<'a>),
        BitXor(primitive::Xor<'a>),
        Eq(primitive::EqEq<'a>),
        Ne(primitive::Ne<'a>),
        Le(primitive::Le<'a>),
        Ge(primitive::Ge<'a>),
        Lt(primitive::Lt<'a>),
        Gt(primitive::Gt<'a>)
    }
    #[derive(Debug)]
    pub struct UnaryAp<'a> {
        pub typ: Type,
        pub op: UnaryOp<'a>,
        pub right: Box<Expr<'a>>
    }
    #[derive(Debug)]
    pub enum UnaryOp<'a> {
        Neg(primitive::Sub<'a>),
        Not(primitive::Bang<'a>)
    }
    #[derive(Debug)]
    pub struct If<'a> {
        pub typ: Type,
        pub if0: primitive::If<'a>,
        pub condition: Box<Expr<'a>>,
        pub then_branch: Block<'a>,
        pub elseif_branches: Vec<(primitive::Else<'a>, primitive::If<'a>, Box<Expr<'a>>, Block<'a>)>,
        pub else_branch: (primitive::Else<'a>, Block<'a>)
    }
    #[derive(Debug)]
    pub struct For<'a> {
        pub typ: Type,
        pub for0: primitive::For<'a>,
        pub binding: Var<'a>,
        pub in0: primitive::In<'a>,
        pub expr: Box<Expr<'a>>,
        pub block: Block<'a>
    }
    #[derive(Debug)]
    pub struct Var<'a> {
        pub typ: Type,
        pub scope: Scope,
        pub name: primitive::Ident<'a>
    }
    #[derive(Debug)]
    pub struct Ap<'a> {
        pub typ: Type,
        pub expr: Box<Expr<'a>>,
        pub right: ApRight<'a>
    }
    #[derive(Debug)]
    pub struct ApRight<'a> {
        pub group: Group<'a, Punctuated<'a, Expr<'a>, primitive::Comma<'a>>>
    }
    #[derive(Debug)]
    pub struct Punctuated<'a, P, S> {
        pub expr: Box<P>,
        pub other: Option<(S, Box<Punctuated<'a, P, S>>)>,
        _a: PhantomData<&'a ()>
    }
    #[derive(Debug)]
    pub struct SelectorAp<'a> {
        pub typ: Type,
        pub expr: Box<Expr<'a>>,
        pub right: SelectorApRight<'a>
    }
    #[derive(Debug)]
    pub struct SelectorApRight<'a> {
        pub selector: SelectorOp<'a>
    }
    #[derive(Debug)]
    pub enum SelectorOp<'a> {
        Named(NamedSelector<'a>),
        Bracket(BracketSelector<'a>)
    }
    #[derive(Debug)]
    pub struct NamedSelector<'a> {
        pub dot: primitive::Dot<'a>,
        pub name: primitive::Ident<'a>
    }
    #[derive(Debug)]
    pub struct BracketSelector<'a> {
        pub left: primitive::BracketLeft<'a>,
        pub expr: Box<Expr<'a>>,
        pub right: primitive::BracketRight<'a>
    }

    pub mod scope {
        use crate::rex::parse::Var;
        use crate::View;

        #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        pub enum Scope {
            Global,
            Local
        }

        impl<'a> View<'a> {
            pub fn globals(&self) -> Vec<Var> {
                todo!()
            }
        }
    }

    pub mod typ {
        use crate::rex::parse::{AttributeValue, BinaryOp, Expr, Node, NodeOrBlock, SelectorOp, TagBlock, Var};
        use crate::rex::parse::primitive::Lit;
        use crate::rex::parse::scope::Scope;

        #[derive(Copy, Clone, Debug, PartialEq)]
        pub enum Type {
            Any,
            Function,
            Unit,
            Array,
            Object,
            String,
            Int,
            Float,
            Bool,
            HtmlElement
        }

        impl Type {

            #[inline]
            pub fn eq_or_any(&self, other: &Self) -> bool {
                *self == *other || *self == Type::Any || *other == Type::Any
            }

            #[inline]
            pub(crate) fn is_primitive(&self) -> bool {
                match self {
                    Type::Any => false,
                    Type::Function => false,
                    Type::Unit => false,
                    Type::Array => false,
                    Type::Object => false,
                    Type::String => true,
                    Type::Int => true,
                    Type::Float => true,
                    Type::Bool => true,
                    Type::HtmlElement => false
                }
            }
        }

        impl<'a> Expr<'a> {

            #[inline]
            pub fn typ(&self) -> Type {
                match self {
                    Expr::If(if0) => if0.typ,
                    Expr::For(for0) => for0.typ,
                    Expr::UnaryAp(un_ap) => un_ap.typ,
                    Expr::Lit(lit) => match lit {
                        Lit::Str(_) => Type::String,
                        Lit::Int(_) => Type::Int,
                        Lit::Float(_) => Type::Float,
                        Lit::Bool(_) => Type::Bool
                    },
                    Expr::Var(var) => var.typ,
                    Expr::Node(_) => Type::HtmlElement,
                    Expr::Empty(_) => Type::Unit,
                    Expr::Group(group) => group.expr.typ(),
                    Expr::BinaryAp(bin_ap) => bin_ap.typ,
                    Expr::SelectorAp(sel_ap) => sel_ap.typ,
                    Expr::Ap(ap) => ap.typ
                }
            }

            #[inline]
            pub fn infer(&mut self, typ: Type) {
                if typ == Type::Any {
                    return;
                }
                match self {
                    Expr::If(if0) => {
                        if0.typ = typ;
                        if0.then_branch.expr.infer(typ);
                        for (_, _, _, block) in &mut if0.elseif_branches {
                            block.expr.infer(typ);
                        }
                        if0.else_branch.1.expr.infer(typ);
                    },
                    Expr::For(for0) => {
                        for0.typ = typ;
                        for0.block.expr.infer(typ);
                    }
                    Expr::UnaryAp(un_ap) => {
                        un_ap.typ = typ;
                        un_ap.right.infer(typ);
                    }
                    Expr::Lit(_) =>
                        panic!("Can't infer type for literal!"),
                    Expr::Var(var) => var.typ = typ,
                    Expr::Node(_) => panic!("Can't infer type for node!"),
                    Expr::Empty(_) => panic!("Can't infer type for empty expression!"),
                    Expr::Group(group) => {
                        group.expr.infer(typ);
                    }
                    Expr::BinaryAp(bin_ap) => {
                        bin_ap.typ = typ;
                        bin_ap.left.infer(typ);
                        bin_ap.right.right.infer(typ);
                    }
                    Expr::SelectorAp(sel) => {
                        sel.typ = typ;
                    }
                    Expr::Ap(ap) => ap.typ = typ
                }
            }

            pub fn infer_scope_node(node: &mut Node, var: &Var, scope: Scope) {
                match node {
                    Node::Text(_) => {},
                    Node::Tag(tag) => {
                        for attr in &mut tag.attributes {
                            match &mut attr.value {
                                AttributeValue::StrLit(_) => {}
                                AttributeValue::Block(block) => block.expr.infer_scope(var, scope)
                            }
                        }
                        match &mut tag.block {
                            None => {}
                            Some(block) => {
                                for child in &mut block.children {
                                    match child {
                                        NodeOrBlock::Node(node) => Self::infer_scope_node(node, var, scope),
                                        NodeOrBlock::Block(block) => block.expr.infer_scope(var, scope)
                                    }
                                }
                            }
                        }
                    }
                }
            }

            #[inline]
            pub fn infer_scope(&mut self, var: &Var, scope: Scope) {
                match self {
                    Expr::If(if0) => {
                        if0.condition.infer_scope(var, scope);
                        if0.then_branch.expr.infer_scope(var, scope);
                        if0.else_branch.1.expr.infer_scope(var, scope);
                    },
                    Expr::For(for0) => {
                        for0.expr.infer_scope(var, scope);
                        for0.block.expr.infer_scope(var, scope);
                    }
                    Expr::UnaryAp(un_ap) => {
                        un_ap.right.infer_scope(var, scope);
                    }
                    Expr::Lit(_) => {},
                    Expr::Var(this) => {
                        if this.name == var.name {
                            this.scope = scope;
                        }
                    },
                    Expr::Node(node) => Self::infer_scope_node(node, var, scope),
                    Expr::Empty(_) => {},
                    Expr::Group(group) => {
                        group.expr.infer_scope(var, scope)
                    }
                    Expr::BinaryAp(bin_ap) => {
                        bin_ap.left.infer_scope(var, scope);
                        bin_ap.right.right.infer_scope(var, scope);
                    }
                    Expr::SelectorAp(sel) => {
                        sel.expr.infer_scope(var, scope);
                        match &mut sel.right.selector {
                            SelectorOp::Named(_) => {}
                            SelectorOp::Bracket(b) => b.expr.infer_scope(var, scope)
                        }
                    }
                    Expr::Ap(ap) => {
                        ap.expr.infer_scope(var, scope);
                        let mut punct = &mut ap.right.group.expr;
                        loop {
                            punct.expr.infer_scope(var, scope);
                            if let Some((_, other)) = &mut punct.other {
                                punct = other
                            } else {
                                break;
                            }
                        }
                    }
                }
            }
        }

        impl<'a> BinaryOp<'a> {
            #[inline]
            pub fn typ(&self, typ: Type) -> Option<Type> {
                if !typ.is_primitive() {
                    return None;
                }
                match self {
                    BinaryOp::Multiplied(_) => {
                        if typ == Type::String {
                            return None;
                        }
                        Some(typ)
                    },
                    BinaryOp::Divided(_) => {
                        if typ == Type::String {
                            return None;
                        }
                        Some(typ)
                    },
                    BinaryOp::Plus(_) => Some(typ),
                    BinaryOp::Minus(_) => {
                        if typ == Type::String {
                            return None;
                        }
                        Some(typ)
                    },
                    BinaryOp::Modulo(_) => {
                        if typ != Type::Bool && typ != Type::Int {
                            return None;
                        }
                        Some(Type::Int)
                    },
                    BinaryOp::And(_) => Some(Type::Bool),
                    BinaryOp::Or(_) => Some(Type::Bool),
                    BinaryOp::BitAnd(_) => {
                        if typ == Type::String {
                            return None;
                        }
                        Some(typ)
                    },
                    BinaryOp::BitOr(_) => {
                        if typ == Type::String {
                            return None;
                        }
                        Some(typ)
                    },
                    BinaryOp::BitXor(_) => {
                        if typ == Type::String {
                            return None;
                        }
                        Some(typ)
                    },
                    BinaryOp::Eq(_) => Some(Type::Bool),
                    BinaryOp::Ne(_) => Some(Type::Bool),
                    BinaryOp::Le(_) => Some(Type::Bool),
                    BinaryOp::Ge(_) => Some(Type::Bool),
                    BinaryOp::Lt(_) => Some(Type::Bool),
                    BinaryOp::Gt(_) => Some(Type::Bool)
                }
            }
        }
    }

    mod implementation {
        use crate::cursor::Cursor;
        use crate::parser::{Parse, Parser};
        use crate::rex::lex;
        use crate::rex::parse::{Ap, ApRight, Attribute, AttributeValue, Block, BracketSelector, Error, Expr, For, Group, If, BinaryAp, BinaryApRight, NamedSelector, Node, NodeOrBlock, primitive, BinaryOp, Punctuated, SelectorAp, SelectorApRight, SelectorOp, TagBlock, TagNode, TextNode, Var, View, UnaryAp, UnaryOp};
        use crate::rex::parse::scope::Scope;
        use crate::rex::parse::typ::Type;

        impl From<lex::Error> for Error {
            fn from(err: lex::Error) -> Self {
                Error::Lexer(err)
            }
        }

        impl<'a> Parse for View<'a> {
            type Error = Error;
            type Token = Result<lex::Token<'a>, lex::Error>;

            fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                let (parser, _) = parser.opt_parse_token::<lex::Whitespace>();
                let (parser, root) = parser.opt_parse::<NodeOrBlock>();
                let (mut parser, _) = parser.opt_parse_token::<lex::Whitespace>();
                return if !parser.finished() {
                    let next = parser.next().unwrap()?;
                    Err(Error::UnexpectedToken(next.span().owned()))
                } else {
                    Ok((parser, View {
                        root
                    }))
                }
            }
        }

        impl<'a> Parse for NodeOrBlock<'a> {
            type Error = Error;
            type Token = Result<lex::Token<'a>, lex::Error>;

            fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                let (parser, node) = parser.opt_parse::<Node>();
                match node {
                    Some(node) => Ok((parser, NodeOrBlock::Node(node))),
                    None => {
                        let (parser, block) = parser.parse::<Block>()?;
                        Ok((parser, NodeOrBlock::Block(block)))
                    }
                }
            }
        }

        impl<'a> Parse for Node<'a> {
            type Error = Error;
            type Token = Result<lex::Token<'a>, lex::Error>;

            fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                let (parser, tag) = parser.opt_parse::<TagNode>();
                match tag {
                    Some(tag) => Ok((parser, Node::Tag(tag))),
                    None => {
                        let (parser, text) = parser.parse::<TextNode>()?;
                        Ok((parser, Node::Text(text)))
                    }
                }
            }
        }

        impl<'a> Parse for TextNode<'a> {
            type Error = Error;
            type Token = Result<lex::Token<'a>, lex::Error>;

            fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                let (parser, text) = parser.parse_token::<lex::Text>()?;
                Ok((parser, TextNode {
                    text
                }))
            }
        }

        impl<'a> Parse for TagNode<'a> {
            type Error = Error;
            type Token = Result<lex::Token<'a>, lex::Error>;

            fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                let (parser, lt) = parser.parse::<primitive::Lt>()?;
                let (mut parser, name) = parser.parse::<primitive::Ident>()?;
                let mut attributes1 = Vec::new();
                let (parser, attributes) = loop {
                    let (parser1, _) = parser.opt_parse_token::<lex::Whitespace>();
                    let (parser1, attribute) = parser1.opt_parse::<Attribute>();
                    match attribute {
                        Some(attr) => {
                            attributes1.push(attr);
                            parser = parser1
                        },
                        None => break (parser1, attributes1)
                    }
                };
                let (parser, _) = parser.opt_parse_token::<lex::Whitespace>();
                let (parser, div) = parser.opt_parse::<primitive::Div>();
                let (parser, gt) = parser.parse::<primitive::Gt>()?;
                let (parser, _) = parser.opt_parse_token::<lex::Whitespace>();
                let (parser, block) = parser.opt_parse::<TagBlock>();
                Ok((parser, TagNode {
                    lt,
                    name,
                    attributes,
                    div,
                    gt,
                    block
                }))
            }
        }

        impl<'a> Parse for TagBlock<'a> {
            type Error = Error;
            type Token = Result<lex::Token<'a>, lex::Error>;

            fn parse<C: Cursor<Item=Self::Token>>(mut parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                let mut children1 = Vec::new();
                let (parser, children) = loop {
                    let (parser1, _) = parser.opt_parse_token::<lex::Whitespace>();
                    let (parser1, child) = parser1.opt_parse::<NodeOrBlock>();
                    match child {
                        Some(nb) => {
                            children1.push(nb);
                            parser = parser1;
                        },
                        None => break (parser1, children1)
                    }
                };
                let (parser, _) = parser.opt_parse_token::<lex::Whitespace>();
                let (parser, lt) = parser.parse::<primitive::Lt>()?;
                let (parser, div) = parser.parse::<primitive::Div>()?;
                let (parser, name) = parser.parse::<primitive::Ident>()?;
                let (parser, _) = parser.opt_parse_token::<lex::Whitespace>();
                let (parser, gt) = parser.parse::<primitive::Gt>()?;
                Ok((parser, TagBlock {
                    children,
                    lt,
                    div,
                    name,
                    gt
                }))
            }
        }

        impl<'a> Parse for Attribute<'a> {
            type Error = Error;
            type Token = Result<lex::Token<'a>, lex::Error>;

            fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                let (parser, name) = parser.parse::<primitive::Ident>()?;
                let (parser, _) = parser.opt_parse_token::<lex::Whitespace>();
                let (parser, eq) = parser.parse::<primitive::Eq>()?;
                let (parser, _) = parser.opt_parse_token::<lex::Whitespace>();
                let (parser, value) = parser.parse::<AttributeValue>()?;
                Ok((parser, Attribute {
                    name,
                    eq,
                    value
                }))
            }
        }

        impl<'a> Parse for AttributeValue<'a> {
            type Error = Error;
            type Token = Result<lex::Token<'a>, lex::Error>;

            fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                let (parser, lit) = parser.opt_parse::<primitive::LitStr>();
                match lit {
                    Some(lit) => Ok((parser, AttributeValue::StrLit(lit))),
                    None => {
                        let (parser, block) = parser.parse::<Block>()?;
                        Ok((parser, AttributeValue::Block(block)))
                    }
                }
            }
        }

        impl<'a> Parse for Block<'a> {
            type Error = Error;
            type Token = Result<lex::Token<'a>, lex::Error>;

            fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                let (parser, left) = parser.parse::<primitive::BraceLeft>()?;
                let (parser, _) = parser.opt_parse_token::<lex::Whitespace>();
                let (parser, expr) = parser.parse::<Expr>()?;
                let (parser, _) = parser.opt_parse_token::<lex::Whitespace>();
                let (parser, right) = parser.parse::<primitive::BraceRight>()?;
                Ok((parser, Block {
                    left,
                    expr: Box::new(expr),
                    right
                }))
            }
        }

        impl<'a> Parse for Expr<'a> {
            type Error = Error;
            type Token = Result<lex::Token<'a>, lex::Error>;

            fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                let (mut parser, left) = {
                    let (parser, if0) = parser.opt_parse::<If>();
                    match if0 {
                        Some(if0) => (parser,  Some(Expr::If(if0))),
                        None => {
                            let (parser, for0) = parser.opt_parse::<For>();
                            match for0 {
                                Some(for0) => {
                                    (parser, Some(Expr::For(for0)))
                                },
                                None => {
                                    let (parser, unary_ap) = parser.opt_parse::<UnaryAp>();
                                    match unary_ap {
                                        Some(unary_ap) => (parser, Some(Expr::UnaryAp(unary_ap))),
                                        None => {
                                            let (parser, lit) = parser.opt_parse::<primitive::Lit>();
                                            match lit {
                                                Some(lit) => (parser, Some(Expr::Lit(lit))),
                                                None => {
                                                    let (parser, group) = parser.opt_parse::<Group<Expr>>();
                                                    match group {
                                                        Some(group) => (parser, Some(Expr::Group(group))),
                                                        None => {
                                                            let (parser, var) = parser.opt_parse::<Var>();
                                                            match var {
                                                                Some(var) => (parser, Some(Expr::Var(var))),
                                                                None => {
                                                                    let (parser, node) = parser.opt_parse::<Node>();
                                                                    match node {
                                                                        Some(node) => (parser, Some(Expr::Node(node))),
                                                                        None => {
                                                                            let (parser, empty) = parser.opt_parse::<primitive::Empty>();
                                                                            match empty {
                                                                                Some(empty) => (parser, Some(Expr::Empty(empty))),
                                                                                None => (parser, None)
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                };
                // Expressions that have an expression on the left side (rules that are not regular on the right side)
                // SelectorAp :== Expr . Ident
                // InfixAp :== Expr PrimitiveOp Expr
                match left {
                    Some(mut left) => {
                        let (parser, left) = loop {
                            let (parser1, _) = parser.opt_parse_token::<lex::Whitespace>();
                            let (parser1, right) = parser1.opt_parse::<ApRight>();
                            match right {
                                Some(right) => {
                                    let left_typ = left.typ();
                                    if !left_typ.eq_or_any(&Type::Function) {
                                        return Err(Error::TypeError(left_typ, Type::Function))
                                    }
                                    left.infer(Type::Function);
                                    parser = parser1;
                                    left = Expr::Ap(Ap {
                                        typ: Type::Any,
                                        expr: Box::new(left),
                                        right
                                    });
                                },
                                None => {
                                    let (parser1, right) = parser1.opt_parse::<SelectorApRight>();
                                    match right {
                                        Some(right) => {
                                            let left_typ = left.typ();
                                            match right.selector {
                                                SelectorOp::Named(_) => {
                                                    if !left_typ.eq_or_any(&Type::Object) {
                                                        return Err(Error::TypeError(left_typ, Type::Object))
                                                    }
                                                    left.infer(Type::Object);
                                                }
                                                SelectorOp::Bracket(_) => {
                                                    if !left_typ.eq_or_any(&Type::Array) {
                                                        return Err(Error::TypeError(left_typ, Type::Array))
                                                    }
                                                    left.infer(Type::Array);
                                                }
                                            }

                                            parser = parser1;
                                            left = Expr::SelectorAp(SelectorAp {
                                                typ: Type::Any,
                                                expr: Box::new(left),
                                                right
                                            });
                                        },
                                        None => {
                                            let (parser1, right) = parser1.opt_parse::<BinaryApRight>();
                                            match right {
                                                Some(mut right) => {
                                                    let mut left_typ = left.typ();
                                                    let right_typ = right.right.typ();

                                                    if left_typ.eq_or_any(&right_typ) {
                                                        if left_typ == Type::Any {
                                                            left.infer(right_typ);
                                                            left_typ = right_typ;
                                                        }
                                                        if right_typ == Type::Any {
                                                            right.right.infer(left_typ);
                                                        }
                                                        if let Some(typ) = right.op.typ(left_typ) {
                                                            parser = parser1;
                                                            left = Expr::BinaryAp(BinaryAp {
                                                                typ,
                                                                left: Box::new(left),
                                                                right
                                                            });
                                                        } else {
                                                            return Err(Error::UnsupportedOperation);
                                                        }
                                                    } else {
                                                        return Err(Error::TypeError(left_typ, right_typ));
                                                    }
                                                },
                                                None => break (parser1, left)
                                            }
                                        }
                                    }
                                }
                            }
                        };
                        Ok((parser, left))
                    },
                    None => match parser.next() {
                        Some(next) => Err(Error::UnexpectedToken(next?.span().owned())),
                        None => Err(Error::UnexpectedEof)
                    }
                }
            }
        }

        impl<'a> Parse for If<'a> {
            type Error = Error;
            type Token = Result<lex::Token<'a>, lex::Error>;

            fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                let (parser, if0) = parser.parse::<primitive::If>()?;
                let (parser, _) = parser.opt_parse_token::<lex::Whitespace>();
                let (parser, condition) = parser.parse::<Expr>()?;
                let (parser, _) = parser.opt_parse_token::<lex::Whitespace>();
                let (mut parser, mut then_branch) = parser.parse::<Block>()?;

                let mut elseif_branches = Vec::new();
                let (parser, mut elseif_branches, mut else_branch) = loop {
                    let (parser1, _) = parser.opt_parse_token::<lex::Whitespace>();
                    let (parser1, else0) = parser1.parse::<primitive::Else>()?;
                    let (parser1, _) = parser1.opt_parse_token::<lex::Whitespace>();
                    let (parser1, if0) = parser1.opt_parse::<primitive::If>();
                    match if0 {
                        Some(if0) => {
                            let (parser1, _) = parser1.opt_parse_token::<lex::Whitespace>();
                            let (parser1, expr) = parser1.parse::<Expr>()?;
                            let (parser1, _) = parser1.opt_parse_token::<lex::Whitespace>();
                            let (parser1, block) = parser1.parse::<Block>()?;
                            elseif_branches.push((else0, if0, Box::new(expr), block));
                            parser = parser1;
                        },
                        None => {
                            let (parser1, _) = parser1.opt_parse_token::<lex::Whitespace>();
                            let (parser1, block) = parser1.parse::<Block>()?;
                            break (parser1, elseif_branches, (else0, block))
                        }
                    }
                };

                // Type check and inference
                let mut any_expr_mut = Vec::with_capacity(2 + elseif_branches.len());
                any_expr_mut.push(&mut then_branch.expr);
                any_expr_mut.push(&mut else_branch.1.expr);
                for (_, _, _, block) in &mut elseif_branches { any_expr_mut.push(&mut block.expr); }
                let mut typ = Type::Any;
                for expr in &any_expr_mut {
                    let typ1 = expr.typ();
                    if !typ.eq_or_any(&typ1) {
                        return Err(Error::TypeError(typ, typ1));
                    }
                    if typ == Type::Any && typ1 != Type::Any {
                        typ = typ1;
                    }
                }
                for expr in any_expr_mut {
                    if expr.typ() == Type::Any {
                        expr.infer(typ);
                    }
                }

                Ok((parser, If {
                    typ,
                    if0,
                    condition: Box::new(condition),
                    then_branch,
                    elseif_branches,
                    else_branch
                }))
            }
        }

        impl<'a> Parse for For<'a> {
            type Error = Error;
            type Token = Result<lex::Token<'a>, lex::Error>;

            fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                let (parser, for0) = parser.parse::<primitive::For>()?;
                let (parser, _) = parser.opt_parse_token::<lex::Whitespace>();
                let (parser, mut binding) = parser.parse::<Var>()?;
                let (parser, _) = parser.opt_parse_token::<lex::Whitespace>();
                let (parser, in0) = parser.parse::<primitive::In>()?;
                let (parser, _) = parser.opt_parse_token::<lex::Whitespace>();
                let (parser, expr) = parser.parse::<Expr>()?;
                let (parser, _) = parser.opt_parse_token::<lex::Whitespace>();
                let (parser, mut block) = parser.parse::<Block>()?;
                binding.scope = Scope::Local;
                block.expr.infer_scope(&binding, binding.scope);
                Ok((parser, For {
                    typ: expr.typ(),
                    for0,
                    binding,
                    in0,
                    expr: Box::new(expr),
                    block
                }))
            }
        }

        impl<'a, E> Parse for Group<'a, E> where
            E: Parse<
                Error=Error,
                Token=Result<lex::Token<'a>, lex::Error>
            >
        {
            type Error = Error;
            type Token = Result<lex::Token<'a>, lex::Error>;

            fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                let (parser, left) = parser.parse::<primitive::ParenLeft>()?;
                let (parser, _) = parser.opt_parse_token::<lex::Whitespace>();
                let (parser, expr) = parser.parse::<E>()?;
                let (parser, _) = parser.opt_parse_token::<lex::Whitespace>();
                let (parser, right) = parser.parse::<primitive::ParenRight>()?;
                Ok((parser, Group {
                    left,
                    expr: Box::new(expr),
                    right
                }))
            }
        }

        impl<'a> Parse for BinaryOp<'a> {
            type Error = Error;
            type Token = Result<lex::Token<'a>, lex::Error>;

            fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                let (parser, mul) = parser.opt_parse::<primitive::Mul>();
                match mul {
                    Some(mul) => Ok((parser, BinaryOp::Multiplied(mul))),
                    None => {
                        let (parser, div) = parser.opt_parse::<primitive::Div>();
                        match div {
                            Some(div) => Ok((parser, BinaryOp::Divided(div))),
                            None => {
                                let (parser, add) = parser.opt_parse::<primitive::Add>();
                                match add {
                                    Some(add) => Ok((parser, BinaryOp::Plus(add))),
                                    None => {
                                        let (parser, sub) = parser.opt_parse::<primitive::Sub>();
                                        match sub {
                                            Some(sub) => Ok((parser, BinaryOp::Minus(sub))),
                                            None => {
                                                let (parser, mod0) = parser.opt_parse::<primitive::Mod>();
                                                match mod0 {
                                                    Some(mod0) => Ok((parser, BinaryOp::Modulo(mod0))),
                                                    None => {
                                                        let (parser, and) = parser.opt_parse::<primitive::AndAnd>();
                                                        match and {
                                                            Some(and) => Ok((parser, BinaryOp::And(and))),
                                                            None => {
                                                                let (parser, or) = parser.opt_parse::<primitive::OrOr>();
                                                                match or {
                                                                    Some(or) => Ok((parser, BinaryOp::Or(or))),
                                                                    None => {
                                                                        let (parser, bit_and) = parser.opt_parse::<primitive::And>();
                                                                        match bit_and {
                                                                            Some(bit_and) => Ok((parser, BinaryOp::BitAnd(bit_and))),
                                                                            None => {
                                                                                let (parser, bit_or) = parser.opt_parse::<primitive::Or>();
                                                                                match bit_or {
                                                                                    Some(bit_or) => Ok((parser, BinaryOp::BitOr(bit_or))),
                                                                                    None => {
                                                                                        let (parser, bit_xor) = parser.opt_parse::<primitive::Xor>();
                                                                                        match bit_xor {
                                                                                            Some(bit_xor) => Ok((parser, BinaryOp::BitXor(bit_xor))),
                                                                                            None => {
                                                                                                let (parser, eq) = parser.opt_parse::<primitive::EqEq>();
                                                                                                match eq {
                                                                                                    Some(eq) => Ok((parser, BinaryOp::Eq(eq))),
                                                                                                    None => {
                                                                                                        let (parser, ne) = parser.opt_parse::<primitive::Ne>();
                                                                                                        match ne {
                                                                                                            Some(ne) => Ok((parser, BinaryOp::Ne(ne))),
                                                                                                            None => {
                                                                                                                let (parser, le) = parser.opt_parse::<primitive::Le>();
                                                                                                                match le {
                                                                                                                    Some(le) => Ok((parser, BinaryOp::Le(le))),
                                                                                                                    None => {
                                                                                                                        let (parser, ge) = parser.opt_parse::<primitive::Ge>();
                                                                                                                        match ge {
                                                                                                                            Some(ge) => Ok((parser, BinaryOp::Ge(ge))),
                                                                                                                            None => {
                                                                                                                                let (parser, lt) = parser.opt_parse::<primitive::Lt>();
                                                                                                                                match lt {
                                                                                                                                    Some(lt) => Ok((parser, BinaryOp::Lt(lt))),
                                                                                                                                    None => {
                                                                                                                                        let (parser, gt) = parser.parse::<primitive::Gt>()?;
                                                                                                                                        Ok((parser, BinaryOp::Gt(gt)))
                                                                                                                                    }
                                                                                                                                }
                                                                                                                            }
                                                                                                                        }
                                                                                                                    }
                                                                                                                }
                                                                                                            }
                                                                                                        }
                                                                                                    }
                                                                                                }
                                                                                            }
                                                                                        }
                                                                                    }
                                                                                }
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        impl<'a> Parse for BinaryApRight<'a> {
            type Error = Error;
            type Token = Result<lex::Token<'a>, lex::Error>;

            fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                let (parser, primitive_op) = parser.parse::<BinaryOp>()?;
                let (parser, _) = parser.opt_parse_token::<lex::Whitespace>();
                let (parser, right) = parser.parse::<Expr>()?;

                Ok((parser, BinaryApRight {
                    op: primitive_op,
                    right: Box::new(right)
                }))
            }
        }

        impl<'a> Parse for UnaryAp<'a> {
            type Error = Error;
            type Token = Result<lex::Token<'a>, lex::Error>;

            fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                let (parser, op) = parser.parse::<UnaryOp>()?;
                let (parser, _) = parser.opt_parse_token::<lex::Whitespace>();
                // Parse expressions that are directly next to the unary op to lift them up in the parse tree
                let (parser, lit) = parser.opt_parse::<primitive::LitInt>();
                let (parser, right) = match lit {
                    Some(lit) => (parser, Expr::Lit(primitive::Lit::Int(lit))),
                    None => {
                        let (parser, lit) = parser.opt_parse::<primitive::LitFloat>();
                        match lit {
                            Some(lit) => (parser, Expr::Lit(primitive::Lit::Float(lit))),
                            None => {
                                let (parser, lit) = parser.opt_parse::<primitive::LitBool>();
                                match lit {
                                    Some(lit) => (parser, Expr::Lit(primitive::Lit::Bool(lit))),
                                    None => {
                                        let (parser, var) = parser.opt_parse::<Var>();
                                        match var {
                                            Some(var) => (parser, Expr::Var(var)),
                                            None => {
                                                let (parser, group) = parser.opt_parse::<Group<Expr>>();
                                                match group {
                                                    Some(group) => (parser, Expr::Group(group)),
                                                    None => {
                                                        let (parser, expr) = parser.parse::<Expr>()?;
                                                        (parser, expr)
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                };
                Ok((parser, UnaryAp {
                    typ: right.typ(),
                    op,
                    right: Box::new(right)
                }))
            }
        }

        impl<'a> Parse for UnaryOp<'a> {
            type Error = Error;
            type Token = Result<lex::Token<'a>, lex::Error>;

            fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                let (parser, sub) = parser.opt_parse::<primitive::Sub>();
                match sub {
                    Some(sub) => Ok((parser, UnaryOp::Neg(sub))),
                    None => {
                        let (parser, bang) = parser.parse::<primitive::Bang>()?;
                        Ok((parser, UnaryOp::Not(bang)))
                    }
                }
            }
        }

        impl<'a> Parse for SelectorApRight<'a> {
            type Error = Error;
            type Token = Result<lex::Token<'a>, lex::Error>;

            fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                let (parser, selector_op) = parser.parse::<SelectorOp>()?;
                Ok((parser, SelectorApRight {
                    selector: selector_op
                }))
            }
        }

        impl<'a> Parse for SelectorOp<'a> {
            type Error = Error;
            type Token = Result<lex::Token<'a>, lex::Error>;

            fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                let (parser, named) = parser.opt_parse::<NamedSelector>();
                match named {
                    Some(named) => Ok((parser, SelectorOp::Named(named))),
                    None => {
                        let (parser, bracket) = parser.parse::<BracketSelector>()?;
                        Ok((parser, SelectorOp::Bracket(bracket)))
                    }
                }
            }
        }

        impl<'a> Parse for NamedSelector<'a> {
            type Error = Error;
            type Token = Result<lex::Token<'a>, lex::Error>;

            fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                let (parser, _) = parser.opt_parse_token::<lex::Whitespace>();
                let (parser, dot) = parser.parse::<primitive::Dot>()?;
                let (parser, _) = parser.opt_parse_token::<lex::Whitespace>();
                let (parser, name) = parser.parse::<primitive::Ident>()?;

                Ok((parser, NamedSelector {
                    dot,
                    name
                }))
            }
        }

        impl<'a> Parse for BracketSelector<'a> {
            type Error = Error;
            type Token = Result<lex::Token<'a>, lex::Error>;

            fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                let (parser, left) = parser.parse::<primitive::BracketLeft>()?;
                let (parser, _) = parser.opt_parse_token::<lex::Whitespace>();
                let (parser, expr) = parser.parse::<Expr>()?;
                let (parser, _) = parser.opt_parse_token::<lex::Whitespace>();
                let (parser, right) = parser.parse::<primitive::BracketRight>()?;

                Ok((parser, BracketSelector {
                    left,
                    expr: Box::new(expr),
                    right
                }))
            }
        }

        impl<'a> Parse for ApRight<'a> {
            type Error = Error;
            type Token = Result<lex::Token<'a>, lex::Error>;

            fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                let (parser, group) = parser.parse::<Group<Punctuated<Expr, primitive::Comma>>>()?;
                Ok((parser, ApRight {
                    group
                }))
            }
        }

        impl<'a, P, S> Parse for Punctuated<'a, P, S> where
            P: Parse<Error=Error, Token=Result<lex::Token<'a>, lex::Error>>,
            S: Parse<Error=Error, Token=Result<lex::Token<'a>, lex::Error>>,
        {
            type Error = Error;
            type Token = Result<lex::Token<'a>, lex::Error>;

            fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                let (parser, _) = parser.opt_parse_token::<lex::Whitespace>();
                let (parser, expr) = parser.parse::<P>()?;
                let (parser, sep) = parser.opt_parse::<S>();
                let (parser, other) = match sep {
                    Some(sep) => {
                        let (parser, other) = parser.parse::<Punctuated<P, S>>()?;
                        (parser, Some((sep, Box::new(other))))
                    },
                    None => (parser, None)
                };
                Ok((parser, Punctuated {
                    expr: Box::new(expr),
                    other,
                    _a: Default::default()
                }))
            }
        }

        impl<'a> Parse for Var<'a> {
            type Error = Error;
            type Token = Result<lex::Token<'a>, lex::Error>;

            fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error> {
                let (parser, name) = parser.parse::<primitive::Ident>()?;
                Ok((parser, Var {
                    typ: Type::Any,
                    scope: Scope::Global,
                    name
                }))
            }
        }
    }
}

