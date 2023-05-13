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

    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
    use crate::util::SpanOwned;

    #[derive(Debug)]
    pub enum Error {
        Lexer(lex::Error),
        UnexpectedToken(SpanOwned),
        UnexpectedEof,
        TypeError(Type, Type),
        UnsupportedOperation,
        DuplicateVar(String)
    }

    pub mod primitive {
        use std::marker::PhantomData;
        use crate::rex::lex::{FloatLiteral, IntLiteral, Punct, StrLiteral, Text};

        #[derive(Debug, Eq, Clone, Hash)]
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
    // it is also the order in which expressions are tried to be parsed
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
    #[derive(Clone, Debug)]
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
        pub _a: PhantomData<&'a ()>
    }
    #[derive(Debug)]
    pub struct PunctuatedIter<'b, 'a, P, S> {
        inner: Option<&'b Punctuated<'a, P, S>>
    }
    #[derive(Debug)]
    pub struct PunctuatedIterMut<'b, 'a, P, S> {
        inner: Option<&'b mut Punctuated<'a, P, S>>
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
        use std::collections::HashSet;
        use crate::rex::parse::{AttributeValue, Expr, Node, NodeOrBlock, SelectorOp, Var};
        use crate::View;

        #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
        pub enum Scope {
            Global,
            Local
        }
    }

    pub mod typ {
        use std::cmp::Ordering;
        use std::collections::{BTreeMap, HashMap, HashSet};
        use std::hash::{Hash, Hasher};
        use std::ops::{Add, Deref};
        use crate::rex::parse::{Ap, ApRight, AttributeValue, BinaryAp, BinaryApRight, BinaryOp, Block, Error, Expr, For, If, Node, NodeOrBlock, primitive, Punctuated, SelectorAp, SelectorApRight, SelectorOp, UnaryAp, UnaryOp, Var};
        use crate::rex::parse::primitive::{Comma, Lit};
        use crate::rex::parse::scope::Scope;
        use crate::rex::View;
        use crate::util::HashMultimap;

        #[derive(Clone, Debug, Hash)]
        pub enum AbstractType {
            Any,
            // String, Number, IntLike, Float, Int, Bool
            AddAndEq,
            // IntLike, Int, Bool, Float
            Number,
            // Int, Bool
            IntLike
        }
        #[derive(Clone, Debug, PartialEq, Hash)]
        pub enum PrimitiveType {
            Function(Vec<Type>),
            Unit,
            Array(Box<Type>),
            Object(BTreeMap<String, Type>),
            String,
            Int,
            Float,
            Bool,
            Node
        }

        #[derive(Clone, Debug, Hash)]
        pub enum Type {
            Abstract(AbstractType),
            Primitive(PrimitiveType)
        }

        pub struct Typed;

        impl Typed {

            fn min(vars: &HashSet<&mut Var>) -> Type {
                let mut iter = vars.iter();
                let mut min = iter.next().unwrap().typ.clone();
                for var in iter {
                    if var.typ < min {
                        min = var.typ.clone();
                    }
                }
                min
            }

            pub fn un_ap<'a>(op: UnaryOp<'a>, mut right: Expr<'a>) -> Result<UnaryAp<'a>, Error> {
                let mut right_typ = right.typ();
                let typ = match op {
                    UnaryOp::Neg(_) => {
                        if right_typ > &Type::INT_LIKE {
                            right.infer(&Type::INT_LIKE);
                            Type::INT_LIKE
                        } else if right_typ <= &Type::INT_LIKE {
                            right_typ.clone()
                        } else {
                            return Err(Error::TypeError(right_typ.clone(), Type::INT_LIKE))
                        }
                    }
                    UnaryOp::Not(_) => {
                        if right_typ != &Type::BOOL {
                            return Err(Error::TypeError(right_typ.clone(), Type::BOOL))
                        }
                        Type::BOOL
                    }
                };
                Ok(UnaryAp {
                    typ,
                    op,
                    right: Box::new(right)
                })
            }

            pub fn sel_ap<'a>(mut left: Expr<'a>, right: SelectorApRight<'a>) -> Result<SelectorAp<'a>, Error> {
                /*
                 * Type inference of left type
                */
                let left_typ = left.typ();
                let typ = match &right.selector {
                    SelectorOp::Named(named) => {
                        let sel_name = named.name.text.span.value();
                        let typ = Type::object([(sel_name.to_string(), Type::ANY)]);
                        if left_typ == &Type::ANY {
                            left.infer(&typ);
                            Type::ANY
                        } else if left_typ.is_object() {
                            left.infer(&(left_typ + &typ)?);
                            left.typ().object_inner(sel_name).unwrap().clone()
                        }
                        else {
                            return Err(Error::TypeError(left_typ.clone(), typ))
                        }
                    }
                    SelectorOp::Bracket(_) => {
                        let typ = Type::array(Type::ANY);
                        if left_typ == &Type::ANY {
                            left.infer(&typ);
                            Type::ANY
                        } else if left_typ.is_array() {
                            left_typ.array_inner().clone()
                        } else {
                            return Err(Error::TypeError(left_typ.clone(), typ))
                        }
                    }
                };
                Ok(SelectorAp {
                    typ,
                    expr: Box::new(left),
                    right
                })
            }

            pub fn bin_ap<'a>(mut left: Expr<'a>, mut right: BinaryApRight<'a>) -> Result<BinaryAp<'a>, Error> {
                let mut left_typ = &left.typ().clone();
                let mut right_typ = &right.right.typ().clone();

                /*
                 * The following part is the most complex part: Type inference for a binary-ap
                 * This includes a lot of different cases for different ops and different present types.
                 */

                // First of all a binary op can only be made on the following types
                fn binop_type(typ: &Type) -> bool {
                    typ <= &Type::ADD_AND_EQ || typ == &Type::ANY
                }

                if !binop_type(left_typ) || !binop_type(right_typ) {
                    return Err(Error::UnsupportedOperation);
                }

                if left_typ < right_typ {
                    right.right.infer(left_typ);
                    right_typ = left_typ;
                }
                if right_typ < left_typ {
                    left.infer(right_typ);
                    left_typ = right_typ;
                }

                // MANUAL:
                //      ls & rs         :means both have the same value
                //      |               :means or
                //      &               :means and
                //      ==, !=          :means equal, not equal
                //      =>, <=>, !=>    :mean typical implications
                //      ls == {...}     :means ls must be one of the values in the set
                // Assert ls and rs are <=number | string | any
                // but:
                // 1.   ls == any <=> rs == any
                // 2.   ls == number => rs == number | rs == string
                // 3.   ls == int_like => rs == int_like | rs == string
                // 4.   ls == {float, int, bool} => !rs.is_abstract()
                //      ls == string => rs != any      :because string < any
                //
                // Lemma:
                //      ls & rs != string & ls != any
                //  =>
                //      ls & rs == {number, int_like} | (ls == {float, int, bool} & rs == {float, int, bool})
                // Proof:
                //      ls & rs != string & ls != any
                //  with all left cases for ls and rs and 1. =>
                //      ls == {number, int_like, float, int, bool} & rs == {number, int_like, float, int, bool}
                //  with 2. and 3. and because rs != string =>
                //      (ls == number & rs == number) | (ls == int_like & rs == int_like) | (ls == {float, int, bool} & rs == {float, int, bool})
                //  <=>
                //      ls & rs == {number, int_like} | (ls == {float, int, bool} & rs == {float, int, bool}) qed.

                // Calculate the result type of the binary op
                // And if the binary op can be calculated
                let res_typ = match &right.op {
                    BinaryOp::Multiplied(_) | BinaryOp::Divided(_) | BinaryOp::Minus(_) => {
                        if left_typ == &Type::STRING || right_typ == &Type::STRING {
                            None
                        } else if left_typ == &Type::ANY /* && right_typ == &Type::ANY can be omitted. This should be always true */ {
                            // Both operands are any
                            left.infer(&Type::NUMBER);
                            right.right.infer(&Type::NUMBER);
                            Some(Type::NUMBER)
                        } else { // ls & rs <= number
                            // calculate the winning number type float < int < bool < int_like < number
                            Some(Type::number_min(left_typ, right_typ).clone())
                        }
                    },
                    BinaryOp::Plus(_) => {
                        if left_typ == &Type::STRING || right_typ == &Type::STRING {
                            Some(Type::STRING)
                        } else if left_typ == &Type::ANY /* && right_typ == &Type::ANY can be omitted. This should be always true */ {
                            left.infer(&Type::ADD_AND_EQ);
                            right.right.infer(&Type::ADD_AND_EQ);
                            Some(Type::ADD_AND_EQ)
                        } else {
                            // calculate the winning number type float < int < bool < int_like < number
                            Some(Type::number_min(left_typ, right_typ).clone())
                        }
                    },
                    BinaryOp::Modulo(_) => {
                        if left_typ == &Type::ANY /* && right_typ == &Type::ANY can be omitted. This should be always true */ {
                            left.infer(&Type::INT_LIKE);
                            right.right.infer(&Type::INT_LIKE);
                            Some(Type::INT_LIKE)
                        } else if !(left_typ <= &Type::INT_LIKE) || !(right_typ <= &Type::INT_LIKE) {
                            None
                        } else {
                            Some(Type::INT)
                        }
                    },
                    BinaryOp::And(_) | BinaryOp::Or(_) => {
                        if left_typ == &Type::ANY /* && right_typ == &Type::ANY can be omitted. This should be always true */ {
                            left.infer(& Type::BOOL);
                            right.right.infer(&Type::BOOL);
                            Some(Type::BOOL)
                        } else if left_typ != &Type::BOOL || right_typ != &Type::BOOL {
                            //Both sides must be bool or any
                            None
                        } else {
                            Some(Type::BOOL)
                        }
                    },
                    BinaryOp::BitAnd(_) | BinaryOp::BitOr(_) | BinaryOp::BitXor(_) => {
                        if left_typ == &Type::ANY /* && right_typ == &Type::Any can be omitted this should be always true */  {
                            left.infer(&Type::INT_LIKE);
                            right.right.infer(&Type::INT_LIKE);
                            Some(Type::INT_LIKE)
                        } else if !(left_typ <= &Type::INT_LIKE) || !(right_typ <= &Type::INT_LIKE)  {
                            None
                        } else {
                            Some(Type::number_min(left_typ, right_typ).clone())
                        }
                    },
                    // Current restriction can only compare primitives (strings, numbers, int_likes, floats, ints, bools)
                    BinaryOp::Eq(_) |
                    BinaryOp::Ne(_) => {
                        if left_typ == &Type::ANY /* && right_typ == &Type::Any can be omitted this should be always true */  {
                            left.infer(&Type::ADD_AND_EQ);
                            right.right.infer(&Type::ADD_AND_EQ);
                            Some(Type::ADD_AND_EQ)
                        } else if !(left_typ <= &Type::ADD_AND_EQ) || !(right_typ <= &Type::ADD_AND_EQ) {
                            None
                        } else {
                            Some(Type::BOOL)
                        }
                    },
                    BinaryOp::Le(_) |
                    BinaryOp::Ge(_) |
                    BinaryOp::Lt(_) |
                    BinaryOp::Gt(_) => {
                        if left_typ == &Type::ANY /* && right_typ == &Type::Any can be omitted this should be always true */ {
                            left.infer(&Type::NUMBER);
                            right.right.infer(&Type::NUMBER);
                            Some(Type::NUMBER)
                        } else if !(left_typ <= &Type::NUMBER) || !(right_typ <= &Type::NUMBER) {
                            None
                        } else {
                            Some(Type::BOOL)
                        }
                    },
                };
                let typ = match res_typ {
                    Some(t) => t,
                    None => return Err(Error::UnsupportedOperation)
                };
                Ok(BinaryAp {
                    typ,
                    left: Box::new(left),
                    right
                })
            }

            pub fn ap<'a>(mut left: Expr<'a>, right: ApRight<'a>) -> Result<Ap<'a>, Error> {
                let left_typ = left.typ();
                let mut arg_types: Vec<Type> = right.group.expr.args().map(|e| e.typ().clone()).collect();
                // push return type
                arg_types.push(Type::ANY);
                let f_typ = Type::function(arg_types);
                let typ = {
                    if left_typ == &Type::ANY {
                        left.infer(&f_typ);
                        Type::ANY
                    } else if left_typ.is_function() {
                        left.infer(&(left_typ + &f_typ)?);
                        left.typ().target_typ().clone()
                    } else {
                        return Err(Error::TypeError(left_typ.clone(), f_typ.clone()))
                    }
                };
                left.infer(&f_typ);
                Ok(Ap {
                    typ,
                    expr: Box::new(left),
                    right
                })
            }

            pub fn r#if<'a>(
                if0: primitive::If<'a>,
                mut condition: Expr<'a>,
                mut then_branch: Block<'a>,
                mut elseif_branches: Vec<(primitive::Else<'a>, primitive::If<'a>, Box<Expr<'a>>, Block<'a>)>,
                mut else_branch: (primitive::Else<'a>, Block<'a>)
            ) -> Result<If<'a>, Error> {
                // Type check and type inference
                let mut any_expr_mut = Vec::with_capacity(2 + elseif_branches.len());
                any_expr_mut.push(&mut then_branch.expr);
                any_expr_mut.push(&mut else_branch.1.expr);
                for (_, _, _, block) in &mut elseif_branches { any_expr_mut.push(&mut block.expr); }
                let mut typ = Type::ANY;
                for expr in &any_expr_mut {
                    let expr_typ = expr.typ();
                    if !(expr_typ <= &typ) && !(expr_typ > &typ) {
                        return Err(Error::TypeError(expr_typ.clone(), typ));
                    }
                    if expr_typ < &typ {
                        typ = expr_typ.clone()
                    }

                }
                for expr in any_expr_mut {
                    if expr.typ() > &typ {
                        expr.infer(&typ);
                    }
                }

                // We must determine the minimum type for each global var and set all same vars to the same type
                let mut all = condition.globals_mut();
                all.union(then_branch.expr.globals_mut());

                let else_if_globals: Vec<(HashMultimap<String, &mut Var<'a>>, HashMultimap<String, &mut Var<'a>>)> = elseif_branches.iter_mut()
                    .map(|(_, _, cond, block)|
                        (cond.globals_mut(), block.expr.globals_mut())
                    )
                    .collect();

                for (cond_gl, block_gl) in else_if_globals {
                    all.union(cond_gl);
                    all.union(block_gl);
                }
                all.union(else_branch.1.expr.globals_mut());

                for (_, vars) in all {
                    if vars.len() <= 1 { continue; }
                    let min_type = Self::min(&vars);
                    for var in vars {
                        var.typ = min_type.clone();
                    }
                }

                Ok(If {
                    typ: Type::ANY,
                    if0,
                    condition: Box::new(condition),
                    then_branch,
                    elseif_branches,
                    else_branch
                })
            }

            pub fn r#for<'a>(for0: primitive::For<'a>, mut binding: Var<'a>, in0: primitive::In<'a>, mut expr: Expr<'a>, mut block: Block<'a>) -> Result<For<'a>, Error> {
                // Finds all occurrences of this binding in the block expression and find its type
                let binding_name = binding.name.text.span.value();
                block.expr.check_var_duplicated(binding_name)?;
                if let Some(binding_typ) = block.expr.find_var_typ_and_infer(binding_name)? {
                    binding.typ = binding_typ.clone();
                }
                let required_expr_typ = Type::array(binding.typ.clone());
                let expr_typ = expr.typ();
                if !(expr_typ <= &required_expr_typ) && !(expr_typ > &required_expr_typ) {
                    return Err(Error::TypeError(expr_typ.clone(), required_expr_typ));
                }
                if expr_typ < &required_expr_typ {
                    let inner = expr_typ.array_inner();
                    binding.typ = inner.clone();
                    block.expr.infer_var(binding_name, inner);
                } else if expr_typ > &required_expr_typ {
                    expr.infer(&required_expr_typ);
                }
                let typ = Type::array(block.expr.typ().clone());

                // Infer minimum type for global vars
                let mut all = expr.globals_mut();
                all.union(block.expr.globals_mut());

                for (_, vars) in all {
                    if vars.len() <= 1 { continue; }
                    let typ = Self::min(&vars);
                    for var in vars {
                        var.typ = typ.clone();
                    }
                }

                Ok(For {
                    typ,
                    for0,
                    binding,
                    in0,
                    expr: Box::new(expr),
                    block
                })
            }
        }

        impl Type {
            pub const ANY: Type = Type::Abstract(AbstractType::Any);
            pub const NUMBER: Type = Type::Abstract(AbstractType::Number);
            pub const INT_LIKE: Type = Type::Abstract(AbstractType::IntLike);
            pub const ADD_AND_EQ: Type = Type::Abstract(AbstractType::AddAndEq);

            pub const FLOAT: Type = Type::Primitive(PrimitiveType::Float);
            pub const INT: Type = Type::Primitive(PrimitiveType::Int);
            pub const BOOL: Type = Type::Primitive(PrimitiveType::Bool);
            pub const STRING: Type = Type::Primitive(PrimitiveType::String);

            pub const NODE: Type = Type::Primitive(PrimitiveType::Node);
            pub const UNIT: Type = Type::Primitive(PrimitiveType::Unit);

            #[inline]
            pub fn array(typ: Type) -> Type {
                Type::Primitive(PrimitiveType::Array(Box::new(typ)))
            }

            #[inline]
            pub fn object<const N: usize>(attributes: [(String, Type);N]) -> Type {
                Type::Primitive(PrimitiveType::Object(BTreeMap::from(attributes)))
            }

            #[inline]
            pub fn function(arg_types: Vec<Type>) -> Type {
                debug_assert!(arg_types.len() > 0);
                Type::Primitive(PrimitiveType::Function(arg_types))
            }

            #[inline]
            pub fn is_abstract(&self) -> bool {
                match self {
                    Type::Abstract(_) => true,
                    _ => false
                }
            }

            #[inline]
            pub fn is_array(&self) -> bool {
                match self {
                    Type::Primitive(PrimitiveType::Array(_)) => true,
                    _ => false
                }
            }

            #[inline]
            pub fn is_object(&self) -> bool {
                match self {
                    Type::Primitive(PrimitiveType::Object(_)) => true,
                    _ => false
                }
            }

            #[inline]
            pub fn is_function(&self) -> bool {
                match self {
                    Type::Primitive(PrimitiveType::Function(_)) => true,
                    _ => false
                }
            }

            #[inline]
            pub fn object_inner(&self, sel_name: &str) -> Option<&Type> {
                let Type::Primitive(PrimitiveType::Object(attributes)) = self else {
                    panic!("Can't access selector on non object type!");
                };
                attributes.get(sel_name)
            }

            #[inline]
            pub fn infer_object_inner(&mut self, sel_name: &str, typ: &Type) {
                if let Some(t) = self.object_inner(sel_name) {
                    debug_assert!(typ <= t);
                }
                let Type::Primitive(PrimitiveType::Object(attributes)) = self else {
                    panic!("Can't access selector on non object type!");
                };
                attributes.insert(sel_name.into(), typ.clone());
            }

            #[inline]
            pub fn array_inner(&self) -> &Type {
                let Type::Primitive(PrimitiveType::Array(inner)) = self else {
                    panic!("Can't access inner type on non array type!");
                };
                &inner
            }

            #[inline]
            pub fn infer_array_inner(&mut self, typ: &Type) {
                let Type::Primitive(PrimitiveType::Array(inner)) = self else {
                    panic!("Can't access inner type on non array type!");
                };
                debug_assert!(typ <= inner);
                *inner = Box::new(typ.clone());
            }

            #[inline]
            pub fn target_typ(&self) -> &Type {
                let Type::Primitive(PrimitiveType::Function(arg_types)) = self else {
                    panic!("Can't access inner type on non array type!");
                };
                arg_types.last().unwrap()
            }

            #[inline]
            pub fn infer_target_typ(&mut self, target_typ: &Type) {
                let Type::Primitive(PrimitiveType::Function(arg_types)) = self else {
                    panic!("Can't access inner type on non array type!");
                };
                let last_mut = arg_types.last_mut().unwrap();
                debug_assert!(target_typ <= last_mut);
                *last_mut = target_typ.clone();
            }

            #[inline]
            pub fn number_min<'a>(lhs: &'a Self, rhs: &'a Self) -> &'a Self {
                match lhs {
                    Type::Abstract(AbstractType::Number) | Type::Abstract(AbstractType::IntLike) => match rhs {
                        Type::Abstract(AbstractType::Number) => lhs,
                        Type::Abstract(AbstractType::IntLike) | Type::Primitive(_) => rhs,
                        _ => panic!("rhs must be number!")
                    },
                    Type::Primitive(PrimitiveType::Float) => lhs,
                    Type::Primitive(PrimitiveType::Int) | Type::Primitive(PrimitiveType::Bool) => {
                        match rhs {
                            Type::Abstract(_) => lhs,
                            Type::Primitive(PrimitiveType::Float) | Type::Primitive(PrimitiveType::Int) => rhs,
                            Type::Primitive(PrimitiveType::Bool) => lhs,
                            _ => panic!("rhs must be number!")
                        }
                    }
                    _ => panic!("lhs must be a number!")
                }
            }

            pub fn infer(&mut self, typ: &Type) {
                debug_assert!(typ <= self);
                if typ < self {
                    *self = typ.clone();
                }
            }

            fn partial_cmp_attributes_helper(more: &BTreeMap<String, Type>, less: &BTreeMap<String, Type>) -> Option<Ordering> {
                debug_assert!(more.len() >= less.len());
                let mut covered = 0;
                let mut one_less = false;
                for (self_key, self_value) in more {
                    match less.get(self_key) {
                        Some(value) => {
                            if self_value > value || !(self_value <= value) {
                                return None;
                            }
                            covered += 1;
                            if self_value < value {
                                one_less = true;
                            }
                        }
                        None => {}
                    }
                }
                // more doesn't cover all keys of other map
                if covered != less.len() {
                    return None;
                }
                if more.len() == less.len() && !one_less {
                    Some(Ordering::Equal)
                } else {
                    Some(Ordering::Less)
                }
            }

            #[inline]
            fn partial_cmp_attributes(lhs: &BTreeMap<String, Type>, rhs: &BTreeMap<String, Type>) -> Option<Ordering> {
                if lhs.len() >= rhs.len() {
                    Self::partial_cmp_attributes_helper(lhs, rhs)
                } else {
                    Self::partial_cmp_attributes_helper(rhs, lhs).map(|ord| ord.reverse())
                }
            }

            fn partial_cmp_arg_types(lhs: &Vec<Type>, rhs: &Vec<Type>) -> Option<Ordering> {
                if lhs.len() != rhs.len() {
                    return None;
                }
                let mut assumed_ordering = None;
                let mut i = 0;
                for typ in lhs {
                    match assumed_ordering {
                        None => {
                            let Some(new_ord) = typ.partial_cmp(&rhs[i]) else {
                                return None;
                            };
                            assumed_ordering = Some(new_ord);
                        }
                        Some(ass_ord) => {
                            let Some(new_ord) = typ.partial_cmp(&rhs[i]) else {
                                return None;
                            };
                            if new_ord != ass_ord {
                                if ass_ord == Ordering::Equal {
                                    assumed_ordering = Some(new_ord);
                                } else {
                                    return None;
                                }
                            }
                        }
                    }
                    i += 1;
                }
                assumed_ordering
            }
        }

        // TODO rework for function objects, object objects, unit types and arrays
        impl Add for &Type {
            type Output = Result<Type, Error>;

            fn add(self, rhs: Self) -> Self::Output {
                match self {
                    Type::Abstract(left) => match rhs {
                        Type::Abstract(right) => left + right,
                        Type::Primitive(right) => right + left
                    },
                    Type::Primitive(left) => match rhs {
                        Type::Abstract(right) => left + right,
                        Type::Primitive(right) => left + right
                    }
                }
            }
        }

        impl Add<&AbstractType> for &PrimitiveType {
            type Output = Result<Type, Error>;

            fn add(self, rhs: &AbstractType) -> Self::Output {
                match rhs {
                    AbstractType::Any => Ok(Type::Primitive(self.clone())),
                    AbstractType::AddAndEq => match self {
                        PrimitiveType::String | PrimitiveType::Int | PrimitiveType::Float | PrimitiveType::Bool => {
                            Ok(Type::Primitive(self.clone()))
                        }
                        _ => Err(Error::TypeError(Type::Primitive(self.clone()), Type::Abstract(rhs.clone())))
                    }
                    AbstractType::Number => match self {
                        PrimitiveType::Int | PrimitiveType::Float | PrimitiveType::Bool => {
                            Ok(Type::Primitive(self.clone()))
                        }
                        _ => Err(Error::TypeError(Type::Primitive(self.clone()), Type::Abstract(rhs.clone())))
                    }
                    AbstractType::IntLike => match self {
                        PrimitiveType::Int | PrimitiveType::Bool => {
                            Ok(Type::Primitive(self.clone()))
                        }
                        _ => Err(Error::TypeError(Type::Primitive(self.clone()), Type::Abstract(rhs.clone())))
                    }
                }
            }
        }

        impl Add for &PrimitiveType {
            type Output = Result<Type, Error>;

            fn add(self, rhs: Self) -> Self::Output {
                match self {
                    PrimitiveType::Function(left_args) => match rhs {
                        PrimitiveType::Function(right_args) => {
                            if left_args.len() != right_args.len() {
                                return Err(Error::TypeError(Type::Primitive(self.clone()), Type::Primitive(rhs.clone())))
                            }
                            let mut res_args = vec![];
                            for (left, right) in left_args.iter().zip(right_args.iter()) {
                                res_args.push((left + right)?);
                            }
                            Ok(Type::Primitive(PrimitiveType::Function(res_args)))
                        },
                        _ => Err(Error::TypeError(Type::Primitive(self.clone()), Type::Primitive(rhs.clone())))
                    },
                    PrimitiveType::Array(left) => match rhs {
                        PrimitiveType::Array(right) => {
                            Ok(Type::Primitive(PrimitiveType::Array(Box::new((left.as_ref() + right.as_ref())?))))
                        },
                        _ => Err(Error::TypeError(Type::Primitive(self.clone()), Type::Primitive(rhs.clone())))
                    },
                    PrimitiveType::Object(left) => match rhs {
                        PrimitiveType::Object(right) => {
                            let mut union = HashMultimap::from(left.clone());
                            union.union_with_map(right.clone());
                            let mut res = BTreeMap::new();
                            for (key, set) in union.into_iter() {
                                let mut set_iter = set.into_iter();
                                let mut res_typ = set_iter.next().unwrap();
                                for typ in set_iter {
                                    res_typ = (&typ + &res_typ)?;
                                }
                                res.insert(key, res_typ);
                            }
                            Ok(Type::Primitive(PrimitiveType::Object(res)))
                        }
                        _ => Err(Error::TypeError(Type::Primitive(self.clone()), Type::Primitive(rhs.clone())))
                    },
                    _ => Err(Error::TypeError(Type::Primitive(self.clone()), Type::Primitive(rhs.clone())))
                }
            }
        }

        impl Add for &AbstractType {
            type Output = Result<Type, Error>;

            // Merges two abstract types to form the strictest new type of the two
            fn add(self, rhs: Self) -> Self::Output {
                match self {
                    AbstractType::Any => Ok(Type::Abstract(rhs.clone())),
                    AbstractType::AddAndEq => match rhs {
                        AbstractType::Any | AbstractType::AddAndEq => Ok(Type::Abstract(AbstractType::AddAndEq)),
                        AbstractType::Number => Ok(Type::Abstract(AbstractType::Number)),
                        AbstractType::IntLike => Ok(Type::Abstract(AbstractType::IntLike))
                    },
                    AbstractType::Number => match rhs {
                        AbstractType::Any | AbstractType::Number | AbstractType::AddAndEq => Ok(Type::Abstract(AbstractType::Number)),
                        AbstractType::IntLike => Ok(Type::Abstract(AbstractType::IntLike))
                    },
                    AbstractType::IntLike => Ok(Type::Abstract(AbstractType::IntLike)),

                }
            }
        }

        impl Eq for Type {}

        impl PartialEq for Type {
            fn eq(&self, other: &Self) -> bool {
                match self {
                    Type::Abstract(self_typ) => match other {
                        Type::Abstract(other_typ) => self_typ == other_typ,
                        Type::Primitive(_) => false
                    }
                    Type::Primitive(self_typ) => match other {
                        Type::Abstract(_) => false,
                        Type::Primitive(other_typ) => self_typ == other_typ
                    }
                }
            }
        }

        impl PartialOrd for Type {
            fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                match self {
                    Type::Abstract(abs) => abs.partial_cmp(other),
                    Type::Primitive(prim) => match other {
                        Type::Abstract(abs) => abs.partial_cmp(self).map(|ord| ord.reverse()),
                        Type::Primitive(other_prim) => match other_prim {
                            PrimitiveType::Object(attributes) => {
                                if let PrimitiveType::Object(self_attributes) = prim {
                                    Self::partial_cmp_attributes(self_attributes, attributes)
                                } else {
                                    None
                                }
                            },
                            PrimitiveType::Array(inner) => {
                                if let PrimitiveType::Array(self_inner) = prim {
                                    self_inner.partial_cmp(inner)
                                } else {
                                    None
                                }
                            },
                            PrimitiveType::Function(arg_types) => {
                                if let PrimitiveType::Function(self_arg_types) = prim {
                                    Self::partial_cmp_arg_types(self_arg_types, arg_types)
                                } else {
                                    None
                                }
                            },
                            _ => if prim == other_prim { Some(Ordering::Equal) } else { None }
                        }
                    }
                }
            }
        }

        impl PartialEq for AbstractType {
            fn eq(&self, other: &Self) -> bool {
                match self {
                    AbstractType::Any => if let AbstractType::Any = other { true } else { false }
                    AbstractType::Number => if let AbstractType::Number = other { true } else { false }
                    AbstractType::IntLike => if let AbstractType::IntLike = other { true } else { false }
                    AbstractType::AddAndEq => if let AbstractType::AddAndEq = other { true } else { false }
                }
            }
        }


        impl PartialEq<Type> for AbstractType {
            fn eq(&self, other: &Type) -> bool {
                match other {
                    Type::Abstract(abs) => abs == self,
                    Type::Primitive(_) => return false
                }
            }
        }

        impl PartialOrd<Type> for AbstractType {
            fn partial_cmp(&self, other: &Type) -> Option<Ordering> {
                match self {
                    AbstractType::Any => match other {
                        Type::Abstract(AbstractType::Any) => Some(Ordering::Equal),
                        _ => Some(Ordering::Greater)
                    },
                    AbstractType::AddAndEq => match other {
                        Type::Abstract(a) => match a {
                            AbstractType::Any => Some(Ordering::Less),
                            AbstractType::AddAndEq => Some(Ordering::Equal),
                            AbstractType::Number => Some(Ordering::Greater),
                            AbstractType::IntLike => Some(Ordering::Greater)
                        },
                        Type::Primitive(p) => match p {
                            PrimitiveType::String | PrimitiveType::Int | PrimitiveType::Float | PrimitiveType::Bool => Some(Ordering::Greater),
                            _ => None
                        }
                    },
                    AbstractType::Number => match other {
                        Type::Abstract(a) => match a {
                            AbstractType::Any => Some(Ordering::Less),
                            AbstractType::AddAndEq => Some(Ordering::Less),
                            AbstractType::Number => Some(Ordering::Equal),
                            AbstractType::IntLike => Some(Ordering::Greater)
                        },
                        Type::Primitive(p) => match p {
                            PrimitiveType::Int | PrimitiveType::Float | PrimitiveType::Bool => Some(Ordering::Greater),
                            _ => None
                        }
                    }
                    AbstractType::IntLike => match other {
                        Type::Abstract(a) => match a {
                            AbstractType::Any | AbstractType::AddAndEq | AbstractType::Number => Some(Ordering::Less),
                            AbstractType::IntLike => Some(Ordering::Equal),
                        },
                        Type::Primitive(p) => match p {
                            PrimitiveType::Int | PrimitiveType::Bool => Some(Ordering::Greater),
                            _ => None
                        }
                    }
                }
            }
        }

        impl<'a> View<'a> {

            pub fn globals_mut(&mut self) -> HashMultimap<String, &mut Var<'a>> {
                self.root.as_mut().map(|x| match x {
                    NodeOrBlock::Node(node) => node.globals_mut(),
                    NodeOrBlock::Block(b) => b.expr.globals_mut()
                })
                    .unwrap_or(HashMultimap::new())
            }

            pub fn globals(&self) -> HashMultimap<String, &Var<'a>> {
                self.root.as_ref().map(|x| match x {
                    NodeOrBlock::Node(node) => node.globals(),
                    NodeOrBlock::Block(b) => b.expr.globals()
                })
                    .unwrap_or(HashMultimap::new())
            }
        }

        impl<'a> Node<'a> {
            pub fn globals_mut(&mut self) -> HashMultimap<String, &mut Var<'a>> {
                let mut res = HashMultimap::new();
                match self {
                    Node::Text(_) => res,
                    Node::Tag(tag) => {
                        let mut res = HashMultimap::new();
                        for attr in &mut tag.attributes {
                            match &mut attr.value {
                                AttributeValue::StrLit(_) => {}
                                AttributeValue::Block(block) => {
                                    res.extend(block.expr.globals_mut());
                                }
                            }
                        }
                        if let Some(block) = &mut tag.block {
                            for child in &mut block.children {
                                match child {
                                    NodeOrBlock::Node(node) => {
                                        res.extend(node.globals_mut());
                                    }
                                    NodeOrBlock::Block(b) => {
                                        res.extend(b.expr.globals_mut());
                                    }
                                }
                            }
                        }
                        res
                    }

                }
            }

            pub fn globals(&self) -> HashMultimap<String, &Var<'a>> {
                let mut res = HashMultimap::new();
                match self {
                    Node::Text(_) => res,
                    Node::Tag(tag) => {
                        let mut res = HashMultimap::new();
                        for attr in &tag.attributes {
                            match &attr.value {
                                AttributeValue::StrLit(_) => {}
                                AttributeValue::Block(block) => {
                                    res.extend(block.expr.globals());
                                }
                            }
                        }
                        if let Some(block) = &tag.block {
                            for child in &block.children {
                                match child {
                                    NodeOrBlock::Node(node) => {
                                        res.extend(node.globals());
                                    }
                                    NodeOrBlock::Block(b) => {
                                        res.extend(b.expr.globals());
                                    }
                                }
                            }
                        }
                        res
                    }

                }
            }
        }

        impl<'a> Expr<'a> {

            pub fn globals(&self) -> HashMultimap<String, &Var<'a>> {
                match self {
                    Expr::If(if0) => {
                        let mut res = HashMultimap::new();
                        res.extend(if0.condition.globals());
                        res.extend(if0.then_branch.expr.globals());
                        res.extend(if0.else_branch.1.expr.globals());
                        for (_, _, cond, block) in &if0.elseif_branches {
                            res.extend(cond.globals());
                            res.extend(block.expr.globals());
                        }
                        res
                    }
                    Expr::For(for0) => {
                        let mut res = HashMultimap::new();
                        res.extend(for0.expr.globals());
                        res.extend(for0.block.expr.globals());
                        res
                    }
                    Expr::UnaryAp(un_ap) => un_ap.right.globals(),
                    Expr::Lit(_) => HashMultimap::new(),
                    Expr::Var(var) => {
                        match var.scope {
                            Scope::Global => HashMultimap::from([(var.name.text.span.value().to_string(), vec![var])]),
                            Scope::Local => HashMultimap::new()
                        }
                    }
                    Expr::Node(node) => node.globals(),
                    Expr::Empty(_) => HashMultimap::new(),
                    Expr::Group(group) => group.expr.globals(),
                    Expr::BinaryAp(bin_ap) => {
                        let mut res = HashMultimap::new();
                        res.extend(bin_ap.left.globals());
                        res.extend(bin_ap.right.right.globals());
                        res
                    },
                    Expr::SelectorAp(sel_ap) => {
                        let mut res = HashMultimap::new();
                        res.extend(sel_ap.expr.globals());
                        match &sel_ap.right.selector {
                            SelectorOp::Named(_) => {}
                            SelectorOp::Bracket(b) => {
                                res.extend(b.expr.globals());
                            }
                        }
                        res
                    }
                    Expr::Ap(ap) => {
                        let mut res = HashMultimap::new();
                        res.extend(ap.expr.globals());
                        for arg in ap.right.group.expr.args() {
                            res.extend(arg.globals());
                        }
                        res
                    }
                }
            }

            pub fn globals_mut(&mut self) -> HashMultimap<String, &mut Var<'a>> {
                match self {
                    Expr::If(if0) => {
                        let mut res = HashMultimap::new();
                        res.extend(if0.condition.globals_mut());
                        res.extend(if0.then_branch.expr.globals_mut());
                        res.extend(if0.else_branch.1.expr.globals_mut());
                        for (_, _, cond, block) in &mut if0.elseif_branches {
                            res.extend(cond.globals_mut());
                            res.extend(block.expr.globals_mut());
                        }
                        res
                    }
                    Expr::For(for0) => {
                        let mut res = HashMultimap::new();
                        res.extend(for0.expr.globals_mut());
                        res.extend(for0.block.expr.globals_mut());
                        res
                    }
                    Expr::UnaryAp(un_ap) => un_ap.right.globals_mut(),
                    Expr::Lit(_) => HashMultimap::new(),
                    Expr::Var(var) => {
                        match var.scope {
                            Scope::Global => HashMultimap::from([(var.name.text.span.value().to_string(), vec![var])]),
                            Scope::Local => HashMultimap::new()
                        }
                    }
                    Expr::Node(node) => node.globals_mut(),
                    Expr::Empty(_) => HashMultimap::new(),
                    Expr::Group(group) => group.expr.globals_mut(),
                    Expr::BinaryAp(bin_ap) => {
                        let mut res = HashMultimap::new();
                        res.extend(bin_ap.left.globals_mut());
                        res.extend(bin_ap.right.right.globals_mut());
                        res
                    },
                    Expr::SelectorAp(sel_ap) => {
                        let mut res = HashMultimap::new();
                        res.extend(sel_ap.expr.globals_mut());
                        match &mut sel_ap.right.selector {
                            SelectorOp::Named(_) => {}
                            SelectorOp::Bracket(b) => {
                                res.extend(b.expr.globals_mut());
                            }
                        }
                        res
                    }
                    Expr::Ap(ap) => {
                        let mut res = HashMultimap::new();
                        res.extend(ap.expr.globals_mut());
                        for arg in ap.right.group.expr.args_mut() {
                            res.extend(arg.globals_mut());
                        }
                        res
                    }
                }
            }

            #[inline]
            pub fn typ(&self) -> &Type {
                match self {
                    Expr::If(if0) => &if0.typ,
                    Expr::For(for0) => &for0.typ,
                    Expr::UnaryAp(un_ap) => &un_ap.typ,
                    Expr::Lit(lit) => match lit {
                        Lit::Str(_) => &Type::Primitive(PrimitiveType::String),
                        Lit::Int(_) => &Type::Primitive(PrimitiveType::Int),
                        Lit::Float(_) => &Type::Primitive(PrimitiveType::Float),
                        Lit::Bool(_) => &Type::Primitive(PrimitiveType::Bool)
                    },
                    Expr::Var(var) => &var.typ,
                    Expr::Node(_) => &Type::Primitive(PrimitiveType::Node),
                    Expr::Empty(_) => &Type::Primitive(PrimitiveType::Unit),
                    Expr::Group(group) => group.expr.typ(),
                    Expr::BinaryAp(bin_ap) => &bin_ap.typ,
                    Expr::SelectorAp(sel_ap) => &sel_ap.typ,
                    Expr::Ap(ap) => &ap.typ
                }
            }

            fn types_match_and_infer(types: Vec<Option<&mut Type>>) -> Result<Option<&mut Type>, Error> {
                let mut min_typ = None;
                let mut rest = vec![];
                for typ_opt in types {
                    match min_typ {
                        None => min_typ = typ_opt,
                        Some(min) => match typ_opt {
                            None => min_typ = Some(min),
                            Some(typ) => {
                                if !(typ <= min) && !(typ > min) {
                                    return Err(Error::TypeError(typ.clone(), (*min).clone()));
                                }
                                if typ < min {
                                    rest.push(min);
                                    min_typ = Some(typ);
                                } else {
                                    rest.push(typ);
                                    min_typ = Some(min);
                                }
                            }
                        }
                    }
                }
                if let Some(min) = &min_typ {
                    for typ in rest {
                        if *min < typ {
                            *typ = (*min).clone();
                        }
                    }
                }
                Ok(min_typ)
            }

            /**
             * Finds the type of a given var name in an expression and adjusts all unequal types
             * if possible otherwise returns a type error
             */
            pub fn find_var_typ_and_infer(&mut self, var_name: &str) -> Result<Option<&mut Type>, Error> {
                match self {
                    Expr::If(if0) => {
                        let mut vars = vec![];
                        vars.push(if0.condition.find_var_typ_and_infer(var_name)?);
                        vars.push(if0.then_branch.expr.find_var_typ_and_infer(var_name)?);
                        for (_, _, expr, block) in &mut if0.elseif_branches {
                            vars.push(expr.find_var_typ_and_infer(var_name)?);
                            vars.push(block.expr.find_var_typ_and_infer(var_name)?);
                        }
                        vars.push(if0.else_branch.1.expr.find_var_typ_and_infer(var_name)?);
                        Self::types_match_and_infer(vars)
                    },
                    Expr::For(for0) => {
                        let mut vars = vec![];
                        vars.push(for0.expr.find_var_typ_and_infer(var_name)?);
                        vars.push(for0.block.expr.find_var_typ_and_infer(var_name)?);
                        Self::types_match_and_infer(vars)
                    },
                    Expr::UnaryAp(un_ap) => un_ap.right.find_var_typ_and_infer(var_name),
                    Expr::Lit(_) => Ok(None),
                    Expr::Var(var) => {
                        if var.name.text.span.value() == var_name {
                            Ok(Some(&mut var.typ))
                        } else {
                            Ok(None)
                        }
                    },
                    Expr::Node(_) => Ok(None),
                    Expr::Empty(_) => Ok(None),
                    Expr::Group(group) => group.expr.find_var_typ_and_infer(var_name),
                    Expr::BinaryAp(bin_ap) => {
                        let mut vars = vec![];
                        vars.push(bin_ap.left.find_var_typ_and_infer(var_name)?);
                        vars.push(bin_ap.right.right.find_var_typ_and_infer(var_name)?);
                        Self::types_match_and_infer(vars)
                    },
                    Expr::SelectorAp(sel_ap) => {
                        let mut vars = vec![];
                        vars.push(sel_ap.expr.find_var_typ_and_infer(var_name)?);
                        match &mut sel_ap.right.selector {
                            SelectorOp::Named(_) => {}
                            SelectorOp::Bracket(b) => vars.push(b.expr.find_var_typ_and_infer(var_name)?)
                        }
                        Self::types_match_and_infer(vars)
                    },
                    Expr::Ap(ap) => {
                        let mut vars = vec![];
                        vars.push(ap.expr.find_var_typ_and_infer(var_name)?);
                        for arg in ap.right.group.expr.args_mut() {
                            vars.push(arg.find_var_typ_and_infer(var_name)?);
                        }
                        Self::types_match_and_infer(vars)
                    }
                }
            }

            pub fn check_var_duplicated(&mut self, var_name: &str) -> Result<(), Error> {
                match self {
                    Expr::If(if0) => {
                        if0.condition.check_var_duplicated(var_name)?;
                        if0.then_branch.expr.check_var_duplicated(var_name)?;
                        for (_, _, expr, block) in &mut if0.elseif_branches {
                            expr.check_var_duplicated(var_name)?;
                            block.expr.check_var_duplicated(var_name)?;
                        }
                        if0.else_branch.1.expr.check_var_duplicated(var_name)?;
                        Ok(())
                    },
                    Expr::For(for0) => {
                        if for0.binding.name.text.span.value() == var_name {
                            return Err(Error::DuplicateVar(var_name.into()))
                        }
                        for0.expr.check_var_duplicated(var_name)?;
                        for0.block.expr.check_var_duplicated(var_name)?;
                        Ok(())
                    },
                    Expr::UnaryAp(un_ap) => {
                        un_ap.right.check_var_duplicated(var_name)?;
                        Ok(())
                    },
                    Expr::Lit(_) | Expr::Var(_) | Expr::Node(_) | Expr::Empty(_) => Ok(()),
                    Expr::Group(group) => {
                        group.expr.check_var_duplicated(var_name)?;
                        Ok(())
                    },
                    Expr::BinaryAp(bin_ap) => {
                        bin_ap.left.check_var_duplicated(var_name)?;
                        bin_ap.right.right.check_var_duplicated(var_name)?;
                        Ok(())
                    },
                    Expr::SelectorAp(sel_ap) => {
                        sel_ap.expr.check_var_duplicated(var_name)?;
                        match &mut sel_ap.right.selector {
                            SelectorOp::Named(_) => {}
                            SelectorOp::Bracket(b) => b.expr.check_var_duplicated(var_name)?
                        }
                        Ok(())
                    },
                    Expr::Ap(ap) => {
                        ap.expr.check_var_duplicated(var_name)?;
                        for arg in ap.right.group.expr.args_mut() {
                            arg.check_var_duplicated(var_name)?;
                        }
                        Ok(())
                    }
                }
            }

            /**
             * Infers the type for a var by a given name and type
             */
            #[inline]
            pub fn infer_var(&mut self, var_name: &str, typ: &Type) {
                match self {
                    Expr::If(if0) => {
                        if0.condition.infer_var(var_name, typ);
                        if0.then_branch.expr.infer_var(var_name, typ);
                        for (_, _, condition, block) in &mut if0.elseif_branches {
                            condition.infer_var(var_name, typ);
                            block.expr.infer_var(var_name, typ);
                        }
                        if0.else_branch.1.expr.infer_var(var_name, typ);
                    },
                    Expr::For(for0) => {
                        for0.expr.infer_var(var_name, typ);
                        for0.block.expr.infer_var(var_name, typ);
                    },
                    Expr::UnaryAp(un_ap) => un_ap.right.infer_var(var_name, typ),
                    Expr::Var(var) => {
                        if var.name.text.span.value() == var_name {
                            var.typ = typ.clone()
                        }
                    },
                    Expr::Node(_) | Expr::Empty(_) | Expr::Lit(_) => {},
                    Expr::Group(group) => group.expr.infer_var(var_name, typ),
                    Expr::BinaryAp(bin_ap) => {
                        bin_ap.left.infer_var(var_name, typ);
                        bin_ap.right.right.infer_var(var_name, typ);
                    },
                    Expr::SelectorAp(sel_ap) => {
                        sel_ap.expr.infer_var(var_name, typ);
                        match &mut sel_ap.right.selector {
                            SelectorOp::Named(_) => {}
                            SelectorOp::Bracket(b) => b.expr.infer_var(var_name, typ),
                        }
                    },
                    Expr::Ap(ap) => {
                        ap.expr.infer_var(var_name, typ);
                        for arg in ap.right.group.expr.args_mut() {
                            arg.infer_var(var_name, typ);
                        }
                    }
                }
            }

            /**
             * This sets the type for an expression.
             * This can only be done with types that are smaller equals the existing type.
             * NOTE: This function doesn't merge object types!
             */
            #[inline]
            pub fn infer(&mut self, typ: &Type) {
                match self {
                    Expr::If(if0) => {
                        debug_assert!(typ <= &if0.typ);
                        if typ < &if0.typ {
                            if0.typ = typ.clone();
                            if0.then_branch.expr.infer(typ);
                            for (_, _, _, block) in &mut if0.elseif_branches {
                                block.expr.infer(typ);
                            }
                            if0.else_branch.1.expr.infer(typ);
                        }
                    },
                    Expr::For(for0) => {
                        debug_assert!(typ <= &for0.typ);
                        if typ < &for0.typ {
                            for0.typ = typ.clone();
                            for0.block.expr.infer(typ);
                        }
                    },
                    Expr::UnaryAp(un_ap) => {
                        debug_assert!(typ <= &un_ap.typ);
                        if typ < &un_ap.typ {
                            un_ap.typ = typ.clone();
                            un_ap.right.infer(typ);
                        }
                    },
                    Expr::Lit(lit) => match lit {
                        Lit::Str(_) => debug_assert!(typ == &Type::STRING),
                        Lit::Int(_) => debug_assert!(typ == &Type::INT),
                        Lit::Float(_) => debug_assert!(typ == &Type::FLOAT),
                        Lit::Bool(_) => debug_assert!(typ == &Type::BOOL),
                    },
                    Expr::Var(var) => {
                        debug_assert!(typ <= &var.typ);
                        if typ < &var.typ {
                            var.typ = typ.clone();
                        }
                    },
                    Expr::Node(_) => debug_assert!(typ == &Type::NODE),
                    Expr::Empty(_) => debug_assert!(typ == &Type::UNIT),
                    Expr::Group(group) => {
                        group.expr.infer(typ);
                    },
                    Expr::BinaryAp(bin_ap) => {
                        debug_assert!(typ <= &bin_ap.typ);
                        if typ < &bin_ap.typ {
                            bin_ap.typ = typ.clone();
                            let left_typ = bin_ap.left.typ();
                            let right_typ = bin_ap.right.right.typ();
                            if typ < left_typ {
                                bin_ap.left.infer(typ);
                            }
                            if typ < right_typ {
                                bin_ap.right.right.infer(typ);
                            }
                        }
                    },
                    Expr::SelectorAp(sel) => {
                        debug_assert!(typ <= &sel.typ);
                        if typ < &sel.typ {
                            sel.typ = typ.clone();
                            match &sel.right.selector {
                                SelectorOp::Named(named) => {
                                    let sel_name = named.name.text.span.value();
                                    let mut sel_typ = sel.expr.typ().clone();
                                    sel_typ.infer_object_inner(sel_name, typ);
                                    sel.expr.infer(&sel_typ);
                                }
                                SelectorOp::Bracket(_) => {
                                    let mut sel_typ = sel.expr.typ().clone();
                                    sel_typ.infer_array_inner(typ);
                                    sel.expr.infer(&sel_typ);
                                }
                            }
                        }
                    }
                    Expr::Ap(ap) => {
                        debug_assert!(typ <= &ap.typ);
                        if typ < &ap.typ {
                            let mut f_typ = ap.expr.typ().clone();
                            f_typ.infer_target_typ(typ);
                            ap.expr.infer(&f_typ);
                        }
                    }
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
                        for (_, _, _, block) in &mut if0.elseif_branches {
                            block.expr.infer_scope(var, scope);
                        }
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

        impl<'a> Eq for Var<'a> {}

        impl<'a> PartialEq for Var<'a> {
            fn eq(&self, other: &Self) -> bool {
                self.scope == other.scope &&
                self.typ == other.typ &&
                self.name.text.span.value() == other.name.text.span.value()
            }
        }

        impl<'a> Hash for Var<'a> {
            fn hash<H: Hasher>(&self, state: &mut H) {
                self.name.text.span.value().hash(state)
            }
        }
    }

    mod implementation {
        use crate::cursor::Cursor;
        use crate::parser::{Parse, Parser};
        use crate::rex::lex;
        use crate::rex::parse::{ApRight, Attribute, AttributeValue, Block, BracketSelector, Error, Expr, For, Group, If, BinaryApRight, NamedSelector, Node, NodeOrBlock, primitive, BinaryOp, Punctuated, SelectorAp, SelectorApRight, SelectorOp, TagBlock, TagNode, TextNode, Var, View, UnaryAp, UnaryOp, PunctuatedIter, PunctuatedIterMut};
        use crate::rex::parse::scope::Scope;
        use crate::rex::parse::typ::{Type, Typed};

        impl From<lex::Error> for Error {
            fn from(err: lex::Error) -> Self {
                Error::Lexer(err)
            }
        }

        impl<'a, P, S> Punctuated<'a, P, S> {
            pub(crate) fn args(&self) -> PunctuatedIter<'_, 'a, P, S> {
                PunctuatedIter {
                    inner: Some(self)
                }
            }
            pub(crate) fn args_mut(&mut self) -> PunctuatedIterMut<'_, 'a, P, S> {
                PunctuatedIterMut {
                    inner: Some(self)
                }
            }
        }

        impl<'b, 'a, P, S> Iterator for PunctuatedIter<'b, 'a, P, S> {
            type Item = &'b P;

            fn next(&mut self) -> Option<Self::Item> {
                match self.inner {
                    None => None,
                    Some(punct) => {
                        let e = &*punct.expr;
                        self.inner = punct.other.as_ref().map(|(_, other)| &**other);
                        Some(e)
                    }
                }
            }
        }

        impl<'b, 'a, P, S> Iterator for PunctuatedIterMut<'b, 'a, P, S> {
            type Item = &'b mut P;

            fn next(&mut self) -> Option<Self::Item> {
                match self.inner.take() {
                    None => None,
                    Some(punct) => {
                        let e = &mut *punct.expr;
                        self.inner = punct.other.as_mut().map(|(_, other)| &mut **other);
                        Some(e)
                    }
                }
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

            fn parse<C: Cursor<Item=Self::Token>>(parser: Parser<C>) -> Result<(Parser<C>, Self), Self::Error>  {
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
                                    parser = parser1;
                                    left = Expr::Ap(Typed::ap(left, right)?);
                                },
                                None => {
                                    let (parser1, right) = parser1.opt_parse::<SelectorApRight>();
                                    match right {
                                        Some(right) => {
                                            parser = parser1;
                                            left = Expr::SelectorAp(Typed::sel_ap(left, right)?);
                                        },
                                        None => {
                                            let (parser1, right) = parser1.opt_parse::<BinaryApRight>();
                                            match right {
                                                Some(mut right) => {
                                                    parser = parser1;
                                                    left = Expr::BinaryAp(Typed::bin_ap(left, right)?);
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
                let (parser, elseif_branches, else_branch) = loop {
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
                Ok((parser, Typed::r#if(if0, condition, then_branch, elseif_branches, else_branch)?))
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
                let (parser, mut expr) = parser.parse::<Expr>()?;
                let (parser, _) = parser.opt_parse_token::<lex::Whitespace>();
                let (parser, mut block) = parser.parse::<Block>()?;
                binding.scope = Scope::Local;
                block.expr.infer_scope(&binding, binding.scope);

                Ok((parser, Typed::r#for(for0, binding, in0, expr, block)?))
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

                Ok((parser, Typed::un_ap(op, right)?))
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
                    typ: Type::ANY,
                    scope: Scope::Global,
                    name
                }))
            }
        }
    }
}

pub use parse::View;