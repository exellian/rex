use crate::cursor::Cursor;
use crate::rex::parse::View;

mod util;
mod cursor;
mod parser;
mod rex;

fn main() {
    let code = include_str!("../test-src/test.rex");
    let mut lexer = rex::lex::Lexer::new(code);

/*
    let mut tokens = vec![];
    loop {
        let token = lexer.next();
        match token {
            Some(tok) => tokens.push(tok.unwrap()),
            None => break
        }
    }
    println!("{:?}", tokens);
*/

    let parser = parser::Parser::new(lexer);
    let (_, view) = parser.parse::<View>().unwrap();
    println!("{:#?}", view);

}