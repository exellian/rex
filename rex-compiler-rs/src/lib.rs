use std::fs::File;
use std::io::Write;
use crate::codegen::js::JsCodegen;
use crate::rex::parse::View;

mod util;
mod cursor;
pub mod parser;
pub mod rex;
pub mod codegen;

pub use rex::lex;


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

    let js = JsCodegen::new();
    let res = js.generate(&view);
    std::fs::create_dir_all("test-out/").unwrap();
    let mut out = File::create("test-out/test.js").unwrap();
    out.write_all(res.as_bytes()).unwrap();
}