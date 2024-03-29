use crate::codegen::js::JsCodegen;
use crate::rex::parse::View;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::Write;

pub mod codegen;
mod cursor;
pub mod parser;
pub mod rex;
mod util;

use crate::codegen::rs::RsCodegen;
pub use rex::lex;

fn main() {
    let code = include_str!("../test-src/test.rex");
    let mut lexer = lex::Lexer::new(code);
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

    let rs = RsCodegen::new();
    let res = rs.generate(&view);
    std::fs::create_dir_all("test-out/").unwrap();
    let mut out = File::create("test-out/test.rs").unwrap();
    out.write_all(res.as_bytes()).unwrap();
}
