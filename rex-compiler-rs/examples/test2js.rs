use rex_compiler_rs::codegen::js::JsCodegen;
use rex_compiler_rs::rex::View;
use rex_compiler_rs::{parser, rex};
use std::fs::File;
use std::io::Write;

fn main() {
    let code = include_str!("../test-src/test1.rex");
    let lexer = rex::lex::Lexer::new(code);

    let parser = parser::Parser::new(lexer);
    let (_, view) = parser.parse::<View>().unwrap();

    let rs = JsCodegen::new();
    let res = rs.generate(&view);
    std::fs::create_dir_all("test-out/").unwrap();
    let mut out = File::create("test-out/test2.js").unwrap();
    out.write_all(res.as_bytes()).unwrap();
}
