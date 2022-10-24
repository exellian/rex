#![deny(clippy::all)]

#[macro_use]
extern crate napi_derive;

use napi::Status;
use rex_compiler_rs::codegen::js::JsCodegen;
use rex_compiler_rs::lex::Lexer;
use rex_compiler_rs::parser::Parser;
use rex_compiler_rs::rex::View;

#[napi]
pub fn compile_view(code: String) -> napi::Result<String> {
    let lexer = Lexer::new(&code);
    let parser = Parser::new(lexer);
    let codegen = JsCodegen::new();
    match parser.parse::<View>() {
        Ok((_, view)) => Ok(codegen.generate(&view)),
        Err(err) => Err(napi::Error::new(Status::Ok, format!("{:?}", err)))
    }
}
