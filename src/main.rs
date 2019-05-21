#![feature(never_type)]
mod ir;
mod lexer;
mod parser;
mod preprocessor;
use crate::lexer::TokenStream;
use crate::parser::{parse, print_ast};
use crate::preprocessor::preprocess;
fn main() {
    let args: Vec<String> = std::env::args().collect();
    let file_path = args.get(1).expect("No input file");
    let code = std::fs::read_to_string(file_path).expect("Can't open file");
    println!("{}", code);
    let code = preprocess(code);
    println!("After preprocess:\n{}", code);
    let ast = parse(TokenStream::new(&code));
    print_ast(&ast, "ast.dot").unwrap();
    let program = ast.unwrap_program();
    println!("Program:\n{}", program);
}
