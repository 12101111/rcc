mod ir;
mod lexer;
mod preprocessor;
mod syntax;
use crate::lexer::Lexer;
use crate::preprocessor::preprocess;
use crate::syntax::{parse, print_ast};
use ir::Program;
fn main() {
    let args: Vec<String> = std::env::args().collect();
    let file_path = args.get(1).expect("No input file");
    let code = std::fs::read_to_string(file_path).expect("Can't open file");
    println!("{}", code);
    let code = preprocess(code);
    println!("After preprocess:\n{}", code);
    let lexer = Lexer::new(&code);
    let ast = parse(lexer);
    print_ast(&ast, "ast.dot").unwrap();
    let program = Program::new(ast);
    println!("\nProgram:\n{}", program);
}
