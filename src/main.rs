mod parser;
mod preprocessor;
mod lexer;
use lexer::Lexer;
use preprocessor::preprocess;
fn main() {
    let args: Vec<String> = std::env::args().collect();
    let file_path = &args[1];
    let code = std::fs::read_to_string(file_path).expect("Can't open file");
    println!("{}", code);
    let code = preprocess(code);
    println!("After preprocess:\n{}",code);
    let mut lexer = Lexer::new(&code);
    for _ in 0.. {
        if let Some(i) = lexer.next() {
            println!("{}\t{:?}", i, i)
        } else {
            break;
        }
    }
    println!("All idnets:");
    for i in 0.. {
        if let Some(i) = lexer.get_ident(i) {
            println!("{}", i);
        } else {
            break;
        }
    }
}
