mod lexer;
use lexer::Lexer;
fn main() {
    let args: Vec<String> = std::env::args().collect();
    let file_path = &args[1];
    let code = std::fs::read_to_string(file_path).expect("Can't open file");
    println!("{}", code);
    let lexer = Lexer::new(&code);
    for i in lexer {
        println!("{}\t{:?}", i, i)
    }
}
