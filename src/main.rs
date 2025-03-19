use crate::lexer::{lex, TokenPos};
use crate::parser::parse;

mod lexer;
mod parser;

pub fn error(message: String, pos: TokenPos) -> String {
    format!("{} near {}:{}:{}", message, pos.path, pos.line, pos.col)
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let mut path: &String = &"test.zk".to_string();
    if args.len() >= 2 {
        path = &args[1];
    }
    let code = std::fs::read_to_string(path).expect("Failed to read the file");

    let tokens = lex(code, path.clone()).unwrap_or_else(|err| {
        eprintln!("Compilation error: {}", err);
        std::process::exit(1);
    });

    let ast = parse(tokens).unwrap_or_else(|err| {
        eprintln!("Compilation error: {}", err);
        std::process::exit(1);
    });

    println!("{:#?}", ast);
}
