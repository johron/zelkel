use crate::lexer::Token;
use logos::Logos;
use crate::parser::parser::parse_program;

mod lexer;
mod ast;
mod codegen;
mod parser;

fn main() {
    let src = r#"
static fn! main() {
    std::io.println("Hello, world!"); // test
}
"#;
    let mut lex = Token::lexer(src);

    let tokens = vec![];
    while let Some(result) = lex.next() {
        match result {
            Ok(token) => println!("Matched: {:?}", token),
            Err(_) => println!("Error at span: {:?}, found '{}'", lex.span(), lex.slice()),
        }
    }

    let (_, program) = parse_program(&tokens).unwrap();
}
