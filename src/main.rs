use crate::lexer::Token;
use logos::Logos;

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

    while let Some(result) = lex.next() {
        match result {
            Ok(token) => println!("Matched: {:?}", token),
            Err(_) => println!("Error at span: {:?}, found '{}'", lex.span(), lex.slice()),
        }
    }
}
