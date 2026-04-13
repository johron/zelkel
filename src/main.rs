use logos::Logos;
use crate::lexer::lexer::lexer;
use crate::parser::parser::{parse_program};

mod ast;
mod codegen;
mod parser;
mod lexer;

fn main() -> Result<(), String> {
    let src = r#"
class String {
}
"#;
    let tokens = lexer(src).unwrap();

    let (_, program) = parse_program(tokens).map_err(|e| format!("Parse error: {:?}", e))?;
    
    println!("{:#?}", program);
    Ok(())
}
