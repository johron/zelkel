use crate::lexer::Token;
use logos::Logos;
use crate::parser::parser::{parse_program};

mod lexer;
mod ast;
mod codegen;
mod parser;

fn main() -> Result<(), String> {
    let src = r#"
class String {
}
"#;
    let lex = Token::lexer(src);

    let tokens: Vec<Token> = lex.filter_map(|res| res.ok()).collect();
    
    let (_, program) = parse_program(&tokens).map_err(|e| format!("Parse error: {:?}", e))?;
    
    println!("{:#?}", program);
    Ok(())
}
