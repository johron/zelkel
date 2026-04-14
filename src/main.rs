use crate::lexer::lexer::lexer;
use crate::lexer::token::Tokens;
use crate::parser::parser::{parse_program};

mod ast;
mod codegen;
mod parser;
mod lexer;

fn main() -> Result<(), String> {
    let src = r#"
require io;

static fn! main() -> void {
    io:println("Hello, world!");
}
"#;

    let (rest, token_vec) = lexer(src, src).map_err(|e| {
        // e is a nom::Err<nom::error::Error<&str>>
        if let nom::Err::Error(err) = e {
            let (line, col) = get_line_col(src, err.input);
            let unexpected = err.input.chars().next().unwrap_or(' ');
            return format!("Lex error at line {}, col {}: Unexpected character '{}'", line, col, unexpected);
        }
        format!("Lexing failed: {:?}", e)
    })?;

    // Check if the whole input was consumed (important if not using all_consuming)
    if !rest.trim().is_empty() {
        let (line, col) = get_line_col(src, rest);
        let unexpected = rest.trim().chars().next().unwrap_or(' ');
        return Err(format!("Lex error at line {}, col {}: Unexpected character '{}'", line, col, unexpected));
    }


    let input = Tokens::new(&token_vec);

    let (_, program) = parse_program(input).map_err(|e| {
        match e {
            nom::Err::Error(err) | nom::Err::Failure(err) => {
                // err.input is the TokenSlice where the error happened
                if let Some(first_token) = err.input.tokens.first() {
                    let offset = first_token.offset();
                    // Re-use the src to find line/col from the byte offset
                    let (line, col) = get_line_col_from_offset(src, offset);
                    format!("Parse error at line {}, col {}: Unexpected token {:?}", line, col, first_token)
                } else {
                    "Parse error at end of file".to_string()
                }
            }
            _ => "Incomplete parsing".to_string(),
        }
    })?;
    
    println!("{:#?}", program);
    Ok(())
}

fn get_line_col(full_src: &str, remaining: &str) -> (usize, usize) {
    let offset = full_src.len() - remaining.len();
    let prefix = &full_src[..offset];

    let line = prefix.lines().count();
    let col = prefix.lines().last().map(|l| l.len() + 1).unwrap_or(1);

    // If the prefix ends with a newline, we are at the start of the next line
    if prefix.ends_with('\n') {
        (line + 1, 1)
    } else {
        (line.max(1), col)
    }
}

fn get_line_col_from_offset(src: &str, offset: usize) -> (usize, usize) {
    let prefix = &src[..offset];
    let line = prefix.lines().count();
    let col = prefix.lines().last().map(|l| l.len() + 1).unwrap_or(1);
    (line.max(1), col)
}