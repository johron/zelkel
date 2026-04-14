#[macro_export]
macro_rules! is_token {
    ($variant:ident) => {
        |t: &crate::lexer::token::Token| matches!(t, crate::lexer::token::Token::$variant(..))
    };
}

#[macro_export]
macro_rules! expect_token {
    ($input:expr, $variant:ident) => {
        $crate::parser::parser::match_token(|t| matches!(t, $crate::lexer::token::Token::$variant(..)))($input)
    };
}

#[macro_export]
macro_rules! is_tok {
    ($variant:ident) => {
        $crate::parser::parser::match_token(|t| matches!(t, $crate::lexer::token::Token::$variant(..)))
    };
}
