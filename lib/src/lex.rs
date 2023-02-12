use crate::error::{ErrorKind, Result};
use crate::token::{Token, TokenType};
use logos::Logos;

pub(crate) fn lex(input: &str) -> Result<Vec<Token>> {
    let mut tokens = Vec::new();
    let mut lexer = TokenType::lexer(input);
    while let Some(token) = lexer.next() {
        let spanned = Token {
            token,
            raw: lexer.slice().into(),
            span: lexer.span(),
        };

        if spanned.token == TokenType::Error {
            return Err(spanned.into_error(input, ErrorKind::Unknown));
        }

        tokens.push(spanned);
    }

    Ok(tokens)
}
