use crate::error::Result;
use crate::token::{SpannedToken, Token};
use logos::Logos;

pub(crate) fn lex(input: &str) -> Result<Vec<SpannedToken>> {
    Token::lexer(input)
        .spanned()
        .map(|token| {
            let spanned = SpannedToken {
                token: token.0,
                span: token.1,
            };
            match spanned.token {
                Token::Error => Err(spanned.as_error(input, "unknown token".to_owned()))?,
                _ => Ok(spanned),
            }
        })
        .collect()
}
