use crate::token::{Token, TokenType};
use logos::Logos;

#[derive(Debug)]
pub(crate) struct Lexer<'a> {
    inner: logos::Lexer<'a, TokenType>,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(source: &'a str) -> Self {
        Self {
            inner: TokenType::lexer(source),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let tt = self.inner.next()?;
        let slice = self.inner.slice();
        let span = self.inner.span();

        Some(Token {
            token: tt,
            raw: slice.into(),
            span,
        })
    }
}
