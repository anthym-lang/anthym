use crate::ast::*;
use crate::error::{Error, ErrorKind, Result};
use crate::ice::IceExt;
use crate::lex::lex;
use crate::token::{Token, TokenType};
use crate::unescape::unescape;
use std::collections::VecDeque;
use std::iter::from_fn;

macro_rules! ok_or_else {
    ($expr: expr, $or_else: block) => {
        match $expr {
            Some(inner) => inner,
            None => return Err($or_else),
        }
    };
}

pub(crate) fn parse_text(source: impl AsRef<str>) -> anyhow::Result<Program> {
    let mut parser = Parser::new(source.as_ref())?;
    Ok(parser.program()?)
}

#[derive(Debug)]
struct Parser<'a> {
    source: &'a str,
    tokens: VecDeque<Token>,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(source: &'a str) -> Result<Self> {
        Ok(Self {
            source,
            tokens: lex(source)?.into(),
        })
    }

    pub(crate) fn program(&mut self) -> Result<Program> {
        from_fn(|| {
            if self.current().is_some() {
                Some(self.stmt())
            } else {
                None
            }
        })
        .collect()
    }

    fn token(&mut self, token: TokenType) -> Result<Token> {
        self.take()
            .ok_or_else(|| Error::eof(self.source, [token].into()))
            .and_then(|owned| {
                if owned.token == token {
                    Ok(owned)
                } else {
                    Err(owned.into_error(
                        self.source,
                        ErrorKind::Expected {
                            expected: [token].into(),
                        },
                    ))
                }
            })
    }

    fn ident(&mut self) -> Result<Ident> {
        self.take()
            .ok_or_else(|| Error::eof(self.source, [TokenType::Ident].into()))
            .and_then(|owned| match owned.token {
                TokenType::Ident => Ok(owned.raw.into()),
                _ => Err(owned.into_error(
                    self.source,
                    ErrorKind::Expected {
                        expected: [TokenType::Ident].into(),
                    },
                )),
            })
    }

    // FIXME: formatting wut????
    fn literal(&mut self) -> Result<Literal> {
        self.take().ok_or_else(|| Error::eof(self.source, [TokenType::IntLiteral, TokenType::BoolLiteral, TokenType::CharLiteral, TokenType::FloatLiteral, TokenType::StringLiteral].into())).and_then(|owned| match owned.token {
            TokenType::FloatLiteral => Ok(Literal::Float(owned.raw.parse().unwrap_or_ice_msg("the lexer and the parser think that float literals are different things"))),
            TokenType::IntLiteral => Ok(Literal::Int(owned.raw.parse().unwrap_or_ice_msg("the lexer and the parser are arguing about the definition of an integer literal"))),
            TokenType::BoolLiteral => Ok(Literal::Bool(owned.raw.parse().unwrap_or_ice_msg("the lexer and the parser can't agree on what a boolean literal is"))),
            TokenType::CharLiteral => Ok(Literal::Char(unescape(owned.raw).unwrap_or_ice_msg("the lexer and the parser are clashing about the definition of an escape sequence").parse().unwrap_or_ice_msg("the lexer and the parser are debating the definition of a character literal"))),
            TokenType::StringLiteral => Ok(Literal::Str(unescape(owned.raw).unwrap_or_ice_msg("the lexer and the parser are having a heated argument about what an escape sequence").parse().unwrap_or_ice_msg("the lexer and the parser are fighting about the definition of a string literal"))),
            _ => Err(owned.into_error(self.source, ErrorKind::Expected { expected: [TokenType::IntLiteral, TokenType::BoolLiteral, TokenType::CharLiteral, TokenType::FloatLiteral, TokenType::StringLiteral].into() })),
        })
    }

    fn fn_call(&mut self) -> Result<FnCall> {
        let name = self.ident()?;
        let opening = self.token(TokenType::LParen)?;
        let mut args = Vec::new();
        loop {
            if self.current_is_token(TokenType::RParen).unwrap_or(false) {
                self.take();
                break Ok(FnCall { name, args });
            }
            args.push(self.expr()?);
            if !ok_or_else!(self.current_is_token(TokenType::RParen), {
                opening.into_error(
                    self.source,
                    ErrorKind::Unclosed {
                        unclosed: TokenType::LParen,
                    },
                )
            }) {
                self.token(TokenType::Comma)?;
            }
        }
    }

    fn expr(&mut self) -> Result<Expr> {
        let current = self.current().ok_or_else(|| {
            Error::eof(
                self.source,
                [
                    TokenType::FloatLiteral,
                    TokenType::IntLiteral,
                    TokenType::BoolLiteral,
                    TokenType::CharLiteral,
                    TokenType::StringLiteral,
                    TokenType::Ident,
                ]
                .into(),
            )
        })?;
        match &current.token {
            TokenType::FloatLiteral
            | TokenType::IntLiteral
            | TokenType::BoolLiteral
            | TokenType::CharLiteral
            | TokenType::StringLiteral => self.literal().map(Expr::Literal),
            TokenType::Ident => match self.peek(1) {
                Some(inner) => match inner.token {
                    TokenType::LParen => self.fn_call().map(Expr::FnCall),
                    _ => self.ident().map(Expr::Ident),
                },
                _ => self.ident().map(Expr::Ident),
            },
            // SAFETY: we know the current token exists, because we are matching on it
            _ => Err(unsafe { self.take_unchecked() }.into_error(
                self.source,
                ErrorKind::Expected {
                    expected: [
                        TokenType::FloatLiteral,
                        TokenType::IntLiteral,
                        TokenType::BoolLiteral,
                        TokenType::CharLiteral,
                        TokenType::StringLiteral,
                        TokenType::Ident,
                    ]
                    .into(),
                },
            )),
        }
    }

    fn fn_decl(&mut self) -> Result<FnDecl> {
        self.token(TokenType::KeywordFn)?;
        let name = self.ident()?;
        let opening = self.token(TokenType::LParen)?;
        let mut args = Vec::new();
        loop {
            if self.current_is_token(TokenType::RParen).unwrap_or(false) {
                break;
            }
            let arg = self.ident()?;
            self.token(TokenType::Colon)?;
            let ty = self.ident()?;
            args.push((arg, ty));
            if !ok_or_else!(self.current_is_token(TokenType::RParen), {
                opening.into_error(
                    self.source,
                    ErrorKind::Unclosed {
                        unclosed: TokenType::LParen,
                    },
                )
            }) {
                self.token(TokenType::Comma)?;
            }
        }
        self.token(TokenType::RParen)?;
        self.token(TokenType::Arrow)?;
        let ret = self.ident()?;
        let block = self.block()?;

        Ok(FnDecl {
            name,
            args,
            ret,
            block,
        })
    }

    fn var(&mut self) -> Result<Var> {
        let current = self.current().ok_or_else(|| {
            Error::eof(
                self.source,
                [
                    TokenType::KeywordLet,
                    TokenType::KeywordMut,
                    TokenType::Ident,
                ]
                .into(),
            )
        })?;
        match current.token {
            TokenType::KeywordLet => {
                self.take();
                let name = self.ident()?;
                self.token(TokenType::Equals)?;
                let value = self.expr()?;
                Ok(Var::Let(name, value))
            }
            TokenType::KeywordMut => {
                self.take();
                let name = self.ident()?;
                self.token(TokenType::Equals)?;
                let value = self.expr()?;
                Ok(Var::Mut(name, value))
            }
            TokenType::Ident => {
                let name = self.ident()?;
                self.token(TokenType::Equals)?;
                let value = self.expr()?;
                Ok(Var::ReAssign(name, value))
            }
            // SAFETY: we know the current token exists, because we are matching on it
            _ => Err(unsafe { self.take_unchecked() }.into_error(
                self.source,
                ErrorKind::Expected {
                    expected: [
                        TokenType::KeywordLet,
                        TokenType::KeywordMut,
                        TokenType::Ident,
                    ]
                    .into(),
                },
            )),
        }
    }

    fn if_stmt(&mut self) -> Result<If> {
        self.token(TokenType::KeywordIf)?;
        let if_stmt = (self.expr()?, self.block()?);
        let mut else_ifs = Vec::new();
        loop {
            self.token(TokenType::KeywordElse)?;
            if self
                .current_is_token(TokenType::KeywordIf)
                .ok_or_else(|| Error::eof(self.source, [TokenType::KeywordIf].into()))?
            {
                else_ifs.push((self.expr()?, self.block()?));
            } else {
                break Ok(If {
                    if_stmt,
                    else_ifs,
                    else_block: self.block()?,
                });
            }
        }
    }

    fn answer(&mut self) -> Result<Answer> {
        self.token(TokenType::KeywordAnswer)?;
        Ok(Answer { expr: self.expr()? })
    }

    fn stmt(&mut self) -> Result<Stmt> {
        match self
            .current()
            .ok_or_else(|| Error::eof(self.source, [TokenType::Error].into()))?
            .token
        {
            TokenType::KeywordIf => self.if_stmt().map(Stmt::If),
            TokenType::KeywordFn => self.fn_decl().map(Stmt::Fn),
            TokenType::KeywordLet | TokenType::KeywordMut => self.var().map(Stmt::Var),
            TokenType::Ident if self.peek_is_token(1, TokenType::Equals).unwrap_or(false) => {
                self.var().map(Stmt::Var)
            }
            TokenType::KeywordAnswer => self.answer().map(Stmt::Answer),
            _ => self.expr().map(Stmt::Expr),
        }
    }

    fn block(&mut self) -> Result<Block> {
        let opening = self.token(TokenType::LBrace)?;
        let mut stmts = Vec::new();
        while !ok_or_else!(self.current_is_token(TokenType::RBrace), {
            opening.into_error(
                self.source,
                ErrorKind::Unclosed {
                    unclosed: TokenType::LBrace,
                },
            )
        }) {
            stmts.push(self.stmt()?);
        }
        self.token(TokenType::RBrace)?;
        Ok(Block(stmts))
    }

    fn take(&mut self) -> Option<Token> {
        self.tokens.pop_front()
    }

    unsafe fn take_unchecked(&mut self) -> Token {
        // SAFETY: left up to the caller
        unsafe { self.take().unwrap_unchecked() }
    }

    fn current(&self) -> Option<&Token> {
        self.tokens.get(0)
    }

    fn current_is_token(&self, token: TokenType) -> Option<bool> {
        self.current().map(|inner| inner.token == token)
    }

    fn peek(&self, n: usize) -> Option<&Token> {
        self.tokens.get(n)
    }

    fn peek_is_token(&self, n: usize, token: TokenType) -> Option<bool> {
        self.peek(n).map(|inner| inner.token == token)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parser() {
        let program = r#"
fn a(r: int) -> nothing {
    mut x = 1
    x = 2
    answer x
}

let r = "hello"
let s = "hello\n"
let t = 'a'
let u = '\n'
"#;
        let ast = parse_text(program);
        assert_eq!(
            ast.map_err(|err| err.to_string()),
            Ok(Program(vec![
                Stmt::Fn(FnDecl {
                    name: "a".into(),
                    args: vec![("r".into(), "int".into())],
                    ret: "nothing".into(),
                    block: Block(vec![
                        Stmt::Var(Var::Mut("x".into(), Expr::Literal(Literal::Int(1)))),
                        Stmt::Var(Var::ReAssign("x".into(), Expr::Literal(Literal::Int(2)))),
                        Stmt::Answer(Answer {
                            expr: Expr::Ident("x".into())
                        })
                    ],)
                }),
                Stmt::Var(Var::Let(
                    "r".into(),
                    Expr::Literal(Literal::Str("hello".into()))
                )),
                Stmt::Var(Var::Let(
                    "s".into(),
                    Expr::Literal(Literal::Str("hello\n".into()))
                )),
                Stmt::Var(Var::Let("t".into(), Expr::Literal(Literal::Char('a')))),
                Stmt::Var(Var::Let("u".into(), Expr::Literal(Literal::Char('\n'))))
            ]))
        );
    }
}
