use crate::ast::*;
use crate::error::{NiceError, Result};
use crate::lex::lex;
use crate::token::{SpannedToken, Token};
use std::collections::VecDeque;
use std::iter::from_fn;

pub(crate) fn parse_text(source: impl AsRef<str>) -> anyhow::Result<Program> {
    let mut parser = Parser::new(source.as_ref())?;
    Ok(parser.program()?)
}

#[derive(Debug)]
struct Parser<'a> {
    source: &'a str,
    tokens: VecDeque<SpannedToken>,
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
            if self.current().is_ok() {
                Some(self.stmt())
            } else {
                None
            }
        })
        .collect()
    }

    fn token(&mut self, token: Token) -> Result<SpannedToken> {
        self.take()
            .map_err(|err| err.msg(format!("expected {token:?}, got EOF")))
            .and_then(|owned| {
                if owned.token == token {
                    Ok(owned)
                } else {
                    Err(owned.as_error(
                        self.source,
                        format!("expected {token:?}, got {other:?}", other = owned.token),
                    ))
                }
            })
    }

    fn ident(&mut self) -> Result<Ident> {
        self.take()
            .map_err(|err| err.msg("expected Ident, got EOF"))
            .and_then(|owned| match owned.token {
                Token::Ident(ident) => Ok(ident.into()),
                ref other => {
                    Err(owned.as_error(self.source, format!("expected Ident, got {other:?}")))
                }
            })
    }

    fn literal(&mut self) -> Result<Literal> {
        self.take()
            .map_err(|err| err.msg("expected literal, got EOF"))
            .and_then(|owned| match owned.token {
                Token::FloatLiteral(float) => Ok(Literal::Float(float)),
                Token::IntLiteral(int) => Ok(Literal::Int(int)),
                Token::BoolLiteral(bool) => Ok(Literal::Bool(bool)),
                Token::CharLiteral(ch) => Ok(Literal::Char(ch)),
                Token::StringLiteral(s) => Ok(Literal::Str(s)),
                ref other => {
                    Err(owned.as_error(self.source, format!("expected literal, got {other:?}")))
                }
            })
    }

    fn fn_call(&mut self) -> Result<FnCall> {
        let name = self.ident()?;
        let opening = self.token(Token::LParen)?;
        let mut args = Vec::new();
        loop {
            if self.current_is_token(Token::RParen).unwrap_or(false) {
                self.take()?;
                break Ok(FnCall { name, args });
            }
            args.push(self.expr()?);
            if !self.current_is_token(Token::RParen).map_err(|_| {
                opening.as_error(self.source, "(while parsing function call) unclosed LParen")
            })? {
                self.token(Token::Comma)?;
            }
        }
    }

    fn expr(&mut self) -> Result<Expr> {
        let peek = self
            .current()
            .map_err(|err| err.msg("expected expr, found eof"))?;
        match &peek.token {
            Token::FloatLiteral(_)
            | Token::IntLiteral(_)
            | Token::BoolLiteral(_)
            | Token::CharLiteral(_)
            | Token::StringLiteral(_) => self.literal().map(Expr::Literal),
            Token::Ident(_) => match self.peek(1).ok() {
                Some(inner) => match inner.token {
                    Token::LParen => self.fn_call().map(Expr::FnCall),
                    _ => self.ident().map(Expr::Ident),
                },
                _ => self.ident().map(Expr::Ident),
            },
            other => Err(peek.as_error(
                self.source,
                format!("(while parsing an expression) expected Literal or Ident, found {other:?}"),
            )),
        }
    }

    fn fn_decl(&mut self) -> Result<FnDecl> {
        self.token(Token::KeywordFn)?;
        let name = self.ident()?;
        let opening = self.token(Token::LParen)?;
        let mut args = Vec::new();
        loop {
            if self.current_is_token(Token::RParen).unwrap_or(false) {
                break;
            }
            let arg = self.ident()?;
            self.token(Token::Colon)?;
            let ty = self.ident()?;
            args.push((arg, ty));
            if !self.current_is_token(Token::RParen).map_err(|_| {
                opening.as_error(
                    self.source,
                    "(while parsing a function declaration) unclosed LParen",
                )
            })? {
                self.token(Token::Comma)?;
            }
        }
        self.token(Token::RParen)?;
        self.token(Token::Arrow)?;
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
        let current = self.current()
            .map_err(|err| err.msg("expected variable statement, found EOF"))?;
        match current 
            .token
        {
            Token::KeywordLet => {
                self.take()?;
                let name = self.ident()?;
                self.token(Token::Equals)?;
                let value = self.expr()?;
                Ok(Var::Let(name, value))
            }
            Token::KeywordMut => {
                self.take()?;
                let name = self.ident()?;
                self.token(Token::Equals)?;
                let value = self.expr()?;
                Ok(Var::Mut(name, value))
            }
            Token::Ident(_) => {
                let name = self.ident()?;
                self.token(Token::Equals)?;
                let value = self.expr()?;
                Ok(Var::ReAssign(name, value))
            }
            ref other => Err(current.as_error(self.source, format!("(while parsing variable statement) expected `let`, `mut`, or ident, found {other:?}")))
        }
    }

    fn if_stmt(&mut self) -> Result<If> {
        self.token(Token::KeywordIf)?;
        let if_stmt = (self.expr()?, self.block()?);
        let mut else_ifs = Vec::new();
        loop {
            self.token(Token::KeywordElse)?;
            if self.current_is_token(Token::KeywordIf).map_err(|err| {
                err.msg("(while parsing if statement) expected KeywordIf or block, found EOF")
            })? {
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
        self.token(Token::KeywordAnswer)?;
        Ok(Answer { expr: self.expr()? })
    }

    fn stmt(&mut self) -> Result<Stmt> {
        match self
            .current()
            .map_err(|err| {
                err.msg(
                    "(while parsing statment) expected an if statement, a function declaration, a variable statement, or an expression, found EOF",
                )
            })?
            .token
        {
            Token::KeywordIf => self.if_stmt().map(Stmt::If),
            Token::KeywordFn => self.fn_decl().map(Stmt::Fn),
            Token::KeywordLet | Token::KeywordMut => self.var().map(Stmt::Var),
            Token::Ident(_) if self.peek_is_token(1, Token::Equals)? => self.var().map(Stmt::Var),
            Token::KeywordAnswer => self.answer().map(Stmt::Answer),
            _ => self.expr().map(Stmt::Expr),
        }
    }

    fn block(&mut self) -> Result<Block> {
        let opening = self.token(Token::LBrace)?;
        let mut stmts = Vec::new();
        while !self
            .current_is_token(Token::RBrace)
            .map_err(|_| opening.as_error(self.source, "(while parsing block) unclosed LBrace"))?
        {
            stmts.push(self.stmt()?);
        }
        self.token(Token::RBrace)?;
        Ok(Block(stmts))
    }

    fn take(&mut self) -> Result<SpannedToken> {
        self.tokens
            .pop_front()
            .ok_or_else(|| NiceError::eof(self.source, "unexpected EOF"))
    }

    fn current(&self) -> Result<&SpannedToken> {
        self.tokens
            .get(0)
            .ok_or_else(|| NiceError::eof(self.source, "unexpected EOF"))
    }

    fn current_is_token(&self, token: Token) -> Result<bool> {
        self.current().map(|inner| inner.token == token)
    }

    fn peek(&self, n: usize) -> Result<&SpannedToken> {
        self.tokens
            .get(n)
            .ok_or_else(|| NiceError::eof(self.source, "unexpected EOF"))
    }

    fn peek_is_token(&self, n: usize, token: Token) -> Result<bool> {
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
            ])
        ));
    }
}
