use crate::ast::*;
use crate::error::{Error, ErrorKind, Result};
use crate::expects::*;
use crate::ice::IceExt;
use crate::lex::Lexer;
use crate::token::{Token, TokenType};
use crate::unescape::unescape;
use peekmore::{PeekMore, PeekMoreIterator};
use std::iter::from_fn;

macro_rules! ok_or_else {
    ($expr: expr, $or_else: block) => {
        match $expr {
            Some(inner) => inner,
            None => return Err($or_else),
        }
    };
}

macro_rules! grammar {
    {
        $(
            $rule:ident -> $inner:ident ( ( $token:ident $(| $tokens:ident)* ) $inner2:ident )* ;
        )+
    } => {
        $(
            fn $rule(&mut self) -> Result<Expr> {
                let mut expr = self.$inner()?;

                while self.current_is_token(TokenType::$token).unwrap_or(false) $(|| self.current_is_token(TokenType::$tokens).unwrap_or(false))* {
                    let op = self.take_ice()?;
                    let right = self.$inner()?;
                    expr = Expr::Binary(expr.into(), Op::from_token(op), right.into());
                }

                Ok(expr)
            }
        )+
    };
}

pub(crate) fn parse_text(source: impl AsRef<str>) -> anyhow::Result<Program> {
    let mut parser = Parser::new(source.as_ref())?;
    Ok(parser.program()?)
}

#[derive(Debug)]
struct Parser<'a> {
    source: &'a str,
    lexer: PeekMoreIterator<Lexer<'a>>,
    current: Option<Token>,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(source: &'a str) -> Result<Self> {
        Ok(Self {
            source,
            lexer: Lexer::new(source).peekmore(),
            current: None,
        })
    }

    pub(crate) fn program(&mut self) -> Result<Program> {
        from_fn(|| {
            // if the current token is none, then we might be on the first token
            if self.current().is_some() || self.peek().is_some() {
                Some(self.fn_decl())
            } else {
                None
            }
        })
        .collect()
    }

    fn token(&mut self, token: TokenType) -> Result<Token> {
        self.take()?
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

    fn type_(&mut self) -> Result<Type> {
        let base = self.ident()?;
        let mut generics = Vec::new();

        if let Some(true) = self.current_is_token(TokenType::LessThan) {
            self.take_ice()?;
            loop {
                generics.push(self.type_()?);
                if self
                    .current_is_token(TokenType::GreaterThan)
                    .ok_or_else(|| {
                        Error::eof(
                            self.source,
                            [TokenType::GreaterThan, TokenType::Comma].into(),
                        )
                    })?
                {
                    break;
                }
            }
            self.token(TokenType::GreaterThan)?;
        }

        Ok(Type { base, generics })
    }

    fn ident(&mut self) -> Result<Ident> {
        self.take()?
            .ok_or_else(|| Error::eof(self.source, IDENT.into()))
            .and_then(|owned| match owned.token {
                TokenType::Ident => Ok(owned.raw.into()),
                _ => Err(owned.into_error(
                    self.source,
                    ErrorKind::Expected {
                        expected: IDENT.into(),
                    },
                )),
            })
    }

    fn literal(&mut self) -> Result<Literal> {
        self.take()?
            .ok_or_else(|| Error::eof(self.source, LITERAL.into()))
            .and_then(|owned| match owned.token {
                TokenType::FloatLiteral => Ok(Literal::Float(owned.raw.parse().unwrap_or_ice())),
                TokenType::IntLiteral => Ok(Literal::Int(owned.raw.parse().unwrap_or_ice())),
                TokenType::BoolLiteral => Ok(Literal::Bool(owned.raw.parse().unwrap_or_ice())),
                TokenType::CharLiteral => Ok(Literal::Char(
                    unescape(owned.raw).unwrap_or_ice().parse().unwrap_or_ice(),
                )),
                TokenType::StringLiteral => Ok(Literal::Str(
                    unescape(owned.raw).unwrap_or_ice().parse().unwrap_or_ice(),
                )),
                _ => Err(owned.into_error(
                    self.source,
                    ErrorKind::Expected {
                        expected: LITERAL.into(),
                    },
                )),
            })
    }

    fn fn_call(&mut self) -> Result<FnCall> {
        let name = self.ident()?;
        let opening = self.token(TokenType::LParen)?;
        let mut args = Vec::new();
        loop {
            if self.current_is_token(TokenType::RParen).unwrap_or(false) {
                self.take_ice()?;
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
        self.equality()
    }

    grammar! {
        equality -> comparison ( (Inequality | Equality) comparison )*;
        comparison -> term ( (GreaterThan | GreaterThanEqualTo | LessThan | LessThanEqualTo) term )*;
        term -> factor ( (Minus | Plus) factor )*;
        factor -> unary ( (Slash | Asterisk) unary )*;
    }

    fn unary(&mut self) -> Result<Expr> {
        if self
            .current_is_token(TokenType::Bang)
            .or(self.current_is_token(TokenType::Minus))
            .unwrap_or(false)
        {
            let op = self.take_ice()?;
            let right = self.unary()?;
            Ok(Expr::Unary(Op::from_token(op), right.into()))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<Expr> {
        let current = self
            .current()
            .ok_or_else(|| Error::eof(self.source, EXPR.into()))?;

        match &current.token {
            literal if LITERAL.contains(literal) => self.literal().map(Expr::Literal),
            TokenType::Ident => match self.peek() {
                Some(inner) => match inner.token {
                    TokenType::LParen => self.fn_call().map(Expr::FnCall),
                    _ => self.ident().map(Expr::Ident),
                },
                _ => self.ident().map(Expr::Ident),
            },
            TokenType::LParen => {
                self.take_ice()?;
                let expr = self.expr();
                self.token(TokenType::RParen)?;
                expr
            }
            _ => Err(self.take_ice()?.into_error(
                self.source,
                ErrorKind::Expected {
                    expected: EXPR.into(),
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
            let ty = self.type_()?;
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
        let ret = self.type_()?;
        let block = self.block()?;

        Ok(FnDecl {
            name,
            args,
            ret,
            block,
        })
    }

    fn var(&mut self) -> Result<Var> {
        let current = self
            .current()
            .ok_or_else(|| Error::eof(self.source, VAR.into()))?;
        match current.token {
            TokenType::KeywordLet => {
                self.take()?;
                let name = self.ident()?;
                self.token(TokenType::Colon)?;
                let ty = self.type_()?;
                self.token(TokenType::Equals)?;
                let value = self.expr()?;
                Ok(Var::Let(name, ty, value))
            }
            TokenType::KeywordMut => {
                self.take()?;
                let name = self.ident()?;
                self.token(TokenType::Colon)?;
                let ty = self.type_()?;
                self.token(TokenType::Equals)?;
                let value = self.expr()?;
                Ok(Var::Mut(name, ty, value))
            }
            TokenType::Ident => {
                let name = self.ident()?;
                self.token(TokenType::Equals)?;
                let value = self.expr()?;
                Ok(Var::ReAssign(name, value))
            }
            _ => Err(self.take_ice()?.into_error(
                self.source,
                ErrorKind::Expected {
                    expected: VAR.into(),
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
                .ok_or_else(|| Error::eof(self.source, IF_STMT.into()))?
            {
                self.take_ice()?;
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

    fn return_(&mut self) -> Result<Return> {
        self.token(TokenType::KeywordReturn)?;
        Ok(Return { expr: self.expr()? })
    }

    fn stmt(&mut self) -> Result<Stmt> {
        let current = self
            .current()
            .ok_or_else(|| Error::eof(self.source, STMT.into()))?
            .token;

        match current {
            TokenType::KeywordIf => self.if_stmt().map(Stmt::If),
            TokenType::KeywordLet | TokenType::KeywordMut => self.var().map(Stmt::Var),
            TokenType::Ident if (self.peek_is_token(TokenType::Equals).unwrap_or(false)) => {
                self.var().map(Stmt::Var)
            }
            TokenType::KeywordReturn => self.return_().map(Stmt::Return),
            other if EXPR.contains(&other) => self.expr().map(Stmt::Expr),
            _ => Err(self.take_ice()?.into_error(
                self.source,
                ErrorKind::Expected {
                    expected: STMT.into(),
                },
            )),
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

    fn take(&mut self) -> Result<Option<Token>> {
        let owned = self
            .lexer
            .next()
            .map(|token| {
                if token.token == TokenType::Error {
                    return Err(token.into_error(self.source, ErrorKind::Unknown));
                }

                Ok(token)
            })
            .transpose()?;
        self.current = self.peek().cloned();
        self.lexer.advance_cursor();

        Ok(owned)
    }

    fn take_ice(&mut self) -> Result<Token> {
        Ok(self
            .take()?
            .unwrap_or_ice_msg("the parser thought there was a token when there wasn't"))
    }

    fn current(&self) -> Option<&Token> {
        self.current.as_ref()
    }

    fn current_is_token(&self, token: TokenType) -> Option<bool> {
        self.current().map(|inner| inner.token == token)
    }

    fn peek(&mut self) -> Option<&Token> {
        self.lexer.peek()
    }

    fn peek_is_token(&mut self, token: TokenType) -> Option<bool> {
        self.peek().map(|inner| inner.token == token)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parser() {
        let program = r#"
fn a(r: int) -> nothing {
    mut x: int = 1
    x = 2
    return x
}

fn main() -> nothing {
    let r: string = "hello"
    let s: string = "hello\n"
    let t: char = 'a'
    let u: char = '\n'
}
"#;
        let ast = parse_text(program);
        assert_eq!(
            ast.map_err(|err| err.to_string()),
            Ok(Program(vec![
                FnDecl {
                    name: "a".into(),
                    args: vec![("r".into(), "int".into())],
                    ret: "nothing".into(),
                    block: Block(vec![
                        Stmt::Var(Var::Mut(
                            "x".into(),
                            "int".into(),
                            Expr::Literal(Literal::Int(1))
                        )),
                        Stmt::Var(Var::ReAssign("x".into(), Expr::Literal(Literal::Int(2)))),
                        Stmt::Return(Return {
                            expr: Expr::Ident("x".into())
                        })
                    ],)
                },
                FnDecl {
                    name: "main".into(),
                    args: vec![],
                    ret: "nothing".into(),
                    block: Block(vec![
                        Stmt::Var(Var::Let(
                            "r".into(),
                            "string".into(),
                            Expr::Literal(Literal::Str("hello".into()))
                        )),
                        Stmt::Var(Var::Let(
                            "s".into(),
                            "string".into(),
                            Expr::Literal(Literal::Str("hello\n".into()))
                        )),
                        Stmt::Var(Var::Let(
                            "t".into(),
                            "char".into(),
                            Expr::Literal(Literal::Char('a'))
                        )),
                        Stmt::Var(Var::Let(
                            "u".into(),
                            "char".into(),
                            Expr::Literal(Literal::Char('\n'))
                        ))
                    ])
                }
            ]))
        );
    }
}
