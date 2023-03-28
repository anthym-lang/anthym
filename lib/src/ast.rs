use crate::ice::ice;
use crate::token::{Token, TokenType};
use std::borrow::Cow;
use std::iter::FromIterator;

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Type {
    pub(crate) base: Ident,
    pub(crate) generics: Vec<Type>,
}

// FIXME: these from impls do not validate their input
impl From<&str> for Type {
    fn from(value: &str) -> Self {
        if let Some((base, generics)) = value.split_once('<') {
            let generics = &generics[..generics.len() - 1];
            Self {
                base: base.into(),
                generics: generics
                    .split(',')
                    .map(|generic| generic.trim().into())
                    .collect(),
            }
        } else {
            Self {
                base: value.into(),
                generics: Vec::new(),
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Literal {
    Float(f32),
    Int(i32),
    Bool(bool),
    Char(char),
    Str(String),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub(crate) struct Ident(pub(crate) Cow<'static, str>);

impl From<&str> for Ident {
    fn from(value: &str) -> Self {
        Self(Cow::Owned(value.to_owned()))
    }
}

impl From<String> for Ident {
    fn from(value: String) -> Self {
        Self(Cow::Owned(value))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct FnCall {
    pub(crate) name: Ident,
    pub(crate) args: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Op {
    Eq,
    Ne,
    Lt,
    Lte,
    Gt,
    Gte,
    Add,
    Sub,
    Div,
    Mul,
    Not,
}

impl Op {
    pub(crate) fn from_token(value: Token) -> Self {
        match value.token {
            TokenType::Equality => Self::Eq,
            TokenType::Inequality => Self::Ne,
            TokenType::LessThan => Self::Lt,
            TokenType::LessThanEqualTo => Self::Lte,
            TokenType::GreaterThan => Self::Gt,
            TokenType::GreaterThanEqualTo => Self::Gte,
            TokenType::Plus => Self::Add,
            TokenType::Minus => Self::Sub,
            TokenType::Slash => Self::Div,
            TokenType::Asterisk => Self::Mul,
            TokenType::Bang => Self::Not,
            other => ice(format!("invalid op: {other}").as_str()),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Expr {
    Literal(Literal),
    FnCall(FnCall),
    Ident(Ident),
    Binary(Box<Expr>, Op, Box<Expr>),
    Unary(Op, Box<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct FnDecl {
    pub(crate) name: Ident,
    pub(crate) args: Vec<(Ident, Type)>,
    pub(crate) ret: Type,
    pub(crate) block: Block,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Var {
    Let(Ident, Type, Expr),
    Mut(Ident, Type, Expr),
    ReAssign(Ident, Expr),
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct If {
    pub(crate) if_stmt: (Expr, Block),
    pub(crate) else_ifs: Vec<(Expr, Block)>,
    pub(crate) else_block: Block,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Return {
    pub(crate) expr: Expr,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Stmt {
    Var(Var),
    If(If),
    Return(Return),
    Expr(Expr),
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Block(pub(crate) Vec<Stmt>);
impl FromIterator<Stmt> for Block {
    fn from_iter<T: IntoIterator<Item = Stmt>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Program(pub(crate) Vec<FnDecl>);
impl FromIterator<FnDecl> for Program {
    fn from_iter<T: IntoIterator<Item = FnDecl>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}
