use std::{borrow::Cow, iter::FromIterator};

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
pub(crate) enum Expr {
    Literal(Literal),
    FnCall(FnCall),
    Ident(Ident),
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct FnDecl {
    pub(crate) name: Ident,
    pub(crate) args: Vec<(Ident, Ident)>,
    pub(crate) ret: Ident,
    pub(crate) block: Block,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Var {
    Let(Ident, Expr),
    Mut(Ident, Expr),
    ReAssign(Ident, Expr),
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct If {
    pub(crate) if_stmt: (Expr, Block),
    pub(crate) else_ifs: Vec<(Expr, Block)>,
    pub(crate) else_block: Block,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Answer {
    pub(crate) expr: Expr,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Stmt {
    Fn(FnDecl),
    Var(Var),
    If(If),
    Answer(Answer),
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
pub(crate) struct Program(pub(crate) Vec<Stmt>);
impl FromIterator<Stmt> for Program {
    fn from_iter<T: IntoIterator<Item = Stmt>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}
