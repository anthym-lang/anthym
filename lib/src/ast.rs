use std::{borrow::Cow, iter::FromIterator};

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Float(f32),
    Int(i32),
    Bool(bool),
    Char(char),
    Str(String),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Ident(pub Cow<'static, str>);

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
pub struct FnCall {
    pub name: Ident,
    pub args: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Literal(Literal),
    FnCall(FnCall),
    Ident(Ident),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnDecl {
    pub name: Ident,
    pub args: Vec<(Ident, Ident)>,
    pub ret: Ident,
    pub block: Block,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Var {
    Let(Ident, Expr),
    Mut(Ident, Expr),
    ReAssign(Ident, Expr),
}

#[derive(Debug, PartialEq, Clone)]
pub struct If {
    pub ifs: Vec<(Expr, Block)>,
    pub else_block: Block,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Answer {
    pub expr: Expr,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Fn(FnDecl),
    Var(Var),
    If(If),
    Answer(Answer),
    Expr(Expr),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block(pub Vec<Stmt>);
impl FromIterator<Stmt> for Block {
    fn from_iter<T: IntoIterator<Item = Stmt>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

#[derive(Debug, PartialEq)]
pub struct Program(pub Vec<Stmt>);
impl FromIterator<Stmt> for Program {
    fn from_iter<T: IntoIterator<Item = Stmt>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}
