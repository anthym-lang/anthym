use std::fmt::Display;
use std::iter::FromIterator;

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Float(f32),
    Number(i32),
    Bool(bool),
    Char(char),
    Str(String),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Ident(pub Box<str>);
impl From<&str> for Ident {
    fn from(value: &str) -> Self {
        Self(value.to_owned().into_boxed_str())
    }
}
impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Type(pub Box<str>);
impl From<&str> for Type {
    fn from(value: &str) -> Self {
        Self(value.to_owned().into_boxed_str())
    }
}

impl From<String> for Ident {
    fn from(value: String) -> Self {
        Self(value.into_boxed_str())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Literal(Literal),
    FnCall(Ident, Vec<Expr>),
    Ident(Ident),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnDecl(pub Ident, pub Vec<(Ident, Type)>, pub Type, pub Block);

#[derive(Debug, PartialEq, Clone)]
pub enum VarDecl {
    Let(Ident, Expr),
    Mut(Ident, Expr),
    ReAssign(Ident, Expr),
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfStmt(pub Vec<(Expr, Block)>);
impl FromIterator<(Expr, Block)> for IfStmt {
    fn from_iter<T: IntoIterator<Item = (Expr, Block)>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Fn(FnDecl),
    Var(VarDecl),
    If(IfStmt),
    Answer(Expr),
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
