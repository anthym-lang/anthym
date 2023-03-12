use crate::ast::Ident;
use crate::function::Function;
use std::fmt::{Debug, Display};

macro_rules! generate_from_impls {
    ($name: ty, $($variant: ident : $type: ty),+) => {
        $(
            impl From<$type> for $name {
                fn from(x: $type) -> Self {
                    Self::$variant(x)
                }
            }
        )+
    };
}

#[derive(Clone)]
pub(crate) enum Value {
    Float(f32),
    Number(i32),
    Bool(bool),
    Char(char),
    Str(String),
    Fn(Function),
    Type(Ident),
    Nothing,
}

generate_from_impls!(
    Value,
    Float: f32,
    Number: i32,
    Bool: bool,
    Char: char,
    Str: String,
    Fn: Function,
    Type: Ident
);

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Float(fl) => Debug::fmt(&fl, f),
            Self::Number(n) => Debug::fmt(&n, f),
            Self::Bool(b) => Debug::fmt(&b, f),
            Self::Char(c) => Debug::fmt(&c, f),
            Self::Str(s) => Debug::fmt(&s, f),
            Self::Fn(_) => {
                write!(f, "<function>")
            }
            Self::Type(t) => write!(f, "<type {t}>"),
            Self::Nothing => write!(f, "<nothing>"),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Float(fl) => Display::fmt(&fl, f),
            Self::Number(n) => Display::fmt(&n, f),
            Self::Bool(b) => Display::fmt(&b, f),
            Self::Char(c) => Display::fmt(&c, f),
            Self::Str(s) => Display::fmt(&s, f),
            Self::Fn(_) => {
                write!(f, "<function>")
            }
            Self::Type(t) => write!(f, "<type {t}>"),
            Self::Nothing => write!(f, "<nothing>"),
        }
    }
}
