use crate::ast::Ident;
use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
    pub(crate) static ref BUILTINS: HashMap<Ident, (Vec<Ident>, Ident)> = HashMap::from([
        ("print_char".into(), (vec!["char".into()], "char".into())),
        ("print".into(), (vec!["string".into()], "string".into()))
    ]);
}

pub(crate) const BUILTINS_SRC: &[u8] = include_bytes!("builtins.c");

extern "C" {
    pub(crate) fn print(value: *const i8) -> *const i8;
    pub(crate) fn print_char(value: i8) -> i8;
}
