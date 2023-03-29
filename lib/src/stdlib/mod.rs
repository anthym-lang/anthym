use crate::ast::{Ident, Type};
use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
    pub(crate) static ref PRELUDE: HashMap<Ident, (Vec<Type>, Type)> = HashMap::from([
        ("print_char".into(), (vec!["char".into()], "char".into())),
        ("print".into(), (vec!["string".into()], "string".into()))
    ]);
}

pub(crate) const PRELUDE_SRC: &[u8] = include_bytes!("prelude.c");

extern "C" {
    pub(crate) fn print(value: *const i8) -> *const i8;
    pub(crate) fn print_char(value: i8) -> i8;
}
