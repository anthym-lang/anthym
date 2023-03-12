use crate::ast::Ident;
use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
    pub(crate) static ref BUILTINS: HashMap<Ident, (Vec<Ident>, Ident)> =
        HashMap::from([("print_char".into(), (vec!["char".into()], "char".into()))]);
}

pub(crate) extern "C" fn print_char(value: i8) -> i8 {
    println!("{}", value as u8 as char);
    value
}
