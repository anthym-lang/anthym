#![feature(
    iter_array_chunks,
    unboxed_closures,
    fn_traits,
    try_find,
    iterator_try_collect
)]
mod ast;
mod builtins;
mod env;
mod error;
pub mod fmt;
mod function;
mod lex;
mod parse;
pub mod run;
mod token;
mod value;
