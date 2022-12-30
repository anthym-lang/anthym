#![feature(
    iter_array_chunks,
    unboxed_closures,
    fn_traits,
    try_find,
    iterator_try_collect
)]
mod ast;
mod env;
pub mod fmt;
mod function;
mod parse;
pub mod run;
mod stdlib;
