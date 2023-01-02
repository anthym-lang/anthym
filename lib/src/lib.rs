#![feature(unboxed_closures, fn_traits)]
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
