#![warn(clippy::unwrap_used, clippy::unreachable)]
#![deny(
    rust_2018_idioms,
    unreachable_pub,
    missing_debug_implementations,
    clippy::undocumented_unsafe_blocks
)]
#![feature(unboxed_closures, fn_traits)]
mod ast;
mod builtins;
mod env;
mod error;
pub mod fmt;
mod function;
mod ice;
mod lex;
mod parse;
pub mod run;
mod token;
mod unescape;
mod value;
