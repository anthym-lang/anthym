#![warn(clippy::unwrap_used, clippy::unreachable)]
#![deny(
    rust_2018_idioms,
    unreachable_pub,
    missing_debug_implementations,
    clippy::undocumented_unsafe_blocks
)]
// #![feature(unboxed_closures, fn_traits)]
mod ast;
mod builtins;
// mod builtins_deprecated;
// mod env;
mod error;
mod expects;
pub mod fmt;
// mod function;
mod ice;
pub mod jit;
mod lex;
mod parse;
// mod run_deprecated;
mod build;
mod token;
mod unescape;
// mod value;
