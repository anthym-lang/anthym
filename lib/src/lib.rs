#![warn(clippy::unwrap_used, clippy::unreachable)]
#![deny(
    rust_2018_idioms,
    unreachable_pub,
    missing_debug_implementations,
    clippy::undocumented_unsafe_blocks
)]
mod ast;
mod error;
mod expects;
pub mod fmt;
mod ice;
pub mod jit;
mod lex;
mod ops;
mod parse;
mod stdlib;
mod token;
mod unescape;

// ------------------ Code for the old tree-walk interpreter ------------------
// #![feature(unboxed_closures, fn_traits)]
// mod builtins;
// mod value;
// pub mod run;
// mod env;
// mod function;
