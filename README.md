# Anthym
A programming language that can be ran with a JIT compiler, or with an AOT compiler. Uses the [`cranelift`](https://lib.rs/cranelift/) crate to generate object files and for JIT compilation. It uses a handwritten parser, but the lexer was generated with [`logos`](https://lib.rs/logos/).

## Getting Started
How to set up.

### Prequisites
* Git
* [The Rust Toolchain](https://rustup.rs/)

### Installation
Clone the repository:
```
git clone https://github.com/anthym-lang/anthym.git
```
Then install with `cargo` (don't forget to `cd` into the directory first):
```
cargo install --path cli
```
Now create a file called `hello.an`. It will contaian the following:
```rust
fn main() -> int {
    print("Hello, World!")
    return 0
}
```
You can run it with the JIT compiler, like so:
```
anthym run hello.an
```
Or, compile it:
```
anthym build hello.an -o hello
```
The built executable is in the `output/` directory


## Built With
* [`cranelift`](https://lib.rs/cranelift/)
* [`logos`](https://lib.rs/logos/)
* [The Rust Programming Language](https://www.rust-lang.org/)

## License
This project is licensed under the MIT license - please see the [LICENSE](LICENSE) file for more details.

## Acknowledgements
* Robert Nystrom and his book, [Crafting Interpreters](https://craftinginterpreters.com/)
