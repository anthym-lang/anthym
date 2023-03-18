# Rustic
A toy language that can be ran with a JIT compiler, or with an AOT compiler. Uses the [`cranelift`](https://lib.rs/cranelift/) crate to generate object files and for JIT compilation. It uses a handwritten parser, but the lexer was generated with [`logos`](https://lib.rs/logos/).

## Getting Started
How to set up.

### Prequisites
* Git
* [The Rust Toolchain](https://rustup.rs/)

### Installation
Clone the repository.
```bash
git clone https://github.com/Phil-Gates/rustic
```
Then install with `cargo`.
```bash
cargo install --path cli
```
You now have the CLI installed! You can get help from running the CLI with the `--help` flag, like so:
```bash
rustic --help
```

## Built With
* [`cranelift`](https://lib.rs/cranelift/)
* [`logos`](https://lib.rs/logos/)
* [The Rust Programming Language](https://www.rust-lang.org/)

## License
This project is licensed under the MIT license - please see the [LICENSE](LICENSE) file for more details.