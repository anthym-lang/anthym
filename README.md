# Rustic
A toy language that can be ran with a JIT compiler, or with an AOT compiler. Uses the [`cranelift`](https://lib.rs/cranelift/) crate to generate object files and for JIT compilation. It uses a handwritten parser, but the lexer was generated with [`logos`](https://lib.rs/logos/).

## Getting Started
How to set up.

### Prequisites
* Git
* [The Rust Toolchain](https://rustup.rs/)

### Installation
Clone the repository.
```console
~ $ git clone https://github.com/Phil-Gates/rustic
```
Then install with `cargo` (don't forget to `cd` into the directory first).
```console
rustic $ cargo install --path cli
```
You now have the CLI installed! You can get help from running the CLI with the `--help` flag, like so:
```console
rustic $ rustic --help
```

## Built With
* [`cranelift`](https://lib.rs/cranelift/)
* [`logos`](https://lib.rs/logos/)
* [The Rust Programming Language](https://www.rust-lang.org/)

## License
This project is licensed under the MIT license - please see the [LICENSE](LICENSE) file for more details.
