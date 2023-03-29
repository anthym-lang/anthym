fn main() {
    cc::Build::new()
        .file("src/stdlib/prelude.c")
        .compile("prelude");
}
