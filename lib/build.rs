fn main() {
    cc::Build::new()
        .file("src/std/prelude.c")
        .compile("prelude");
}
