fn main() {
    cc::Build::new().file("src/builtins.c").compile("builtins");
}
