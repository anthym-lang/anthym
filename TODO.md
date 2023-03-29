# TODO:
Emphasized = currently working on

## Features
 * Arrays
 * **Operations on arrays and strings**
 * Static typing
 * More builtins
 * Test WASM-ness
 * Compile to bytecode
 * Optimize bytecode
 * JIT / compile to binary & link with cranelift
   * Support else ifs
 * Nicer standard library
 * **Write docs and more tests, and start writing book**

## Refactors
* Stop using anyhow, switch to thiserror
* Rewrite evaluator, maybe with register machine
* Merge error types for nicer handling
* Nicer error messages by storing info in AST (+ custom error type and macros)
* Stop using impl trait
