# TODO:
Emphasized = currently working on

## Features
 * Binary expressions
 * Static typing
 * More builtins
 * Test WASM-ness
 * Compile to bytecode
 * Optimize bytecode
 * **JIT / compile to binary & link with cranelift**

## Refactors
* Stop using anyhow, switch to thiserror
* Rewrite evaluator, maybe with register machine
* Merge error types for nicer handling
* Nicer error messages by storing info in AST (+ custom error type and macros)
