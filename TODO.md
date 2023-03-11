# TODO:
Emphasized = currently working on

## Features
 * Binary expressions
 * Static typing
 * More builtins
 * Test WASM-ness
 * Compile to bytecode
 * Optimize bytecode
 * JIT w/ bytecode?????

## Refactors
* Stop using anyhow, switch to thiserror
* Rewrite evaluator, maybe with register machine
* Merge error types for nicer handling
* Nicer error messages by storing info in AST (+ custom error type and macros)
