# SamRustCompiler

## Presentation

The objective of this project is to implement our own bootstrapped rust compiler. Under the goal of learning more about compilers and programming languages during the quest towards this goal.

You are free to contribute to this project, or submit issues if you find bugs inside the compiler or wishes for a feature to be implemented.

However, please note that this compiler comes without any warranty whatsoever:

- The borrow checker is not implemented
- The compiler is only tested on programs valid for `rustc` and `SRC` or wrong for `rustc`. The compiler is never tested on cases handled by `rustc` and not `SRC`. However, you can send a push request adding such tests.

## Limitations

### Printing

Currently, you can only print a static string :

```rust
print!("Hello World")
```

However, `println` is not implemented and all printings valid in rust with a `{}` such as :

```rust
let a = "my string";
print!("{}", a) // This is not handled
```


### Deallocation

As this compiler does not have a borrow checker we cannot deallocate anything.


### Macros

Only two built-in macros are handled by the compiler : `vec!` and `print!`.

A few others are also handled and are here only for the purpose of helping the developers tests the compiler. As such they are not stable and should not be used!

### Std

The SRC standard library only has a few built-in functions about vectors.

The reason is that currently the compiler does not use the information from the std to type check your program


### Other Missing features

- Parametrized types
- We assume everything is public
- The `String` type
- Incremental compilation
- Floating point arithmetic

## Handled features

Most of the handled features are visible in the example [tests/exec/abr_method.rs](https://github.com/samsa1/SamRustCompiler/blob/main/tests/exec/abr_methods.rs).

Handled features include :

- Multiple integer sizes (`i8`, `u8`, ... `i64`, `u64`, `isize`, `usize`)
- Arithmetic and bitwise operations
- A bit of coercion
- A type inferencer
- Simple constants (too complex constant will raises as few `todo!()` in the compiler)
- Name spaces and submodules (every thing is public)
- Classes
- Implementations and methods
- Limited pattern matching (typing finished, backend under development)
- Custom allocator written in rust (heap of 8k bytes that is never freed, allocator interface is not type-checked)
- A backend that works for Linux and MacOS x86_64 (other backend are not developed due to a lack of hardware).

## Usage :

Build the compiler with the command `make`. The compiler is compiled twice in order to integrate the std inside the compiler.

The compiler also handles a few flags : 
- `--parse-only` to stop compilation after parsing
- `--type-only` to stop compilation after typing
- `--generate-std` to generate the `std_file.rs`

You should use `make clean` to clean the `std_file.rs` inside the compiler and format the code before sending a pull request please.


## Future features :

- More complex pattern matching
- Floating point arithmetic
- Functions parametrized by types
- Parametrized types
- New backends and directly producing binary
- More complex printing
- And so much more handled by `rustc`


