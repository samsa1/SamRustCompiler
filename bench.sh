#!/bin/sh

./target/debug/sam_rust_compiler tests/benchmarks/$1.rs >> /dev/null
 gcc tests/benchmarks/$1.s >> /dev/null
rustc -O tests/benchmarks/$1.rs >> /dev/null
echo "Testing $1 with rustc -O"
time ./$1

rustc tests/benchmarks/$1.rs >> /dev/null
echo "Testing $1 with rustc"
time ./$1

echo "Testing $1 with SamRustCompiler"
time ./a.out
