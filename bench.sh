#!/bin/bash

shopt -s nullglob

if [[ $OSTYPE == 'darwin'* ]]; then
  GCC=gcc
fi

if [[ $OSTYPE == 'linux'* ]]; then
  GCC="gcc -no-pie -z noexecstack"
fi


for f in tests/benchmarks/*.rs; do
    echo "$f"
    file=`basename $f .rs`
    
    ./target/debug/sam_rust_compiler tests/benchmarks/$file.rs -O1 >> /dev/null
    $GCC tests/benchmarks/$file.s >> /dev/null
    mv tests/benchmarks/$file.s tests/benchmark/$file.opt.s
    mv a.out a.opt.out
  
    ./target/debug/sam_rust_compiler tests/benchmarks/$file.rs >> /dev/null
    $GCC tests/benchmarks/$file.s >> /dev/null

    rustc -O tests/benchmarks/$file.rs >> /dev/null 2>&1
    echo " -- $file with rustc -O"
    echo " -- rustc -O" > tests/benchmarks/$file.perf
    \time -a -o tests/benchmarks/$file.perf ./$file

    rustc tests/benchmarks/$file.rs >> /dev/null 2>&1
    echo " -- $file with rustc"
    echo " -- rustc" >> tests/benchmarks/$file.perf
    \time -a -o tests/benchmarks/$file.perf ./$file
    rm $file

    echo " -- $file with SamRustCompiler"
    echo " -- SamRustCompiler" >> tests/benchmarks/$file.perf
    \time -a -o tests/benchmarks/$file.perf ./a.out
    echo "\nNb lines" >> tests/benchmarks/$file.perf
    wc -l tests/benchmarks/$file.s >> tests/benchmarks/$file.perf

    echo " -- $file with SamRustCompiler -O1"
    echo " -- SamRustCompiler" >> tests/benchmarks/$file.perf
    \time -a -o tests/benchmarks/$file.perf ./a.opt.out
    echo "\nNb lines" >> tests/benchmarks/$file.perf
    wc -l tests/benchmarks/$file.opt.s >> tests/benchmarks/$file.perf
done

wc -l tests/exec/*.s > tests/exec/global.perf
