#!/bin/sh

set -ex

RUST_BACKTRACE=1 cargo run --bin caer_compiler -- $1

llvm-as -o /dev/null out/unopt.ll
llc -o out/unopt.s out/unopt.bc
clang -L ../target/debug/deps out/unopt.s -Wl,-Tlink.ld -lcaer_runtime -lm -o out/unopt

llvm-as -o /dev/null out/opt.ll
llc -o out/opt.s out/opt.bc
clang -L ../target/debug/deps out/opt.s -Wl,-Tlink.ld -lcaer_runtime -lm -o out/opt

LD_LIBRARY_PATH=../target/debug/deps rust-gdb out/opt
