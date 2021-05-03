#!/bin/sh

set -ex

RUST_BACKTRACE=1 cargo run -- $1

llvm-as-10 -o /dev/null out/unopt.ll
llc-10 -o out/unopt.s out/unopt.bc
clang-10 -L ../target/debug/deps out/unopt.s -Wl,-Tlink.ld -lcaer_runtime -lm -o out/unopt

llvm-as-10 -o /dev/null out/opt.ll
llc-10 -o out/opt.s out/opt.bc
clang-10 -L ../target/debug/deps out/opt.s -Wl,-Tlink.ld -lcaer_runtime -lm -o out/opt

LD_LIBRARY_PATH=../target/debug/deps lldb out/opt
