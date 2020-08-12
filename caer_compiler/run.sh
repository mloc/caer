#!/bin/sh

set -ex

RUST_BACKTRACE=1 cargo run

llc-10 -o out/unopt.s out/unopt.bc
clang-10 -L ../target/debug/deps out/unopt.bc -Wl,-Tlink.ld -lcaer_runtime -lm -o out/unopt

llc-10 -o out/opt.s out/opt.bc
clang-10 -L ../target/debug/deps out/opt.bc -Wl,-Tlink.ld -lcaer_runtime -lm -o out/opt

LD_LIBRARY_PATH=../target/debug/deps lldb out/opt
