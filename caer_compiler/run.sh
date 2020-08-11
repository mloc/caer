#!/bin/sh

set -ex

RUST_BACKTRACE=1 cargo run
#opt-10 --rewrite-statepoints-for-gc -o out/unopt.sp.bc out/unopt.bc
cp out/unopt.bc out/unopt.sp.bc
#llvm-dis out/unopt.sp.bc -o out/unopt.sp.ll
llc-10 -o out/unopt.s out/unopt.sp.bc
clang-10 -L ../target/debug/deps out/unopt.sp.bc -Wl,-Tlink.ld -lcaer_runtime -lm -o out/unopt

#opt-10 --rewrite-statepoints-for-gc -o out/opt.sp.bc out/opt.bc
cp out/opt.bc out/opt.sp.bc
#llvm-dis out/opt.sp.bc -o out/opt.sp.ll
llc-10 -o out/opt.s out/opt.sp.bc
clang-10 -L ../target/debug/deps out/opt.sp.bc -Wl,-Tlink.ld -lcaer_runtime -lm -o out/opt
LD_LIBRARY_PATH=../target/debug/deps lldb out/opt
