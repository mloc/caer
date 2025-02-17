#!/usr/bin/env bash

set -euf -o pipefail

mkdir -p dbgout/dot/llvm_unopt
mkdir -p dbgout/dot/llvm_opt

cd dbgout/dot/llvm_unopt
llvm-as < ../../../out/unopt.ll | opt -analyze -dot-cfg
cd ../llvm_opt
llvm-as < ../../../out/opt.ll | opt -analyze -dot-cfg
cd ..
