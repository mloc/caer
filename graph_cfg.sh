#!/bin/bash

set -euf -o pipefail

mkdir -p dbgout/dot/llvm_unopt
mkdir -p dbgout/dot/llvm_opt

cd dbgout/dot/llvm_unopt
llvm-as-8 < ../../llvm/unopt.ll | opt-8 -analyze -dot-cfg
cd ../llvm_opt
llvm-as-8 < ../../llvm/opt.ll | opt-8 -analyze -dot-cfg
cd ..
