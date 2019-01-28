#!/bin/bash

set -euf -o pipefail

if [ "$#" -ne 1 ]; then
    echo "gib fn"
    exit 1
fi

mkdir -p dbgout/dot/llvm_unopt
mkdir -p dbgout/dot/llvm_opt

cd dbgout/dot/llvm_unopt
llvm-as-7 < ../../llvm/unopt.ll | opt-7 -analyze -dot-cfg
cd ../llvm_opt
llvm-as-7 < ../../llvm/opt.ll | opt-7 -analyze -dot-cfg
cd ..

xdot tino_cfg/cfg_${1}.dot &
sleep 0.5
xdot llvm_unopt/cfg.${1}.dot &
sleep 0.5
xdot llvm_opt/cfg.${1}.dot &
